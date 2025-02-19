#[macro_use]
extern crate rocket;

use base64::prelude::BASE64_STANDARD;
use base64::Engine;
use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::tokio;
use serde::Serializer;
use std::fmt::Display;
use std::io;
use std::io::Cursor;
use symphonia::core::audio::SampleBuffer;
use symphonia::core::codecs::DecoderOptions;
use symphonia::core::formats::FormatOptions;
use symphonia::core::io::MediaSourceStream;
use symphonia::core::meta::MetadataOptions;
use symphonia::core::probe::Hint;
use symphonia::default::get_probe;
use thiserror::Error;

const PREC: u8 = 10;

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct Data {
    audio: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum ConversionResult<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> From<Result<T, E>> for ConversionResult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(t) => ConversionResult::Ok(t),
            Err(e) => ConversionResult::Err(e),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct OkResult {
    audio: Vec<i8>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ErrResult {
    #[serde(serialize_with = "use_display")]
    error: AudioError,
}

#[derive(Error, Debug, Deserialize, Serialize)]
pub enum AudioError {
    #[error("unknown error")]
    Unknown,
    #[error("error happened during decoding; check your input file: {0}")]
    SymphoniaError(String),
    #[error("file doesn't contain any track")]
    NoTrack,
    #[error("error happened during decoding of base64: {0}")]
    Base64(String),
}

fn use_display<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Display,
    S: Serializer,
{
    serializer.collect_str(value)
}

pub async fn decode(data: Vec<u8>) -> Result<Vec<f32>, AudioError> {
    let source = Box::new(Cursor::new(data));
    let mut decoded = vec![];
    let mss = MediaSourceStream::new(source, Default::default());
    let format_opts = FormatOptions {
        enable_gapless: true,
        ..FormatOptions::default()
    };
    let metadata_opts = MetadataOptions::default();
    let probed = get_probe()
        .format(&Hint::default(), mss, &format_opts, &metadata_opts)
        .or_else(|error| Err(AudioError::SymphoniaError(error.to_string())))?;

    // yield occasionally to not starve other tasks
    tokio::task::yield_now().await;

    let decode_opts = DecoderOptions::default();
    let mut reader = probed.format;
    let track = reader.default_track().ok_or(AudioError::NoTrack)?;
    let track_id = track.id;

    let mut decoder = symphonia::default::get_codecs()
        .make(&track.codec_params, &decode_opts)
        .or_else(|error| Err(AudioError::SymphoniaError(error.to_string())))?;

    loop {
        // yield occasionally to not starve other tasks
        tokio::task::yield_now().await;

        let packet = match reader.next_packet() {
            Ok(packet) => packet,
            Err(error) => {
                if let symphonia::core::errors::Error::IoError(io_error) = &error {
                    if io_error.kind() == io::ErrorKind::UnexpectedEof {
                        warn!("decode error: {}", io_error);
                        break;
                    }
                }
                return Err(AudioError::SymphoniaError(error.to_string()));
            }
        };

        if packet.track_id() != track_id {
            continue;
        }

        match decoder.decode(&packet) {
            Ok(decoded_buffer) => {
                let mut buffer: SampleBuffer<f32> =
                    SampleBuffer::new(decoded_buffer.capacity() as u64, *decoded_buffer.spec());
                buffer.copy_interleaved_ref(decoded_buffer);
                decoded.reserve(buffer.len());
                for sample in buffer.samples() {
                    decoded.push(*sample);
                }
            }
            Err(symphonia::core::errors::Error::DecodeError(err)) => warn!("decode error: {}", err),
            Err(error) => {
                if let symphonia::core::errors::Error::IoError(io_error) = &error {
                    if io_error.kind() == io::ErrorKind::UnexpectedEof {
                        warn!("decode error: {}", io_error);
                        break;
                    }
                }
                return Err(AudioError::SymphoniaError(error.to_string()));
            }
        }
    }

    decoder.finalize();
    Ok(decoded)
}

pub async fn convert_audio(data: Vec<f32>) -> ConversionResult<OkResult, ErrResult> {
    let mut charge = 0.0;
    let mut strength = 0.0;
    let mut previous_bit = false;
    let len = data.len() / 8;
    let mut out: Vec<i8> = Vec::with_capacity(len);

    for i in 0..len {
        // yield occasionally to not starve other tasks
        tokio::task::yield_now().await;

        let mut byte = 0i8;
        for j in 0..8 {
            let level = data[i * 8 + j] * 127.0;
            let bit = level > charge || (level == charge && charge == 127.0);
            let target = if bit { 127.0 } else { -128.0 };
            let mut next_charge = charge
                + ((strength * (target - charge) + (1 << (PREC - 1)) as f32) as u64 >> PREC) as f32;
            if next_charge == charge && next_charge != target {
                next_charge += if bit { 1.0 } else { -1.0 };
            }
            let z = if bit == previous_bit {
                (1 << PREC) - 1
            } else {
                0
            } as f32;
            let mut next_strength = strength;
            if strength != z {
                next_strength += if bit { 1.0 } else { -1.0 };
            }
            if next_strength < (2 << (PREC - 8)) as f32 {
                next_strength = (2 << (PREC - 8)) as f32;
            }
            charge = next_charge;
            strength = next_strength;
            previous_bit = bit;
            byte = if bit {
                ((byte as i16 >> 1) + 128) as i8
            } else {
                byte >> 1
            };
        }
        out.push(byte);
    }

    ConversionResult::Ok(OkResult { audio: out })
}

#[get("/convert/<name>")]
async fn convert_file(name: &str) -> Json<ConversionResult<OkResult, ErrResult>> {
    let file = tokio::fs::read(name).await.unwrap();
    match decode(file).await.or_else(|error| Err(ErrResult { error })) {
        Ok(audio) => convert_audio(audio).await,
        Err(res) => ConversionResult::Err(res),
    }
    .into()
}

#[post("/convert", data = "<data>")]
async fn convert(data: Json<Data>) -> Json<ConversionResult<OkResult, ErrResult>> {
    match decode(BASE64_STANDARD.decode(&data.audio).unwrap())
        .await
        .or_else(|error| Err(ErrResult { error }))
    {
        Ok(audio) => convert_audio(audio).await,
        Err(res) => ConversionResult::Err(res),
    }
    .into()
}

#[get("/")]
async fn index() -> String {
    String::from("Index")
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index, convert, convert_file])
}
