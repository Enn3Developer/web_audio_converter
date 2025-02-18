#[macro_use]
extern crate rocket;

use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::tokio;
use serde::Serializer;
use std::fmt::Display;
use std::io::Cursor;
use symphonia::core::audio::SampleBuffer;
use symphonia::core::codecs::DecoderOptions;
use symphonia::core::formats::FormatOptions;
use symphonia::core::io::MediaSourceStream;
use symphonia::core::meta::MetadataOptions;
use symphonia::core::probe::Hint;
use symphonia::default::get_probe;
use thiserror::Error;

const PREC: u8 = 3;

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct Data {
    audio: Vec<u8>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum ConversionResult<T, E> {
    Ok(T),
    Err(E),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct OkResult {
    audio: Vec<u8>,
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
}

fn use_display<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Display,
    S: Serializer,
{
    serializer.collect_str(value)
}

pub async fn decode(data: Vec<u8>) -> Result<Vec<u8>, AudioError> {
    let source = Box::new(Cursor::new(data));
    let mut decoded = vec![];
    let mss = MediaSourceStream::new(source, Default::default());
    let format_opts = FormatOptions::default();
    let metadata_opts = MetadataOptions::default();
    let probed = get_probe()
        .format(
            &Hint::default().with_extension("mp3"),
            mss,
            &format_opts,
            &metadata_opts,
        )
        .or_else(|error| Err(AudioError::SymphoniaError(error.to_string())))?;

    // yield occasionally to not starve other tasks
    tokio::task::yield_now().await;

    let decode_opts = DecoderOptions::default();
    let mut reader = probed.format;
    let track = reader.default_track().unwrap();
    let track_id = track.id;

    let mut decoder = symphonia::default::get_codecs()
        .make(&track.codec_params, &decode_opts)
        .or_else(|error| Err(AudioError::SymphoniaError(error.to_string())))?;

    loop {
        // yield occasionally to not starve other tasks
        tokio::task::yield_now().await;

        let packet = match reader.next_packet() {
            Ok(packet) => packet,
            Err(error) => break Err(AudioError::SymphoniaError(error.to_string())),
        };

        if packet.track_id() != track_id {
            continue;
        }

        match decoder.decode(&packet) {
            Ok(decoded_buffer) => {
                let mut buffer: SampleBuffer<u8> =
                    SampleBuffer::new(decoded_buffer.capacity() as u64, *decoded_buffer.spec());
                buffer.copy_interleaved_ref(decoded_buffer);
                decoded.reserve(buffer.len());
                for sample in buffer.samples() {
                    decoded.push(*sample);
                }
            }
            Err(symphonia::core::errors::Error::DecodeError(err)) => warn!("decode error: {}", err),
            Err(error) => {
                break Err(AudioError::SymphoniaError(error.to_string()));
            }
        }
    }?; // return if encountered any error

    decoder.finalize();
    Ok(decoded)
}

pub async fn convert_audio(data: Vec<u8>) -> ConversionResult<OkResult, ErrResult> {
    let mut charge = 0;
    let mut strength = 0;
    let mut previous_bit = false;
    let mut out: Vec<u8> = Vec::with_capacity(data.len() / 8);

    for i in 0..data.len() {
        // yield occasionally to not starve other tasks
        tokio::task::yield_now().await;

        let mut byte = 0u8;
        for j in 0..8 {
            let level = data[i * 8 + j] * 127;
            let bit = level > charge || (level == charge && charge == 127);
            let target = if bit { 127 } else { 255 };
            let mut next_charge =
                charge + ((strength * (target - charge) + (1 << (PREC - 1))) >> PREC);
            if next_charge == charge && next_charge != target {
                if bit {
                    next_charge += 1;
                } else {
                    next_charge -= 1;
                }
            }
            let z = if bit == previous_bit {
                (1 << PREC) - 1
            } else {
                0
            };
            let mut next_strength = strength;
            if strength != z {
                if bit == previous_bit {
                    next_strength += 1;
                } else {
                    next_strength -= 1;
                }
            }
            if next_strength < 2 << (PREC - 2) {
                next_strength = 2 << (PREC - 2);
            }
            charge = next_charge;
            strength = next_strength;
            previous_bit = bit;
            byte = if bit { (byte >> 1) + 128 } else { byte >> 1 };
        }
        out.push(byte);
    }

    ConversionResult::Err(ErrResult {
        error: AudioError::Unknown,
    })
}

#[post("/convert", data = "<data>")]
async fn convert(data: Json<Data>) -> Json<ConversionResult<OkResult, ErrResult>> {
    match decode(data.audio.clone())
        .await
        .or_else(|error| Err(ErrResult { error }))
    {
        Ok(data) => convert_audio(data).await,
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
    rocket::build().mount("/", routes![index, convert])
}
