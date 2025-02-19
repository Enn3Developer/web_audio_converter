#[macro_use]
extern crate rocket;

use base64::prelude::BASE64_STANDARD;
use base64::Engine;
use rocket::http::ContentType;
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::{tokio, Request, Response};
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

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct Data {
    audio: String,
}

#[derive(Debug, Deserialize, Serialize, Responder)]
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

impl<'r, 'o: 'r> Responder<'r, 'o> for AudioError {
    fn respond_to(self, _request: &'r Request<'_>) -> rocket::response::Result<'o> {
        let mut response = Response::new();
        response.adjoin_header(ContentType::new("text", "plain"));
        let string = self.to_string();
        response.set_sized_body(string.len(), Cursor::new(string));
        Ok(response)
    }
}

fn use_display<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Display,
    S: Serializer,
{
    serializer.collect_str(value)
}

pub async fn decode(data: Vec<u8>) -> Result<Vec<i8>, AudioError> {
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
                let spec = *decoded_buffer.spec();
                let mut buffer: SampleBuffer<i8> =
                    SampleBuffer::new(decoded_buffer.capacity() as u64, spec);
                buffer.copy_planar_ref(decoded_buffer);
                decoded.reserve(buffer.len());
                let mut samples = buffer.len();
                if spec.channels.count() > 1 {
                    samples /= spec.channels.count();
                }
                for sample in 0..samples {
                    decoded.push(buffer.samples()[sample]);
                }
            }
            Err(symphonia::core::errors::Error::DecodeError(err)) => warn!("decode error: {}", err),
            Err(error) => {
                if let symphonia::core::errors::Error::IoError(io_error) = &error {
                    if io_error.kind() == io::ErrorKind::UnexpectedEof {
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

pub async fn convert_audio(data: Vec<i8>) -> Vec<u8> {
    let mut charge: i32 = 0;
    let mut strength: i32 = 1;
    let mut previous_bit = false;
    let strength_increase = 7;
    let strength_decrease = 20;
    let len = data.len() / 8;
    let mut out = Vec::with_capacity(data.len() / 8);

    for i in 0..len {
        // yield occasionally to not starve other tasks
        tokio::task::yield_now().await;

        let mut byte = 0i8;
        for j in 0..8 {
            let level = data[i * 8 + j] as i32;
            let bit = level > charge || (level == charge && charge == 127);
            let target: i8 = if bit { 127 } else { -128 };
            let target_32 = target as i32;

            // charge adjustment - begin
            // TODO: magic numbers
            let mut next_charge = charge + (strength * (target_32 - charge) + 128) / 256;
            if charge == next_charge {
                if target_32 < charge {
                    next_charge -= 1;
                } else if target_32 > charge {
                    next_charge += 1;
                }
                charge = next_charge;
            }
            // charge adjustment - end

            // strength adjustment - begin
            let (r, z) = if bit == previous_bit {
                (strength_increase, 255)
            } else {
                (strength_decrease, 0)
            };
            // TODO: magic numbers
            let mut next_strength = strength + (r * (z - strength) + 128) / 256;
            if strength == next_strength {
                if z < strength {
                    next_strength -= 1;
                } else if z > strength {
                    next_strength += 1;
                }
                strength = next_strength;
            }
            // strength adjustment - end

            byte = if bit { -(byte >> 1) } else { byte >> 1 };
            previous_bit = bit;
        }
        out.push((byte as i16 + 128) as u8);
    }

    out
}

#[get("/convert/<name>")]
async fn convert_file(name: &str) -> Result<Vec<u8>, ErrResult> {
    let file = tokio::fs::read(name).await.unwrap();
    match decode(file).await.or_else(|error| Err(ErrResult { error })) {
        Ok(audio) => Ok(convert_audio(audio).await),
        Err(res) => Err(res),
    }
}

#[post("/convert", data = "<data>")]
async fn convert(data: Json<Data>) -> Result<Vec<u8>, ErrResult> {
    match decode(BASE64_STANDARD.decode(&data.audio).unwrap())
        .await
        .or_else(|error| Err(ErrResult { error }))
    {
        Ok(audio) => Ok(convert_audio(audio).await),
        Err(res) => Err(res),
    }
}

#[get("/")]
async fn index() -> String {
    String::from("Index")
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index, convert, convert_file])
}
