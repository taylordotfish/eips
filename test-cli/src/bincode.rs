/*
 * Copyright (C) [unpublished] taylor.fish <contact@taylor.fish>
 *
 * This file is part of Eips.
 *
 * Eips is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Eips is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
 * Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with Eips. If not, see <https://www.gnu.org/licenses/>.
 */

pub use bincode::error::{DecodeError, EncodeError};
use std::fmt;
use std::io;

const CONFIG: bincode::config::Configuration = bincode::config::standard();

pub fn serialize<T, W>(value: &T, writer: &mut W) -> Result<(), EncodeError>
where
    T: serde::Serialize,
    W: io::Write,
{
    bincode::serde::encode_into_std_write(value, writer, CONFIG).map(|_| ())
}

pub fn deserialize<T, R>(reader: &mut R) -> Result<T, DecodeError>
where
    T: serde::de::DeserializeOwned,
    R: io::Read,
{
    bincode::serde::decode_from_std_read(reader, CONFIG)
}

pub trait ErrorExt {
    fn io_error(&self) -> Option<&io::Error>;

    fn io_error_kind(&self) -> Option<io::ErrorKind> {
        self.io_error().map(|e| e.kind())
    }
}

impl ErrorExt for EncodeError {
    fn io_error(&self) -> Option<&io::Error> {
        if let Self::Io {
            inner,
            ..
        } = self
        {
            Some(inner)
        } else {
            None
        }
    }
}

impl ErrorExt for DecodeError {
    fn io_error(&self) -> Option<&io::Error> {
        if let Self::Io {
            inner,
            ..
        } = self
        {
            Some(inner)
        } else {
            None
        }
    }
}

pub struct ErrorDisplay<'a, E>(pub &'a E);

impl<E> fmt::Display for ErrorDisplay<'_, E>
where
    E: ErrorExt + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(e) = self.0.io_error() {
            write!(f, "{e}")
        } else {
            write!(f, "serialization error: {}", self.0)
        }
    }
}
