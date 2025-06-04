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

pub use bincode::Error;
use bincode::Options;
use std::fmt;
use std::io;

fn options() -> impl Options {
    bincode::DefaultOptions::new()
}

pub fn serialize<T, W>(value: &T, writer: &mut W) -> Result<(), Error>
where
    T: serde::Serialize,
    W: io::Write,
{
    options().serialize_into(writer, value)
}

pub fn deserialize<T, R>(reader: &mut R) -> Result<T, Error>
where
    T: serde::de::DeserializeOwned,
    R: io::Read,
{
    options().deserialize_from(reader)
}

pub trait ErrorExt {
    fn display(&self) -> ErrorDisplay<'_>;
    fn io_error(&self) -> Option<&io::Error>;

    fn io_error_kind(&self) -> Option<io::ErrorKind> {
        self.io_error().map(|e| e.kind())
    }
}

impl ErrorExt for bincode::ErrorKind {
    fn display(&self) -> ErrorDisplay<'_> {
        ErrorDisplay(self)
    }

    fn io_error(&self) -> Option<&io::Error> {
        if let Self::Io(inner) = self {
            Some(inner)
        } else {
            None
        }
    }
}

pub struct ErrorDisplay<'a>(&'a bincode::ErrorKind);

impl fmt::Display for ErrorDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(e) = self.0.io_error() {
            write!(f, "{e}")
        } else {
            write!(f, "serialization error: {}", self.0)
        }
    }
}
