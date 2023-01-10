/*
 * Copyright (C) [unpublished] taylor.fish <contact@taylor.fish>
 *
 * This file is part of Eips.
 *
 * Eips is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Eips is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Eips. If not, see <https://www.gnu.org/licenses/>.
 */

//! Error types.

use core::fmt::{self, Debug, Display};

#[non_exhaustive]
#[derive(Clone, Copy, Debug)]
pub enum ChangeError<Id> {
    BadParentId(Id),
    BadOldLocation(Id),
    OldLocationIsMove(Id),
    HiddenMove(Id),
    MergeConflict(Id),
}

impl<Id: Display> Display for ChangeError<Id> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadParentId(id) => write!(fmt, "bad parent id: {id}"),
            Self::BadOldLocation(id) => write!(fmt, "bad old location: {id}"),
            Self::OldLocationIsMove(id) => {
                write!(fmt, "old location is a move destination: {id}")
            }
            Self::HiddenMove(id) => {
                write!(fmt, "change is a move destination but is hidden: {id}")
            }
            Self::MergeConflict(id) => {
                write!(fmt, "conflict between change and existing data: {id}")
            }
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "doc_cfg", doc(cfg(feature = "std")))]
impl<Id: Debug + Display> std::error::Error for ChangeError<Id> {}

#[non_exhaustive]
#[derive(Clone, Copy, Debug)]
pub struct IndexError {
    pub index: usize,
}

impl Display for IndexError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "bad index: {}", self.index)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "doc_cfg", doc(cfg(feature = "std")))]
impl std::error::Error for IndexError {}

#[non_exhaustive]
#[derive(Clone, Copy, Debug)]
pub struct IdError<Id> {
    pub id: Id,
}

impl<Id: Display> Display for IdError<Id> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "bad id: {}", self.id)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "doc_cfg", doc(cfg(feature = "std")))]
impl<Id: Debug + Display> std::error::Error for IdError<Id> {}
