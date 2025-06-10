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

#[cfg(doc)]
use crate::options::EipsOptions;
use core::fmt::{self, Debug, Display};

/// An error encountered while [applying a remote change][apply].
///
/// [apply]: crate::Eips::apply_change
#[non_exhaustive]
#[derive(Clone, Copy, Debug)]
pub enum ChangeError<Id> {
    /// The remote change's parent ID was invalid.
    BadParentId(Id),

    /// The remote change has no parent but its direction is
    /// [`Before`](crate::change::Direction::Before).
    BadDirection(Id),

    /// The remote change corresponds to an existing item, but there was a
    /// conflict between the old and new data.
    MergeConflict(Id),

    /// The remote change contains move information, but move operations are
    /// not supported.
    ///
    /// Remember that the value of [`EipsOptions::SupportsMove`] must be the
    /// same for all clients in a distributed system. Clients that support move
    /// operations cannot talk to clients that don't.
    UnsupportedMove(Id),

    /// The remote change's old location ID was invalid.
    BadOldLocation(Id),

    /// The remote change has no move information but refers to the destination
    /// of a move operation.
    UnexpectedMove(Id),

    /// The remote change's old location ID incorrectly corresponds to an
    /// the destination of a move operation.
    OldLocationIsMove(Id),

    /// The remote change represents an item to move but is incorrectly marked
    /// as hidden.
    HiddenMove(Id),

    /// The change's move timestamp was greater than [`usize::MAX`].
    ///
    /// This should not happen under normal circumstances. Eips requires causal
    /// delivery, which means if a client receives a change with a move
    /// timestamp greater than [`usize::MAX`], it must have already received at
    /// least [`usize::MAX`] prior move operations on this element, but that
    /// would use more than [`usize::MAX`] bytes of memory, which is not
    /// possible.
    TimestampOverflow {
        id: Id,
        timestamp: crate::MoveTimestamp,
    },
}

impl<Id> ChangeError<Id> {
    pub(crate) fn to_basic(&self) -> BasicChangeError {
        use BasicChangeError as Basic;
        match self {
            Self::BadParentId(_) => Basic::BadParentId,
            Self::BadDirection(_) => Basic::BadDirection,
            Self::MergeConflict(_) => Basic::MergeConflict,
            Self::UnsupportedMove(_) => Basic::UnsupportedMove,
            Self::BadOldLocation(_) => Basic::BadOldLocation,
            Self::UnexpectedMove(_) => Basic::UnexpectedMove,
            Self::OldLocationIsMove(_) => Basic::OldLocationIsMove,
            Self::HiddenMove(_) => Basic::HiddenMove,
            Self::TimestampOverflow {
                timestamp,
                ..
            } => Basic::TimestampOverflow {
                timestamp: *timestamp,
            },
        }
    }
}

impl<Id: Display> Display for ChangeError<Id> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let basic = self.to_basic();
        match self {
            Self::BadParentId(id) => write!(f, "{basic}: {id}"),
            Self::BadDirection(id) => write!(f, "{basic}: {id}"),
            Self::MergeConflict(id) => write!(f, "{basic}: {id}"),
            Self::UnsupportedMove(id) => write!(f, "{basic}: {id}"),
            Self::BadOldLocation(id) => write!(f, "{basic}: {id}"),
            Self::UnexpectedMove(id) => write!(f, "{basic}: {id}"),
            Self::OldLocationIsMove(id) => write!(f, "{basic}: {id}"),
            Self::HiddenMove(id) => write!(f, "{basic}: {id}"),
            Self::TimestampOverflow {
                id,
                ..
            } => write!(f, "{basic} (id {id})"),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(feature = "doc_cfg", doc(cfg(feature = "std")))]
impl<Id: Debug + Display> std::error::Error for ChangeError<Id> {}

/// An error encountered due to an invalid or out-of-bounds index.
#[non_exhaustive]
#[derive(Clone, Copy, Debug)]
pub struct IndexError {
    /// The invalid index.
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

/// An error encountered due to an invalid or missing ID.
#[non_exhaustive]
#[derive(Clone, Copy, Debug)]
pub struct IdError<Id> {
    /// The invalid ID.
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

#[derive(Clone, Copy, Debug)]
pub(crate) enum BasicChangeError {
    BadParentId,
    BadDirection,
    MergeConflict,
    UnsupportedMove,
    BadOldLocation,
    UnexpectedMove,
    OldLocationIsMove,
    HiddenMove,
    TimestampOverflow {
        timestamp: crate::MoveTimestamp,
    },
}

impl Display for BasicChangeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadParentId => write!(f, "bad parent id"),
            Self::BadDirection => {
                write!(f, "change has no parent but its direction is 'before'")
            }
            Self::MergeConflict => {
                write!(f, "conflict between change and existing data")
            }
            Self::UnsupportedMove => {
                write!(f, "change has move info but moves are unsupported")
            }
            Self::BadOldLocation => write!(f, "bad old location"),
            Self::UnexpectedMove => {
                write!(f, "change has no move info but is a move destination")
            }
            Self::OldLocationIsMove => {
                write!(f, "old location is a move destination")
            }
            Self::HiddenMove => {
                write!(f, "change is a move destination but is hidden")
            }
            Self::TimestampOverflow {
                timestamp,
            } => write!(f, "move timestamp is too large: {timestamp}"),
        }
    }
}
