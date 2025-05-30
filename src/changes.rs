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

//! Types representing remote and local changes.

#[cfg(doc)]
use super::Eips;
use super::node::MoveTimestamp;
pub use super::node::{Direction, Visibility};
use super::sibling_set::SiblingSetKey;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Represents a change to be made to a client's local sequence of items.
///
/// This type is returned by [`Eips::apply_change`]. The local change should
/// be made to the local sequence of items corresponding to the [`Eips`]
/// sequence.
#[derive(Clone, Copy, Debug)]
pub enum LocalChange {
    /// The remote change was already applied earlier.
    ///
    /// In this case, the remote change should *not* be propagated to other
    /// clients.
    AlreadyApplied,

    /// The remote change was applied but did not result in any local changes.
    ///
    /// In this case, the remote change should still be propagated to other
    /// clients.
    None,

    /// An item should be inserted at the given index in the local sequence of
    /// items.
    Insert(usize),

    /// The item at the given index should be removed from the local sequence
    /// of items.
    Remove(usize),

    /// The item at index `old` should be moved to index `new` in the local
    /// sequence of items.
    ///
    /// The index of the item should be `new` once the move is complete.
    Move {
        /// The old local index.
        old: usize,
        /// The new local index.
        new: usize,
    },
}

/// Represents a change to be made to an [`Eips`] sequence.
///
/// This type is returned by methods like [`Eips::insert`] and [`Eips::remove`]
/// and can be turned into a local change with [`Eips::apply_change`].
///
/// The change is indented to be applied both to the client from which it was
/// generated, as well as by other clients connected via a network.
///
/// This type's fields are made public to ease custom serialization, but
/// usually do not need to be accessed directly. When the crate feature `serde`
/// is enabled, this type will implement [`Serialize`] and [`Deserialize`].
#[doc = "\n"]
#[cfg_attr(
    not(feature = "serde"),
    doc = "
[`Serialize`]: https://docs.rs/serde/1.0/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/1.0/serde/trait.Deserialize.html
"
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct RemoteChange<Id> {
    /// The change's ID.
    pub id: Id,
    /// The ID of the change's parent.
    pub parent: Option<Id>,
    /// The change's direction; see [`Direction`].
    pub direction: Direction,
    /// The change's visibility; see [`Visibility`].
    pub visibility: Visibility,
    /// An internal timestamp used to track moved items.
    pub move_timestamp: MoveTimestamp,
    /// An internal ID used to track moved items.
    pub old_location: Option<Id>,
}

impl<Id: Clone> RemoteChange<Id> {
    pub(crate) fn key(&self) -> SiblingSetKey<&Id> {
        SiblingSetKey::Normal {
            parent: self.parent.as_ref(),
            direction: self.direction,
            child: &self.id,
        }
    }
}
