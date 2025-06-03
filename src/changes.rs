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

pub use crate::node::{Direction, Visibility};
use crate::sibling_set::SiblingSetKey;
#[cfg(doc)]
use crate::{Eips, EipsOptions};
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
///
#[cfg_attr(
    not(feature = "serde"),
    doc = "
[`Serialize`]: https://docs.rs/serde/1/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/1/serde/trait.Deserialize.html
"
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct RemoteChange<Id> {
    /// The change's ID.
    pub id: Id,

    /// The ID of the change's parent. If the change has no parent, this is
    /// equal to [`self.id`](Self::id) (see [`Self::parent`]).
    pub raw_parent: Id,

    /// The change's direction; see [`Direction`].
    pub direction: Direction,

    /// The change's visibility; see [`Visibility`].
    pub visibility: Visibility,

    /// Additional information for items that have been moved.
    /// If move operations aren't supported ([`EipsOptions::SupportsMove`] is
    /// false), this will always be [`None`].
    pub move_info: Option<MoveInfo<Id>>,
}

impl<Id: PartialEq> RemoteChange<Id> {
    /// The ID of the change's parent.
    ///
    /// [`self.raw_parent`] encodes this data in a slightly different way:
    /// [`None`] is represented by [`self.raw_parent`] having the same value as
    /// `self.id`. This avoids using an <code>[Option]\<Id></code>, which may
    /// take up more space.
    ///
    /// [`self.raw_parent`]: Self::raw_parent
    pub fn parent(&self) -> Option<&Id> {
        if self.id == self.raw_parent {
            None
        } else {
            Some(&self.raw_parent)
        }
    }

    pub(crate) fn key(&self) -> SiblingSetKey<&Id> {
        SiblingSetKey::Normal {
            parent: self.parent(),
            direction: self.direction,
            child: &self.id,
        }
    }
}

/// Additional information for items that have been moved.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Copy, Clone, Debug)]
pub struct MoveInfo<Id> {
    /// An internal timestamp used to track moved items.
    pub timestamp: crate::NonZeroMoveTimestamp,
    /// An internal ID used to track moved items.
    pub old_location: Id,
}
