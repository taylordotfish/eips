/*
 * Copyright (C) 2025 taylor.fish <contact@taylor.fish>
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

//! Iterators.

use crate::change::RemoteChange;
use crate::node::{Node, StaticNode};
use crate::options::{EipsOptions, NodeAllocOptions};
use crate::{Eips, Id};
use core::marker::PhantomData;
use fixed_typed_arena::iter::{Iter as ArenaIter, Position as ArenaPosition};

/// An iterator over the [`RemoteChange`]s in an [`Eips`] sequence.
///
/// See [`Eips::changes`].
pub struct Changes<'a, Id, Opt>
where
    Opt: EipsOptions,
{
    pub(crate) nodes: ArenaIter<'a, Node<Id, Opt>, NodeAllocOptions<Id, Opt>>,
    pub(crate) eips: &'a Eips<Id, Opt>,
}

impl<Id, Opt> Changes<'_, Id, Opt>
where
    Opt: EipsOptions,
{
    /// Pauses this iterator so that it no longer borrows the corresponding
    /// [`Eips`].
    ///
    /// The paused iterator can be turned back into a [`Changes`] iterator
    /// with [`PausedChanges::resume`].
    pub fn pause(self) -> PausedChanges<Id, Opt> {
        PausedChanges {
            position: self.nodes.as_position(),
            _phantom: PhantomData,
        }
    }
}

impl<Id, Opt> Iterator for Changes<'_, Id, Opt>
where
    Id: self::Id,
    Opt: EipsOptions,
{
    type Item = (RemoteChange<Id>, Option<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        self.nodes.next().map(|n| {
            // SAFETY: The pointer is valid because it comes from a reference.
            // It will remain valid for the life of the `StaticNode` because
            // we don't drop the arena allocator. No mutable references to the
            // node exist (the arena doesn't support them).
            let node = unsafe { StaticNode::new(n.into()) };
            self.eips.node_to_indexed_change(node)
        })
    }
}

impl<Id, Opt> Clone for Changes<'_, Id, Opt>
where
    Opt: EipsOptions,
{
    fn clone(&self) -> Self {
        Self {
            nodes: self.nodes.clone(),
            eips: self.eips,
        }
    }
}

/// A paused version of [`Changes`].
///
/// Unlike [`Changes`], this type does not borrow the corresponding [`Eips`].
pub struct PausedChanges<Id, Opt> {
    position: ArenaPosition,
    _phantom: PhantomData<fn() -> (Id, Opt)>,
}

impl<Id, Opt> PausedChanges<Id, Opt>
where
    Opt: EipsOptions,
{
    /// Turns this paused iterator back into an active [`Changes`] iterator.
    ///
    /// # Panics
    ///
    /// If `eips` is not the same as the [`Eips`] from which the original
    /// [`Changes`] iterator was created, this method may panic.
    pub fn resume(self, eips: &Eips<Id, Opt>) -> Changes<'_, Id, Opt> {
        Changes {
            nodes: eips.node_alloc.iter_at(&self.position),
            eips,
        }
    }
}

impl<Id, Opt> Clone for PausedChanges<Id, Opt>
where
    Opt: EipsOptions,
{
    fn clone(&self) -> Self {
        Self {
            position: self.position.clone(),
            _phantom: PhantomData,
        }
    }
}
