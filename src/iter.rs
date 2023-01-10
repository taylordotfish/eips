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

//! Iterators.

use super::node::{Node, StaticNode, Visibility};
use super::options::{Bool, EipsOptions, NodeAllocOptions};
use super::{Eips, Id, RemoteChange};
use core::marker::PhantomData;
use fixed_typed_arena::iter::{Iter as ArenaIter, Position as ArenaPosition};

pub struct Iter<'a, Id, Opt>
where
    Opt: EipsOptions,
{
    pub(super) nodes: ArenaIter<'a, Node<Id, Opt>, NodeAllocOptions<Id, Opt>>,
    pub(super) eips: &'a Eips<Id, Opt>,
}

impl<Id, Opt> Iter<'_, Id, Opt>
where
    Opt: EipsOptions<ResumableIter = Bool<true>>,
{
    pub fn pause(self) -> PausedIter<Id, Opt> {
        PausedIter {
            position: self.nodes.as_position(),
            phantom: PhantomData,
        }
    }
}

impl<Id, Opt> Iterator for Iter<'_, Id, Opt>
where
    Id: self::Id,
    Opt: EipsOptions,
{
    type Item = (RemoteChange<Id>, Option<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        let node = unsafe { StaticNode::new(self.nodes.next()?.into()) };
        let index = (node.visibility() == Visibility::Visible)
            .then(|| self.eips.index(node));
        Some((node.to_change(), index))
    }
}

impl<Id, Opt> Clone for Iter<'_, Id, Opt>
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

pub struct PausedIter<Id, Opt> {
    position: ArenaPosition,
    phantom: PhantomData<fn() -> (Id, Opt)>,
}

impl<Id, Opt> PausedIter<Id, Opt>
where
    Opt: EipsOptions<ResumableIter = Bool<true>>,
{
    pub fn resume(self, eips: &Eips<Id, Opt>) -> Iter<'_, Id, Opt> {
        Iter {
            nodes: eips.node_alloc.iter_at(&self.position),
            eips,
        }
    }
}

impl<Id, Opt> Clone for PausedIter<Id, Opt>
where
    Opt: EipsOptions<ResumableIter = Bool<true>>,
{
    fn clone(&self) -> Self {
        Self {
            position: self.position.clone(),
            phantom: PhantomData,
        }
    }
}
