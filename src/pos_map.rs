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

use super::node::{Node, StaticNode, Visibility};
use super::options::{self, EipsOptions};
use core::fmt;
use core::marker::PhantomData;
use skippy::{LeafNext, LeafRef, This};
use tagged_pointer::TaggedPtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PosMapNodeKind {
    Normal = 0,
    Marker = 1,
}

impl PosMapNodeKind {
    pub const VARIANTS: [Self; 2] = [Self::Normal, Self::Marker];
}

pub struct PosMapNext<Id, Opt>(
    TaggedPtr<Node<Id, Opt>, 2>,
    PhantomData<StaticNode<Id, Opt>>,
);

impl<Id, Opt> PosMapNext<Id, Opt>
where
    Opt: EipsOptions,
{
    pub fn new(next: LeafNext<PosMapNode<Id, Opt>>) -> Self {
        let (ptr, tag) = match next {
            LeafNext::Data(data) => {
                (data.cast(), PosMapNodeKind::VARIANTS.len())
            }
            LeafNext::Leaf(leaf) => (leaf.node().ptr(), leaf.kind() as usize),
        };
        Self(TaggedPtr::new(ptr, tag), PhantomData)
    }

    pub fn get(&self) -> LeafNext<PosMapNode<Id, Opt>> {
        let (ptr, tag) = self.0.get();
        if tag == PosMapNodeKind::VARIANTS.len() {
            LeafNext::Data(ptr.cast())
        } else {
            LeafNext::Leaf(PosMapNode::new(
                unsafe { StaticNode::new(ptr) },
                PosMapNodeKind::VARIANTS[tag],
            ))
        }
    }
}

impl<Id, Opt> Clone for PosMapNext<Id, Opt> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Id, Opt> Copy for PosMapNext<Id, Opt> {}

impl<Id, Opt> fmt::Debug for PosMapNext<Id, Opt>
where
    Id: fmt::Debug,
    Opt: EipsOptions,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("PosMapNext").field(&self.get()).finish()
    }
}

/// Required by [`Node::pos_map_next`] for safety.
pub struct Token(());

pub struct PosMapNode<Id, Opt>(
    TaggedPtr<Node<Id, Opt>, 1>,
    PhantomData<StaticNode<Id, Opt>>,
);

impl<Id, Opt> PosMapNode<Id, Opt> {
    pub fn new(node: StaticNode<Id, Opt>, kind: PosMapNodeKind) -> Self {
        Self(TaggedPtr::new(node.ptr(), kind as usize), PhantomData)
    }

    pub fn kind(&self) -> PosMapNodeKind {
        PosMapNodeKind::VARIANTS[self.0.tag()]
    }

    pub fn node(&self) -> StaticNode<Id, Opt> {
        unsafe { StaticNode::new(self.0.ptr()) }
    }

    #[allow(dead_code)]
    pub fn as_node(&self) -> &Node<Id, Opt> {
        unsafe { self.node().ptr().as_ref() }
    }
}

unsafe impl<Id, Opt> LeafRef for PosMapNode<Id, Opt>
where
    Opt: EipsOptions,
{
    type Options = options::PosMapOptions<Id, Opt>;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().pos_map_next(Token(()))[self.kind() as usize]
            .get()
            .map(|n| n.get())
    }

    fn set_next(this: This<&'_ Self>, next: Option<LeafNext<Self>>) {
        this.node().pos_map_next(Token(()))[this.kind() as usize]
            .set(next.map(PosMapNext::new));
    }

    fn size(&self) -> usize {
        (self.kind() == PosMapNodeKind::Normal
            && self.node().visibility() == Visibility::Visible)
            as usize
    }
}

impl<Id, Opt> Clone for PosMapNode<Id, Opt> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Id, Opt> Copy for PosMapNode<Id, Opt> {}

impl<Id, Opt> PartialEq for PosMapNode<Id, Opt> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Id, Opt> Eq for PosMapNode<Id, Opt> {}

impl<Id, Opt> fmt::Debug for PosMapNode<Id, Opt>
where
    Id: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("PosMapNode")
            .field("kind", &self.kind())
            .field("node", &self.node())
            .finish()
    }
}
