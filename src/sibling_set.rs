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

use crate::node::{Direction, Node, StaticNode};
use crate::options::{self, EipsOptions};
use core::cmp::Ordering;
use core::fmt;
use core::marker::PhantomData;
use skippy::{LeafNext, LeafRef, This};
use tagged_pointer::TaggedPtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiblingSetNodeKind {
    Normal = 0,
    Childless = 1,
}

impl SiblingSetNodeKind {
    pub const VARIANTS: [Self; 2] = [Self::Normal, Self::Childless];
}

pub struct SiblingSetNext<Id, Opt: EipsOptions>(
    TaggedPtr<Node<Id, Opt>, 2>,
    PhantomData<StaticNode<Id, Opt>>,
);

impl<Id, Opt> SiblingSetNext<Id, Opt>
where
    Opt: EipsOptions,
{
    pub fn new(next: LeafNext<SiblingSetNode<Id, Opt>>) -> Self {
        let (ptr, tag) = match next {
            LeafNext::Data(data) => {
                (data.cast(), SiblingSetNodeKind::VARIANTS.len())
            }
            LeafNext::Leaf(leaf) => (leaf.node().ptr(), leaf.kind() as usize),
        };
        Self(TaggedPtr::new(ptr, tag), PhantomData)
    }

    pub fn get(&self) -> LeafNext<SiblingSetNode<Id, Opt>> {
        let (ptr, tag) = self.0.get();
        if tag == SiblingSetNodeKind::VARIANTS.len() {
            LeafNext::Data(ptr.cast())
        } else {
            LeafNext::Leaf(SiblingSetNode::new(
                // SAFETY: The pointer always originates from a valid
                // `StaticNode` when the tag is not equal to
                // `SiblingSetNodeKind::VARIANTS.len()`.
                unsafe { StaticNode::new(ptr) },
                SiblingSetNodeKind::VARIANTS[tag],
            ))
        }
    }
}

impl<Id, Opt> Clone for SiblingSetNext<Id, Opt>
where
    Opt: EipsOptions,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<Id, Opt: EipsOptions> Copy for SiblingSetNext<Id, Opt> {}

impl<Id, Opt> fmt::Debug for SiblingSetNext<Id, Opt>
where
    Id: PartialEq + fmt::Debug,
    Opt: EipsOptions,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("SiblingSetNext").field(&self.get()).finish()
    }
}

/// Required by [`Node::sibling_set_next`] for safety.
pub struct Token(());

pub struct SiblingSetNode<Id, Opt: EipsOptions>(
    TaggedPtr<Node<Id, Opt>, 1>,
    PhantomData<StaticNode<Id, Opt>>,
);

impl<Id, Opt> SiblingSetNode<Id, Opt>
where
    Opt: EipsOptions,
{
    pub fn new(node: StaticNode<Id, Opt>, kind: SiblingSetNodeKind) -> Self {
        Self(TaggedPtr::new(node.ptr(), kind as usize), PhantomData)
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        SiblingSetNodeKind::VARIANTS[self.0.tag()]
    }

    pub fn node(&self) -> StaticNode<Id, Opt> {
        // SAFETY: The pointer always originates from a valid `StaticNode`.
        unsafe { StaticNode::new(self.0.ptr()) }
    }

    pub fn as_node(&self) -> &Node<Id, Opt> {
        // SAFETY: `StaticNode` is conceptually a reference to a `Node`, and
        // this type conceptually holds a `StaticNode`, so it is safe to
        // provide a reference to the `Node` with the same lifetime as `self`.
        unsafe { self.node().ptr().as_ref() }
    }

    pub fn key(&self) -> SiblingSetKey<&Id>
    where
        Id: PartialEq,
    {
        match self.kind() {
            SiblingSetNodeKind::Normal => SiblingSetKey::Normal {
                parent: self.as_node().parent(),
                direction: self.as_node().direction(),
                child: &self.as_node().id,
            },
            SiblingSetNodeKind::Childless => {
                SiblingSetKey::Childless(&self.as_node().id)
            }
        }
    }
}

// SAFETY: `Self` is not `Send` or `Sync` due to the `TaggedPtr`. Both
// `sibling_set_next` pointers in `Node` are initially `None`, and the only
// function that can set them is [`LeafRef::set_next`], which does so with
// normal variable semantics. Clones of `self` will behave the same because
// they will point to the same underlying `Node`.
unsafe impl<Id, Opt> LeafRef for SiblingSetNode<Id, Opt>
where
    Opt: EipsOptions,
{
    type Options = options::SiblingSetOptions<Id, Opt>;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().sibling_set_next(Token(()))[self.kind() as usize]
            .get()
            .map(|n| n.get())
    }

    fn set_next(this: This<&'_ Self>, next: Option<LeafNext<Self>>) {
        this.node().sibling_set_next(Token(()))[this.kind() as usize]
            .set(next.map(SiblingSetNext::new))
    }
}

impl<Id, Opt> Clone for SiblingSetNode<Id, Opt>
where
    Opt: EipsOptions,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<Id, Opt: EipsOptions> Copy for SiblingSetNode<Id, Opt> {}

impl<Id, Opt> PartialEq for SiblingSetNode<Id, Opt>
where
    Opt: EipsOptions,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Id, Opt: EipsOptions> Eq for SiblingSetNode<Id, Opt> {}

impl<Id: Ord, Opt> PartialOrd for SiblingSetNode<Id, Opt>
where
    Opt: EipsOptions,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Id: Ord, Opt> Ord for SiblingSetNode<Id, Opt>
where
    Opt: EipsOptions,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.key().cmp(&other.key())
    }
}

impl<Id, Opt> fmt::Debug for SiblingSetNode<Id, Opt>
where
    Id: PartialEq + fmt::Debug,
    Opt: EipsOptions,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("SiblingSetNode")
            .field("kind", &self.kind())
            .field("node", &self.node())
            .finish()
    }
}

#[derive(Clone, Copy)]
pub enum SiblingSetKey<Id> {
    Normal {
        parent: Option<Id>,
        direction: Direction,
        child: Id,
    },
    Childless(Id),
}

impl<Id: Ord> PartialEq for SiblingSetKey<Id> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl<Id: Ord> Eq for SiblingSetKey<Id> {}

impl<Id: Ord> PartialOrd for SiblingSetKey<Id> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Id: Ord> Ord for SiblingSetKey<Id> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                Self::Normal {
                    parent: parent1,
                    direction: dir1,
                    child: child1,
                },
                Self::Normal {
                    parent: parent2,
                    direction: dir2,
                    child: child2,
                },
            ) => (parent1, dir1, child1).cmp(&(parent2, dir2, child2)),

            (
                Self::Normal {
                    parent,
                    direction,
                    ..
                },
                Self::Childless(id),
            ) => {
                let ord = parent.as_ref().cmp(&Some(id));
                if ord.is_ne() {
                    return ord;
                }
                match direction {
                    Direction::Before => Ordering::Less,
                    Direction::After => Ordering::Greater,
                }
            }

            (
                Self::Childless(id),
                Self::Normal {
                    parent,
                    direction,
                    ..
                },
            ) => {
                let ord = Some(id).cmp(&parent.as_ref());
                if ord.is_ne() {
                    return ord;
                }
                match direction {
                    Direction::Before => Ordering::Greater,
                    Direction::After => Ordering::Less,
                }
            }

            (Self::Childless(id1), Self::Childless(id2)) => id1.cmp(id2),
        }
    }
}

impl<Id, Opt> PartialEq<SiblingSetKey<&Id>> for SiblingSetNode<Id, Opt>
where
    Id: Ord,
    Opt: EipsOptions,
{
    fn eq(&self, other: &SiblingSetKey<&Id>) -> bool {
        self.key() == *other
    }
}

impl<Id, Opt> PartialEq<SiblingSetNode<Id, Opt>> for SiblingSetKey<&Id>
where
    Id: Ord,
    Opt: EipsOptions,
{
    fn eq(&self, other: &SiblingSetNode<Id, Opt>) -> bool {
        *self == other.key()
    }
}

impl<Id, Opt> PartialOrd<SiblingSetKey<&Id>> for SiblingSetNode<Id, Opt>
where
    Id: Ord,
    Opt: EipsOptions,
{
    fn partial_cmp(&self, other: &SiblingSetKey<&Id>) -> Option<Ordering> {
        Some(self.key().cmp(other))
    }
}

impl<Id, Opt> PartialOrd<SiblingSetNode<Id, Opt>> for SiblingSetKey<&Id>
where
    Id: Ord,
    Opt: EipsOptions,
{
    fn partial_cmp(
        &self,
        other: &SiblingSetNode<Id, Opt>,
    ) -> Option<Ordering> {
        Some(self.cmp(&other.key()))
    }
}
