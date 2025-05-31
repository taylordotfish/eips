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

use super::RemoteChange;
use super::pos_map::{self, PosMapNext};
use super::sibling_set::{self, SiblingSetNext};
use cell_ref::Cell;
use core::fmt;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ptr::NonNull;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use tagged_pointer::TaggedPtr;

pub type MoveTimestamp = usize;

#[repr(align(4))]
pub struct Node<Id, Opt> {
    pub id: Id,
    pub parent: Option<Id>,
    pub move_timestamp: Cell<MoveTimestamp>,
    pub packed: Cell<Packed<Id, Opt>>,
    pos_map_next: [Cell<Option<PosMapNext<Id, Opt>>>; 2],
    sibling_set_next: [Cell<Option<SiblingSetNext<Id, Opt>>>; 2],
    phantom: PhantomData<Opt>,
}

impl<Id, Opt> Node<Id, Opt> {
    fn sentinel() -> NonNull<Self> {
        #[repr(align(4))]
        struct Align4(#[allow(dead_code)] u32);

        static SENTINEL: Align4 = Align4(0);
        NonNull::from(&SENTINEL).cast()
    }

    pub fn new(id: Id, parent: Option<Id>) -> Self {
        Self {
            id,
            parent,
            move_timestamp: Cell::default(),
            packed: Cell::default(),
            pos_map_next: Default::default(),
            sibling_set_next: Default::default(),
            phantom: PhantomData,
        }
    }

    pub fn pos_map_next(
        &self,
        _: pos_map::Token,
    ) -> &[Cell<Option<PosMapNext<Id, Opt>>>; 2] {
        &self.pos_map_next
    }

    pub fn sibling_set_next(
        &self,
        _: sibling_set::Token,
    ) -> &[Cell<Option<SiblingSetNext<Id, Opt>>>; 2] {
        &self.sibling_set_next
    }

    pub fn visibility(&self) -> Visibility {
        self.packed.with(Packed::visibility)
    }

    pub fn set_visibility(&self, vis: Visibility) {
        self.packed.with_mut(|p| p.set_visibility(vis));
    }

    pub fn direction(&self) -> Direction {
        self.packed.with(Packed::direction)
    }

    pub fn other_location(&self) -> Option<StaticNode<Id, Opt>> {
        self.packed.get().other_location()
    }

    pub fn set_other_location(&self, node: Option<StaticNode<Id, Opt>>) {
        self.packed.with_mut(|p| p.set_other_location(node));
    }

    pub fn old_location(&self) -> Option<StaticNode<Id, Opt>> {
        (self.move_timestamp.get() > 0)
            .then(|| self.other_location())
            .flatten()
    }

    pub fn new_location(&self) -> Option<StaticNode<Id, Opt>> {
        (self.move_timestamp.get() == 0)
            .then(|| self.other_location())
            .flatten()
    }
}

impl<Id: Clone, Opt> Node<Id, Opt> {
    pub fn to_change(&self) -> RemoteChange<Id> {
        RemoteChange {
            id: self.id.clone(),
            parent: self.parent.clone(),
            direction: self.direction(),
            visibility: self.visibility(),
            move_timestamp: self.move_timestamp.get(),
            old_location: self.old_location().map(|n| n.id.clone()),
        }
    }
}

/// Note: this does not set [`Self::packed`].
impl<Id, Opt> From<RemoteChange<Id>> for Node<Id, Opt> {
    fn from(change: RemoteChange<Id>) -> Self {
        let mut node = Self::new(change.id, change.parent);
        let p = node.packed.get_mut();
        p.set_direction(change.direction);
        p.set_visibility(change.visibility);
        node.move_timestamp.set(change.move_timestamp);
        node
    }
}

impl<Id, Opt> fmt::Debug for Node<Id, Opt>
where
    Id: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Node")
            .field("id", &self.id)
            .field("parent", &self.parent)
            .field("direction", &self.direction())
            .field("visibility", &self.visibility())
            .field("move_timestamp", &self.move_timestamp.get())
            .field("other_location", &self.other_location().as_ref().map(|n| &n.id))
            .finish()
    }
}

pub struct StaticNode<Id, Opt>(NonNull<Node<Id, Opt>>);

impl<Id, Opt> StaticNode<Id, Opt> {
    pub unsafe fn new(node: NonNull<Node<Id, Opt>>) -> Self {
        Self(node)
    }

    pub fn ptr(self) -> NonNull<Node<Id, Opt>> {
        self.0
    }

    pub fn oldest_location(self) -> StaticNode<Id, Opt> {
        self.old_location().unwrap_or(self)
    }

    pub fn newest_location(self) -> StaticNode<Id, Opt> {
        self.new_location().unwrap_or(self)
    }
}

impl<Id, Opt> Clone for StaticNode<Id, Opt> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Id, Opt> Copy for StaticNode<Id, Opt> {}

impl<Id, Opt> Deref for StaticNode<Id, Opt> {
    type Target = Node<Id, Opt>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<Id, Opt> fmt::Debug for StaticNode<Id, Opt>
where
    Id: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("StaticNode").field(&self.0).field(&**self).finish()
    }
}

pub struct Packed<Id, Opt>(
    TaggedPtr<Node<Id, Opt>, 2>,
    PhantomData<StaticNode<Id, Opt>>,
);

impl<Id, Opt> Packed<Id, Opt> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Node::sentinel(), 0), PhantomData)
    }

    pub fn other_location(&self) -> Option<StaticNode<Id, Opt>> {
        Some(self.0.ptr())
            .filter(|p| *p != Node::sentinel())
            .map(|p| unsafe { StaticNode::new(p) })
    }

    pub fn set_other_location(&mut self, node: Option<StaticNode<Id, Opt>>) {
        self.0.set_ptr(node.map_or_else(Node::sentinel, StaticNode::ptr));
    }

    pub fn direction(&self) -> Direction {
        Direction::VARIANTS[self.0.tag() & 0b1]
    }

    pub fn set_direction(&mut self, direction: Direction) {
        self.0.set_tag((self.0.tag() & !0b1) | direction as usize);
    }

    pub fn visibility(&self) -> Visibility {
        Visibility::VARIANTS[(self.0.tag() & 0b10) >> 1]
    }

    pub fn set_visibility(&mut self, vis: Visibility) {
        self.0.set_tag((self.0.tag() & !0b10) | ((vis as usize) << 1));
    }
}

impl<Id, Opt> Clone for Packed<Id, Opt> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Id, Opt> Copy for Packed<Id, Opt> {}

impl<Id, Opt> Default for Packed<Id, Opt> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id, Opt> fmt::Debug for Packed<Id, Opt>
where
    Id: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Packed")
            .field("direction", &self.direction())
            .field("visibility", &self.visibility())
            .field("other_location", &self.other_location())
            .finish()
    }
}

/// The visibility of an item in an [`Eips`] sequence.
///
/// [`Eips`]: crate::Eips
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    /// The item is hidden or deleted.
    Hidden = 0,
    /// The item is visible.
    Visible = 1,
}

impl Visibility {
    /// The variants of [`Self`], ordered such that the index of each variant
    /// is its enum discriminant.
    pub const VARIANTS: [Self; 2] = [Self::Hidden, Self::Visible];
}

/// The direction of an item in an [`Eips`] sequence.
///
/// [`Eips`] is a tree-like structure where each item has a notion of
/// direction.
///
/// [`Eips`]: crate::Eips
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    /// The item is ordered before its parent.
    Before = 0,
    /// The item is ordered after its parent.
    After = 1,
}

impl Direction {
    /// The variants of [`Self`], ordered such that the index of each variant
    /// is its enum discriminant.
    pub const VARIANTS: [Self; 2] = [Self::Before, Self::After];
}
