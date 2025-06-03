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

use crate::changes::RemoteChange;
use crate::options::{self, EipsOptions};
use crate::pos_map::{self, PosMapNext};
use crate::sibling_set::{self, SiblingSetNext};
use cell_ref::Cell;
use core::fmt;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ptr::NonNull;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use tagged_pointer::TaggedPtr;

#[repr(align(4))]
pub struct Node<Id, Opt: EipsOptions> {
    pub id: Id,
    pub raw_parent: Id,
    packed: options::Packed<Id, Opt>,
    pos_map_next: [Cell<Option<PosMapNext<Id, Opt>>>; 2],
    sibling_set_next: [Cell<Option<SiblingSetNext<Id, Opt>>>; 2],
    _phantom: PhantomData<Opt>,
}

impl<Id, Opt> Node<Id, Opt>
where
    Opt: EipsOptions,
{
    pub const SUPPORTS_MOVE: bool = options::Packed::<Id, Opt>::SUPPORTS_MOVE;

    fn sentinel() -> NonNull<Self> {
        #[repr(align(4))]
        struct Align4(#[allow(dead_code)] u32);

        static SENTINEL: Align4 = Align4(0);
        NonNull::from(&SENTINEL).cast()
    }

    fn new(id: Id, raw_parent: Id) -> Self {
        Self {
            id,
            raw_parent,
            packed: Default::default(),
            pos_map_next: Default::default(),
            sibling_set_next: Default::default(),
            _phantom: PhantomData,
        }
    }

    pub fn parent(&self) -> Option<&Id>
    where
        Id: PartialEq,
    {
        if self.id == self.raw_parent {
            None
        } else {
            Some(&self.raw_parent)
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
        self.packed.visibility()
    }

    pub fn set_visibility(&self, vis: Visibility) {
        self.packed.set_visibility(vis);
    }

    pub fn direction(&self) -> Direction {
        self.packed.direction()
    }

    /// Panics if moves aren't supported.
    pub fn other_location(&self) -> Option<StaticNode<Id, Opt>> {
        self.packed.other_location()
    }

    /// Panics if moves aren't supported.
    pub fn set_other_location(&self, node: Option<StaticNode<Id, Opt>>) {
        self.packed.set_other_location(node)
    }

    /// Panics if moves aren't supported.
    pub fn move_timestamp(&self) -> crate::MoveTimestamp {
        self.packed.move_timestamp()
    }

    pub fn move_timestamp_or_none(&self) -> Option<crate::MoveTimestamp> {
        self.supports_move().then(|| self.move_timestamp())
    }

    pub fn supports_move(&self) -> bool {
        Self::SUPPORTS_MOVE
    }

    pub fn old_location(&self) -> Option<StaticNode<Id, Opt>> {
        self.move_timestamp_or_none()
            .filter(|&ts| ts > 0)
            .and_then(|_| self.other_location())
    }

    pub fn new_location(&self) -> Option<StaticNode<Id, Opt>> {
        self.move_timestamp_or_none()
            .filter(|&ts| ts == 0)
            .and_then(|_| self.other_location())
    }
}

/// Note: this does not set [`Self::other_location`].
impl<Id, Opt> From<RemoteChange<Id>> for Node<Id, Opt>
where
    Opt: EipsOptions,
{
    fn from(change: RemoteChange<Id>) -> Self {
        let node = Self::new(change.id, change.raw_parent);
        let p = &node.packed;
        p.set_direction(change.direction);
        p.set_visibility(change.visibility);
        if let Some(mv) = change.move_info {
            debug_assert!(!p.supports_move());
            p.set_move_timestamp(mv.timestamp.get());
        }
        node
    }
}

impl<Id, Opt> fmt::Debug for Node<Id, Opt>
where
    Id: PartialEq + fmt::Debug,
    Opt: EipsOptions,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Node")
            .field("id", &self.id)
            .field("parent", &self.parent())
            .field("direction", &self.direction())
            .field("visibility", &self.visibility())
            .field("move_timestamp", &self.move_timestamp())
            .field(
                "other_location",
                &self.other_location().as_ref().map(|n| &n.id),
            )
            .finish()
    }
}

pub struct StaticNode<Id, Opt: EipsOptions>(NonNull<Node<Id, Opt>>);

impl<Id, Opt> StaticNode<Id, Opt>
where
    Opt: EipsOptions,
{
    /// # Safety
    ///
    /// * `node` must be a valid pointer to a [`Node<Id, Opt>`].
    /// * The [`Node`] must live at least as long as the [`StaticNode`]
    ///   returned by this function.
    /// * The returned [`StaticNode`] conceptually holds a shared reference
    ///   to the [`Node`]; therefore, as long as there exists a [`StaticNode`]
    ///   that points to the [`Node`], the [`Node`] may not be accessed in ways
    ///   that violate Rust's standard aliasing model (in particular, no
    ///   mutable access).
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

impl<Id, Opt> Clone for StaticNode<Id, Opt>
where
    Opt: EipsOptions,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<Id, Opt> Copy for StaticNode<Id, Opt> where Opt: EipsOptions {}

impl<Id, Opt> Deref for StaticNode<Id, Opt>
where
    Opt: EipsOptions,
{
    type Target = Node<Id, Opt>;

    fn deref(&self) -> &Self::Target {
        // SAFETY: Safe due to the invariants described in [`Self::new`].
        unsafe { self.0.as_ref() }
    }
}

impl<Id, Opt> fmt::Debug for StaticNode<Id, Opt>
where
    Id: PartialEq + fmt::Debug,
    Opt: EipsOptions,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("StaticNode").field(&self.0).field(&**self).finish()
    }
}

pub trait Packed<Id, Opt: EipsOptions>: Default {
    const SUPPORTS_MOVE: bool;

    fn supports_move(&self) -> bool {
        Self::SUPPORTS_MOVE
    }

    fn new() -> Self;

    fn move_timestamp(&self) -> crate::MoveTimestamp {
        unimplemented!("Packed::move_timestamp");
    }

    fn set_move_timestamp(&self, _ts: crate::MoveTimestamp) {
        unimplemented!("Packed::set_move_timestamp");
    }

    fn other_location(&self) -> Option<StaticNode<Id, Opt>> {
        unimplemented!("Packed::other_location");
    }

    fn set_other_location(&self, _node: Option<StaticNode<Id, Opt>>) {
        unimplemented!("Packed::set_other_location");
    }

    fn direction(&self) -> Direction;
    fn set_direction(&self, direction: Direction);
    fn visibility(&self) -> Visibility;
    fn set_visibility(&self, vis: Visibility);
}

pub struct FullPacked<Id, Opt: EipsOptions> {
    tp: Cell<TaggedPtr<Node<Id, Opt>, 2>>,
    ts: Cell<crate::MoveTimestamp>,
    _phantom: PhantomData<StaticNode<Id, Opt>>,
}

impl<Id, Opt> FullPacked<Id, Opt>
where
    Opt: EipsOptions,
{
    fn tp(&self) -> TaggedPtr<Node<Id, Opt>, 2> {
        self.tp.get()
    }
}

impl<Id, Opt> Packed<Id, Opt> for FullPacked<Id, Opt>
where
    Opt: EipsOptions,
{
    const SUPPORTS_MOVE: bool = true;

    fn new() -> Self {
        Self {
            tp: Cell::new(TaggedPtr::new(Node::sentinel(), 0)),
            ts: Cell::default(),
            _phantom: PhantomData,
        }
    }

    fn move_timestamp(&self) -> crate::MoveTimestamp {
        self.ts.get()
    }

    fn set_move_timestamp(&self, ts: crate::MoveTimestamp) {
        self.ts.set(ts);
    }

    fn other_location(&self) -> Option<StaticNode<Id, Opt>> {
        Some(self.tp().ptr()).filter(|p| *p != Node::sentinel()).map(|p| {
            // SAFETY: The pointer in `self.tp` always originates from a
            // valid `StaticNode` (except for the sentinel pointer, which
            // we already checked).
            unsafe { StaticNode::new(p) }
        })
    }

    fn set_other_location(&self, node: Option<StaticNode<Id, Opt>>) {
        self.tp.with_mut(|tp| {
            tp.set_ptr(node.map_or_else(Node::sentinel, StaticNode::ptr));
        });
    }

    fn direction(&self) -> Direction {
        Direction::VARIANTS[self.tp().tag() & 0b1]
    }

    fn set_direction(&self, direction: Direction) {
        self.tp.with_mut(|tp| {
            tp.set_tag((tp.tag() & !0b1) | direction as usize);
        });
    }

    fn visibility(&self) -> Visibility {
        Visibility::VARIANTS[(self.tp().tag() & 0b10) >> 1]
    }

    fn set_visibility(&self, vis: Visibility) {
        self.tp.with_mut(|tp| {
            tp.set_tag((tp.tag() & !0b10) | ((vis as usize) << 1));
        });
    }
}

impl<Id, Opt> Default for FullPacked<Id, Opt>
where
    Opt: EipsOptions,
{
    fn default() -> Self {
        Self::new()
    }
}

pub struct MinimalPacked<Id, Opt>(Cell<u8>, PhantomData<fn() -> (Id, Opt)>);

impl<Id, Opt> Packed<Id, Opt> for MinimalPacked<Id, Opt>
where
    Opt: EipsOptions,
{
    const SUPPORTS_MOVE: bool = false;

    fn new() -> Self {
        Self(Cell::new(0), PhantomData)
    }

    fn direction(&self) -> Direction {
        Direction::VARIANTS[(self.0.get() & 0b1) as usize]
    }

    fn set_direction(&self, direction: Direction) {
        self.0.with_mut(|n| {
            *n &= !0b1;
            *n |= direction as u8;
        });
    }

    fn visibility(&self) -> Visibility {
        Visibility::VARIANTS[((self.0.get() & 0b10) >> 1) as usize]
    }

    fn set_visibility(&self, vis: Visibility) {
        self.0.with_mut(|n| {
            *n &= !0b10;
            *n |= (vis as u8) << 1;
        });
    }
}

impl<Id, Opt> Default for MinimalPacked<Id, Opt>
where
    Opt: EipsOptions,
{
    fn default() -> Self {
        Self::new()
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
