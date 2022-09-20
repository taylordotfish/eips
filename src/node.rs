use super::align::Align4;
use super::pos_map::{self, PosMapNext};
use super::sibling_set::{self, SiblingSetNext};
use super::{Id, Slot};
use cell_ref::{Cell, CellExt};
use core::fmt;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ptr::NonNull;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use tagged_pointer::TaggedPtr;

#[repr(align(4))]
pub struct Node<I> {
    id: Cell<Option<I>>,
    parent: Cell<Option<I>>,
    pub move_timestamp: Cell<usize>,
    pub other_location: Cell<OtherLocation<I>>,
    pos_map_next: [Cell<Option<PosMapNext<I>>>; 2],
    sibling_set_next: [Cell<Option<SiblingSetNext<I>>>; 2],
}

impl<I> Node<I> {
    pub fn new(id: I, parent: Option<I>) -> Self {
        Self {
            id: Cell::new(Some(id)),
            parent: Cell::new(parent),
            move_timestamp: Cell::default(),
            other_location: Cell::default(),
            pos_map_next: Default::default(),
            sibling_set_next: Default::default(),
        }
    }

    pub fn pos_map_next(
        &self,
        _: pos_map::Token,
    ) -> &[Cell<Option<PosMapNext<I>>>; 2] {
        &self.pos_map_next
    }

    pub fn sibling_set_next(
        &self,
        _: sibling_set::Token,
    ) -> &[Cell<Option<SiblingSetNext<I>>>; 2] {
        &self.sibling_set_next
    }

    pub fn visibility(&self) -> Visibility {
        self.other_location.with(OtherLocation::visibility)
    }

    pub fn set_visibility(&self, vis: Visibility) {
        self.other_location.with_mut(|ol| ol.set_visibility(vis));
    }

    pub fn direction(&self) -> Direction {
        self.other_location.with(OtherLocation::direction)
    }

    pub fn other_location(&self) -> Option<StaticNode<I>> {
        self.other_location.get().get()
    }
}

impl<I: Id> Node<I> {
    pub fn to_slot(&self) -> Slot<I> {
        Slot {
            id: self.id(),
            parent: self.parent(),
            direction: self.direction(),
            visibility: self.visibility(),
            move_timestamp: self.move_timestamp.get(),
            other_location: self.other_location().map(|n| n.id()),
        }
    }

    pub fn id(&self) -> I {
        self.id.get().unwrap()
    }

    pub fn parent(&self) -> Option<I> {
        self.parent.get()
    }

    pub fn swap_id(&self, other: &Self) {
        self.id.swap(&other.id);
        self.parent.swap(&other.parent);
        let dir1 = self.direction();
        let dir2 = other.direction();
        self.other_location.with_mut(|ol| ol.set_direction(dir2));
        other.other_location.with_mut(|ol| ol.set_direction(dir1));
    }
}

impl<I> From<Slot<I>> for Node<I> {
    /// Note: this does not set [`Self::other_location`].
    fn from(slot: Slot<I>) -> Self {
        let mut node = Self::new(slot.id, slot.parent);
        let ol = node.other_location.get_mut();
        ol.set_direction(slot.direction);
        ol.set_visibility(slot.visibility);
        node.move_timestamp.set(slot.move_timestamp);
        node
    }
}

impl<I> fmt::Debug for Node<I>
where
    I: Id + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Node")
            .field("id", &self.id())
            .field("parent", &self.parent())
            .field("direction", &self.direction())
            .field("visibility", &self.visibility())
            .field("move_timestamp", &self.move_timestamp)
            .field("other_location", &self.other_location().map(|n| n.id()))
            .finish()
    }
}

pub struct StaticNode<I>(NonNull<Node<I>>);

impl<I> StaticNode<I> {
    pub unsafe fn new(node: &mut Node<I>) -> Self {
        Self(NonNull::from(node))
    }

    pub fn ptr(&self) -> NonNull<Node<I>> {
        self.0
    }
}

impl<I> Clone for StaticNode<I> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<I> Copy for StaticNode<I> {}

impl<I> Deref for StaticNode<I> {
    type Target = Node<I>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<I> fmt::Debug for StaticNode<I>
where
    I: Id + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_tuple("StaticNode").field(&self.0).field(&**self).finish()
    }
}

pub struct OtherLocation<I>(TaggedPtr<Align4, 2>, PhantomData<StaticNode<I>>);

impl<I> OtherLocation<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn get(&self) -> Option<StaticNode<I>> {
        Some(self.0.ptr())
            .filter(|p| *p != Align4::sentinel())
            .map(|p| unsafe { StaticNode::new(p.cast().as_mut()) })
    }

    pub fn set(&mut self, node: Option<StaticNode<I>>) {
        self.0 = TaggedPtr::new(
            node.map_or_else(Align4::sentinel, |n| n.ptr().cast()),
            self.0.tag(),
        );
    }

    pub fn direction(&self) -> Direction {
        Direction::VARIANTS[self.0.tag() & 0b1]
    }

    pub fn set_direction(&mut self, direction: Direction) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & !0b1) | direction as usize);
    }

    pub fn visibility(&self) -> Visibility {
        Visibility::VARIANTS[(self.0.tag() & 0b10) >> 1]
    }

    pub fn set_visibility(&mut self, vis: Visibility) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & !0b10) | ((vis as usize) << 1));
    }
}

impl<I> Clone for OtherLocation<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I> Copy for OtherLocation<I> {}

impl<I> Default for OtherLocation<I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I> fmt::Debug for OtherLocation<I>
where
    I: Id + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("OtherLocation")
            .field("direction", &self.direction())
            .field("visibility", &self.visibility())
            .field("node", &self.get())
            .finish()
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    Visible = 0,
    Hidden = 1,
}

impl Visibility {
    const VARIANTS: [Self; 2] = [Self::Visible, Self::Hidden];
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Before = 0,
    After = 1,
}

impl Direction {
    const VARIANTS: [Self; 2] = [Self::Before, Self::After];
}
