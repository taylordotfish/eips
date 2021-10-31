use super::align::Align4;
use super::pos_map::PosMapNext;
use super::sibling_set::SiblingSetNext;
use super::Id;
use super::Insertion;
use crate::cell::{Cell, CellDefaultExt};
use core::marker::PhantomData;
use core::ptr::NonNull;
use tagged_pointer::TaggedPtr;

#[repr(align(4))]
pub struct Node<I: Id> {
    pub id: I,
    pub parent: Option<I>,
    pub pos_map_next: [Cell<PosMapNext<I>>; 2],
    pub sibling_set_next: [Cell<SiblingSetNext<I>>; 2],
    pub move_timestamp: Cell<usize>,
    pub old_location: Cell<Option<&'static Self>>,
    pub new_location: Cell<NewLocation<I>>,
}

impl<I: Id> Node<I> {
    pub fn from_insertion(insertion: &Insertion<I>) -> Self {
        let mut new_location = NewLocation::new();
        new_location.set_direction(insertion.direction);
        Self {
            id: insertion.id.clone(),
            parent: insertion.parent.clone(),
            pos_map_next: Default::default(),
            sibling_set_next: Default::default(),
            move_timestamp: Cell::default(),
            old_location: Cell::default(),
            new_location: Cell::new(new_location),
        }
    }

    pub fn visibility(&self) -> Visibility {
        self.new_location.get().visibility()
    }

    pub fn set_visibility(&self, vis: Visibility) {
        self.new_location.with_mut(|n| n.set_visibility(vis));
    }

    pub fn direction(&self) -> Direction {
        self.new_location.get().direction()
    }

    pub fn new_location_or_self(&'static self) -> &'static Self {
        if let Some(node) = self.new_location.get().get() {
            node
        } else {
            self
        }
    }
}

pub struct NewLocation<I: Id>(
    TaggedPtr<Align4, 2>,
    PhantomData<&'static Node<I>>,
);

impl<I: Id> NewLocation<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn get(&self) -> Option<&'static Node<I>> {
        Some(self.0.ptr())
            .filter(|p| *p != Align4::sentinel())
            .map(|p| unsafe { p.cast().as_ref() })
    }

    pub fn set(&mut self, node: Option<&'static Node<I>>) {
        self.0 = TaggedPtr::new(
            node.map_or_else(Align4::sentinel, |r| NonNull::from(r).cast()),
            self.0.tag(),
        );
    }

    pub fn direction(&self) -> Direction {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_direction(&mut self, direction: Direction) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | direction as usize);
    }

    pub fn visibility(&self) -> Visibility {
        ((self.0.tag() & 0b10) >> 1).into()
    }

    pub fn set_visibility(&mut self, vis: Visibility) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b01) | ((vis as usize) << 1));
    }
}

impl<I: Id> Clone for NewLocation<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for NewLocation<I> {}

impl<I: Id> Default for NewLocation<I> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    Visible = 0,
    Hidden = 1,
}

impl From<usize> for Visibility {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::Visible,
            1 => Self::Hidden,
            n => panic!("bad enum value: {}", n),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    After = 0,
    Before = 1,
}

impl From<usize> for Direction {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::After,
            1 => Self::Before,
            n => panic!("bad enum value: {}", n),
        }
    }
}
