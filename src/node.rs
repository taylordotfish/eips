use super::align::Align4;
use super::pos_map::PosMapNext;
use super::sibling_set::SiblingSetNext;
use super::Id;
use super::Insertion;
use default_cell::Cell;
use core::fmt;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ptr::NonNull;
use tagged_pointer::TaggedPtr;

#[derive(Debug)]
#[repr(align(4))]
pub struct Node<I: Id> {
    pub id: I,
    pub parent: Option<I>,
    pub pos_map_next: [Cell<PosMapNext<I>>; 2],
    pub sibling_set_next: [Cell<SiblingSetNext<I>>; 2],
    pub move_timestamp: Cell<usize>,
    pub other_location: Cell<OtherLocation<I>>,
}

impl<I: Id> Node<I> {
    pub fn from_insertion(insertion: &Insertion<I>) -> Self {
        let mut other_location = OtherLocation::new();
        other_location.set_direction(insertion.direction);
        Self {
            id: insertion.id.clone(),
            parent: insertion.parent.clone(),
            pos_map_next: Default::default(),
            sibling_set_next: Default::default(),
            move_timestamp: Cell::default(),
            other_location: Cell::new(other_location),
        }
    }

    pub fn visibility(&self) -> Visibility {
        self.other_location.get().visibility()
    }

    pub fn set_visibility(&self, vis: Visibility) {
        self.other_location.with_mut(|n| n.set_visibility(vis));
    }

    pub fn direction(&self) -> Direction {
        self.other_location.get().direction()
    }

    pub fn other_location_or_self(this: StaticNode<I>) -> StaticNode<I> {
        if let Some(node) = this.other_location.get().get() {
            node
        } else {
            this
        }
    }
}

pub struct StaticNode<I: Id>(NonNull<Node<I>>);

impl<I: Id> StaticNode<I> {
    pub fn new(node: &'static mut Node<I>) -> Self {
        Self(NonNull::from(node))
    }

    pub unsafe fn from_ptr(ptr: NonNull<Node<I>>) -> Self {
        Self(ptr)
    }

    pub fn ptr(&self) -> NonNull<Node<I>> {
        self.0
    }
}

impl<I: Id> Clone for StaticNode<I> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<I: Id> Copy for StaticNode<I> {}

impl<I: Id> Deref for StaticNode<I> {
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

pub struct OtherLocation<I: Id>(
    TaggedPtr<Align4, 2>,
    PhantomData<StaticNode<I>>,
);

impl<I: Id> OtherLocation<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    pub fn get(&self) -> Option<StaticNode<I>> {
        Some(self.0.ptr())
            .filter(|p| *p != Align4::sentinel())
            .map(|p| unsafe { StaticNode::from_ptr(p.cast()) })
    }

    pub fn set(&mut self, node: Option<StaticNode<I>>) {
        self.0 = TaggedPtr::new(
            node.map_or_else(Align4::sentinel, |n| n.ptr().cast()),
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

impl<I: Id> Clone for OtherLocation<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for OtherLocation<I> {}

impl<I: Id> Default for OtherLocation<I> {
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
