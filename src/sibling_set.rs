use super::align::{Align2, Align4};
use super::node::{Direction, Node, StaticNode};
use super::Id;
use skip_list::{LeafNext, LeafRef, NoSize, SetNextParams, StoreKeys};
use core::cmp::Ordering;
use core::fmt;
use core::marker::PhantomData;
use tagged_pointer::TaggedPtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiblingSetNodeKind {
    Normal = 0,
    Childless = 1,
}

impl From<usize> for SiblingSetNodeKind {
    fn from(n: usize) -> Self {
        match n {
            0 => Self::Normal,
            1 => Self::Childless,
            n => panic!("bad enum value: {}", n),
        }
    }
}

pub struct SiblingSetNext<I: Id>(
    TaggedPtr<Align4, 2>,
    PhantomData<StaticNode<I>>,
);

impl<I: Id> SiblingSetNext<I> {
    pub fn new() -> Self {
        Self(TaggedPtr::new(Align4::sentinel(), 0), PhantomData)
    }

    fn is_opaque_data(&self) -> bool {
        (self.0.tag() & 0b10) != 0
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        (self.0.tag() & 0b01).into()
    }

    pub fn set_kind(&mut self, kind: SiblingSetNodeKind) {
        let (ptr, tag) = self.0.get();
        self.0 = TaggedPtr::new(ptr, (tag & 0b10) | kind as usize);
    }

    pub fn get(&self) -> Option<LeafNext<SiblingSetNode<I>>> {
        Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()).map(|p| {
            if self.is_opaque_data() {
                LeafNext::Data(p.cast())
            } else {
                LeafNext::Leaf(SiblingSetNode::new(
                    unsafe { StaticNode::from_ptr(p.cast()) },
                    self.kind(),
                ))
            }
        })
    }

    pub fn set(&mut self, next: Option<LeafNext<SiblingSetNode<I>>>) {
        let (ptr, tag) = next.map_or_else(
            || (Align4::sentinel(), 0),
            |n| match n {
                LeafNext::Data(data) => (data.cast(), 2),
                LeafNext::Leaf(leaf) => {
                    (leaf.node().ptr().cast(), leaf.kind() as usize)
                }
            },
        );
        self.0 = TaggedPtr::new(ptr, tag);
    }
}

impl<I: Id> Clone for SiblingSetNext<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for SiblingSetNext<I> {}

impl<I: Id> Default for SiblingSetNext<I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I> fmt::Debug for SiblingSetNext<I>
where
    I: Id + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("SiblingSetNext")
            .field("kind", &self.kind())
            .field("opaque", &self.is_opaque_data())
            .field(
                "ptr",
                &Some(self.0.ptr()).filter(|p| *p != Align4::sentinel()),
            )
            .finish()
    }
}

pub struct SiblingSetNode<I: Id>(
    TaggedPtr<Node<I>, 1>,
    PhantomData<StaticNode<I>>,
);

impl<I: Id> SiblingSetNode<I> {
    pub fn new(node: StaticNode<I>, kind: SiblingSetNodeKind) -> Self {
        Self(TaggedPtr::new(node.ptr(), kind as usize), PhantomData)
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        self.0.tag().into()
    }

    pub fn node(&self) -> StaticNode<I> {
        unsafe { StaticNode::from_ptr(self.0.ptr()) }
    }

    pub fn key(&self) -> SiblingSetKey<I> {
        match self.kind() {
            SiblingSetNodeKind::Normal => {
                SiblingSetKey::Normal(SiblingSetNormalKey {
                    parent: self.node().parent.clone(),
                    child: self.node().id.clone(),
                    direction: self.node().direction(),
                })
            }
            SiblingSetNodeKind::Childless => {
                SiblingSetKey::Childless(self.node().id.clone())
            }
        }
    }
}

impl<I: Id> Clone for SiblingSetNode<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I: Id> Copy for SiblingSetNode<I> {}

// TODO: Much of this can be simplified (no sentinel).

unsafe impl<I: Id> LeafRef for SiblingSetNode<I> {
    type Size = NoSize;
    type StoreKeys = StoreKeys;
    type Align = Align2;
    const FANOUT: usize = I::FANOUT;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().sibling_set_next[self.kind() as usize].get().get()
    }

    fn set_next(params: SetNextParams<'_, Self>) {
        let (this, next) = params.get();
        this.node().sibling_set_next[this.kind() as usize]
            .with_mut(|n| n.set(next))
    }
}

impl<I: Id> PartialEq for SiblingSetNode<I> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<I: Id> Eq for SiblingSetNode<I> {}

impl<I: Id> PartialOrd for SiblingSetNode<I> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<I: Id> Ord for SiblingSetNode<I> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key().cmp(&other.key())
    }
}

impl<I> fmt::Debug for SiblingSetNode<I>
where
    I: Id + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("SiblingSetNode")
            .field("kind", &self.kind())
            .field("node", &self.node())
            .finish()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SiblingSetNormalKey<I: Id> {
    pub parent: Option<I>,
    pub child: I,
    pub direction: Direction,
}

pub enum SiblingSetKey<I: Id> {
    Normal(SiblingSetNormalKey<I>),
    Childless(I),
}

impl<I: Id> PartialEq for SiblingSetKey<I> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<I: Id> Eq for SiblingSetKey<I> {}

impl<I: Id> PartialOrd for SiblingSetKey<I> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<I: Id> Ord for SiblingSetKey<I> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Normal(a), Self::Normal(b)) => a.cmp(b),
            (Self::Normal(a), Self::Childless(b)) => {
                match a.parent.as_ref().cmp(&Some(b)) {
                    ord @ (Ordering::Less | Ordering::Greater) => return ord,
                    Ordering::Equal => {}
                }
                match a.direction {
                    Direction::Before => Ordering::Less,
                    Direction::After => Ordering::Greater,
                }
            }
            (Self::Childless(a), Self::Normal(b)) => {
                match Some(a).cmp(&b.parent.as_ref()) {
                    ord @ (Ordering::Less | Ordering::Greater) => return ord,
                    Ordering::Equal => {}
                }
                match b.direction {
                    Direction::Before => Ordering::Greater,
                    Direction::After => Ordering::Less,
                }
            }
            (Self::Childless(a), Self::Childless(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug)]
pub struct FindChildless<I>(pub I);

impl<I: Id> PartialEq<SiblingSetNode<I>> for FindChildless<I> {
    fn eq(&self, other: &SiblingSetNode<I>) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl<I: Id> PartialOrd<SiblingSetNode<I>> for FindChildless<I> {
    fn partial_cmp(&self, other: &SiblingSetNode<I>) -> Option<Ordering> {
        Some(SiblingSetKey::Childless(self.0.clone()).cmp(&other.key()))
    }
}
