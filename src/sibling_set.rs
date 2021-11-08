use super::align::{Align2, Align4};
use super::node::{Direction, Node, StaticNode};
use super::Id;
use core::cmp::Ordering;
use core::fmt;
use core::marker::PhantomData;
use skip_list::{LeafNext, LeafRef, NoSize, SetNextParams, StoreKeys};
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

#[derive(Clone, Debug)]
pub struct SiblingSetNext<I: Id>(
    TaggedPtr<Align4, 2>,
    PhantomData<StaticNode<I>>,
);

impl<I: Id> SiblingSetNext<I> {
    pub fn new(next: LeafNext<SiblingSetNode<I>>) -> Self {
        let (ptr, tag) = match next {
            LeafNext::Data(data) => (data.cast(), 2),
            LeafNext::Leaf(leaf) => {
                (leaf.node().ptr().cast(), leaf.kind() as usize)
            }
        };
        Self(TaggedPtr::new(ptr, tag), PhantomData)
    }

    pub fn get(&self) -> LeafNext<SiblingSetNode<I>> {
        let (ptr, tag) = self.0.get();
        if (tag & 0b10) != 0 {
            LeafNext::Data(ptr.cast())
        } else {
            LeafNext::Leaf(SiblingSetNode::new(
                unsafe { StaticNode::from_ptr(ptr.cast()) },
                (tag & 0b01).into(),
            ))
        }
    }
}

impl<I: Id> Copy for SiblingSetNext<I> {}

#[derive(Clone)]
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
                    parent: self.node().parent(),
                    child: self.node().id(),
                    direction: self.node().direction(),
                })
            }
            SiblingSetNodeKind::Childless => {
                SiblingSetKey::Childless(self.node().id())
            }
        }
    }
}

impl<I: Id> Copy for SiblingSetNode<I> {}

// Required by `Node::sibling_map_next` for safety
pub struct Token(());

unsafe impl<I: Id> LeafRef for SiblingSetNode<I> {
    type Size = NoSize;
    type StoreKeys = StoreKeys;
    type Align = Align2;
    const FANOUT: usize = I::FANOUT;

    fn next(&self) -> Option<LeafNext<Self>> {
        self.node().sibling_set_next(Token(()))[self.kind() as usize]
            .get()
            .map(|n| n.get())
    }

    fn set_next(params: SetNextParams<'_, Self>) {
        let (this, next) = params.get();
        this.node().sibling_set_next(Token(()))[this.kind() as usize]
            .set(next.map(SiblingSetNext::new))
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
