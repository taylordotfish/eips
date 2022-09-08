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

impl SiblingSetNodeKind {
    pub const VARIANTS: [Self; 2] = [Self::Normal, Self::Childless];
}

#[derive(Debug)]
pub struct SiblingSetNext<I>(TaggedPtr<Align4, 2>, PhantomData<StaticNode<I>>);

impl<I: Id> SiblingSetNext<I> {
    pub fn new(next: LeafNext<SiblingSetNode<I>>) -> Self {
        let (ptr, tag) = match next {
            LeafNext::Data(data) => {
                (data.cast(), SiblingSetNodeKind::VARIANTS.len())
            }
            LeafNext::Leaf(leaf) => {
                (leaf.node().ptr().cast(), leaf.kind() as usize)
            }
        };
        Self(TaggedPtr::new(ptr, tag), PhantomData)
    }

    pub fn get(&self) -> LeafNext<SiblingSetNode<I>> {
        let (ptr, tag) = self.0.get();
        if tag == SiblingSetNodeKind::VARIANTS.len() {
            LeafNext::Data(ptr.cast())
        } else {
            LeafNext::Leaf(SiblingSetNode::new(
                unsafe { StaticNode::new(ptr.cast().as_mut()) },
                SiblingSetNodeKind::VARIANTS[tag],
            ))
        }
    }
}

impl<I> Clone for SiblingSetNext<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I> Copy for SiblingSetNext<I> {}

pub struct SiblingSetNode<I>(
    TaggedPtr<Node<I>, 1>,
    PhantomData<StaticNode<I>>,
);

impl<I> SiblingSetNode<I> {
    pub fn new(node: StaticNode<I>, kind: SiblingSetNodeKind) -> Self {
        Self(TaggedPtr::new(node.ptr(), kind as usize), PhantomData)
    }

    pub fn kind(&self) -> SiblingSetNodeKind {
        SiblingSetNodeKind::VARIANTS[self.0.tag()]
    }

    pub fn node(&self) -> StaticNode<I> {
        unsafe { StaticNode::new(self.0.ptr().as_mut()) }
    }
}

impl<I: Id> SiblingSetNode<I> {
    pub fn key(&self) -> SiblingSetKey<I> {
        match self.kind() {
            SiblingSetNodeKind::Normal => SiblingSetKey::Normal {
                parent: self.node().parent(),
                direction: self.node().direction(),
                child: self.node().id(),
            },
            SiblingSetNodeKind::Childless => {
                SiblingSetKey::Childless(self.node().id())
            }
        }
    }
}

impl<I> Clone for SiblingSetNode<I> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<I> Copy for SiblingSetNode<I> {}

// Required by `Node::sibling_set_next` for safety
pub struct Token(());

unsafe impl<I: Id> LeafRef for SiblingSetNode<I> {
    type Size = NoSize;
    type StoreKeys = StoreKeys<true>;
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
        self.cmp(other).is_eq()
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

pub enum SiblingSetKey<I> {
    Normal {
        parent: Option<I>,
        direction: Direction,
        child: I,
    },
    Childless(I),
}

impl<I: Ord> PartialEq for SiblingSetKey<I> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl<I: Ord> Eq for SiblingSetKey<I> {}

impl<I: Ord> PartialOrd for SiblingSetKey<I> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<I: Ord> Ord for SiblingSetKey<I> {
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
