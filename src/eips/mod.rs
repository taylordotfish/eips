use crate::allocator::{alloc_value, dealloc_value};
use crate::cell::CellDefaultExt;
use crate::skip_list::SkipList;
use core::cmp::Ordering;
use core::mem::ManuallyDrop;
use core::num::Wrapping;

mod align;
pub mod allocators;
mod node;
mod pos_map;
mod sibling_set;

pub use allocators::Allocators;
use node::Direction;
pub use node::Visibility;
use node::{Node, StaticNode};
use pos_map::{PosMapNode, PosMapNodeKind};
use sibling_set::FindChildless;
use sibling_set::{SiblingSetNode, SiblingSetNodeKind};

pub trait Id: 'static + Sized + Clone + Ord {}
impl<I: 'static + Sized + Clone + Ord> Id for I {}

pub struct Insertion<I> {
    pub id: I,
    pub parent: Option<I>,
    pub direction: Direction,
}

impl<I: Id> PartialEq<SiblingSetNode<I>> for Insertion<I> {
    fn eq(&self, other: &SiblingSetNode<I>) -> bool {
        matches!(self.partial_cmp(other), Some(Ordering::Equal))
    }
}

impl<I: Id> PartialOrd<SiblingSetNode<I>> for Insertion<I> {
    fn partial_cmp(&self, other: &SiblingSetNode<I>) -> Option<Ordering> {
        match self.parent.cmp(&other.parent_id()) {
            Ordering::Equal => {}
            ordering => return Some(ordering),
        }
        Some((self.direction, self.id.clone()).cmp(&(
            match other.direction() {
                Some(direction) => direction,
                None => return Some(Ordering::Greater),
            },
            match other.child_id() {
                Some(child) => child,
                None => return Some(Ordering::Greater),
            },
        )))
    }
}

pub struct Move<I> {
    pub insertion: Insertion<I>,
    pub old: I,
    pub timestamp: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BadIndex;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BadId;

pub struct Eips<I, A>
where
    I: Id,
    A: Allocators,
{
    node_alloc: A::Alloc0,
    pos_map: ManuallyDrop<SkipList<PosMapNode<I>, A::Alloc1>>,
    sibling_set: ManuallyDrop<SkipList<SiblingSetNode<I>, A::Alloc2>>,
}

impl<I, A> Eips<I, A>
where
    I: Id,
    A: Allocators,
{
    pub fn new(allocators: A) -> Self {
        let allocators = allocators.into_allocators();
        Self {
            node_alloc: allocators.0,
            pos_map: ManuallyDrop::new(SkipList::new_in(allocators.1)),
            sibling_set: ManuallyDrop::new(SkipList::new_in(allocators.2)),
        }
    }

    fn alloc_node(&self, node: Node<I>) -> StaticNode<I> {
        StaticNode::new(unsafe {
            alloc_value(node, &self.node_alloc).as_mut()
        })
    }

    pub fn local_get(&self, index: usize) -> Result<I, BadIndex> {
        Ok(self
            .pos_map
            .get(Wrapping(index))
            .ok_or(BadIndex)?
            .node()
            .id
            .clone())
    }

    pub fn remote_get(&self, id: I) -> Result<usize, BadId> {
        let node =
            self.sibling_set.find(&FindChildless(id)).ok_or(BadId)?.node();
        Ok(self
            .pos_map
            .position(PosMapNode::new(node, PosMapNodeKind::Normal))
            .0)
    }

    pub fn local_insert(
        &mut self,
        index: usize,
        id: I,
    ) -> Result<Insertion<I>, BadIndex> {
        Ok(if let Some(index) = index.checked_sub(1) {
            let pos_node =
                self.pos_map.get(Wrapping(index)).ok_or(BadIndex)?;
            let node = pos_node.node();
            let child = self
                .sibling_set
                .next(SiblingSetNode::new(node, SiblingSetNodeKind::Childless))
                .map(|n| n.node());

            let can_insert_after =
                child.map_or(true, |c| c.parent.as_ref() != Some(&node.id));
            if can_insert_after {
                Insertion {
                    id,
                    parent: Some(node.id.clone()),
                    direction: Direction::After,
                }
            } else {
                let next = self.pos_map.next(pos_node).unwrap().node();
                Insertion {
                    id,
                    parent: Some(next.id.clone()),
                    direction: Direction::Before,
                }
            }
        } else if let Some(node) = self.sibling_set.first().map(|n| n.node()) {
            Insertion {
                id,
                parent: Some(node.id.clone()),
                direction: Direction::Before,
            }
        } else {
            Insertion {
                id,
                parent: None,
                direction: Direction::After,
            }
        })
    }

    fn remote_insert_node(
        &mut self,
        node: StaticNode<I>,
        insertion: Insertion<I>,
    ) -> Result<(), BadId> {
        let sibling = self.sibling_set.find_closest(&insertion);
        if sibling.map_or(false, |s| s.node().id == insertion.id) {
            return Err(BadId);
        }

        let nodes = [PosMapNodeKind::Normal, PosMapNodeKind::Marker]
            .map(|kind| PosMapNode::new(node, kind));
        let node_as_sibling =
            SiblingSetNode::new(node, SiblingSetNodeKind::Normal);

        let neighbor = |sibling: SiblingSetNode<I>| {
            PosMapNode::new(
                sibling.node(),
                if sibling.child_id().is_some() {
                    PosMapNodeKind::Marker
                } else {
                    PosMapNodeKind::Normal
                },
            )
        };

        if let Some(sibling) = sibling {
            self.sibling_set.insert_after(sibling, node_as_sibling);
            match insertion.direction {
                Direction::After => {
                    self.pos_map.insert_after_from(neighbor(sibling), nodes);
                }
                Direction::Before => {
                    self.pos_map.insert_before_from(
                        neighbor(sibling),
                        nodes.into_iter().rev(),
                    );
                }
            }
        } else {
            debug_assert!(node.parent.is_none());
            self.sibling_set.insert_at_start(node_as_sibling);
            self.pos_map.insert_at_start_from(nodes);
        }
        Ok(())
    }

    pub fn remote_insert(
        &mut self,
        insertion: Insertion<I>,
    ) -> Result<usize, BadId> {
        let node = self.alloc_node(Node::from_insertion(&insertion));
        self.remote_insert_node(node, insertion)?;
        Ok(self
            .pos_map
            .position(PosMapNode::new(node, PosMapNodeKind::Normal))
            .0)
    }

    pub fn local_remove(&mut self, index: usize) -> Result<I, BadIndex> {
        Ok(self
            .pos_map
            .get(Wrapping(index))
            .ok_or(BadIndex)?
            .node()
            .id
            .clone())
    }

    pub fn remote_remove(&mut self, id: I) -> Result<usize, BadId> {
        let node =
            self.sibling_set.find(&FindChildless(id)).ok_or(BadId)?.node();
        let pos_node = PosMapNode::new(node, PosMapNodeKind::Normal);
        let index = self.pos_map.position(pos_node).0;
        self.pos_map.update(pos_node, |node| {
            node.node().set_visibility(Visibility::Hidden);
        });
        Ok(index)
    }

    pub fn local_move(
        &mut self,
        old: usize,
        new: usize,
        id: I,
    ) -> Result<Move<I>, BadIndex> {
        let node = self.pos_map.get(Wrapping(old)).ok_or(BadIndex)?.node();
        Ok(Move {
            insertion: self.local_insert(new + (new > old) as usize, id)?,
            old: node.id.clone(),
            timestamp: node.move_timestamp.get() + 1,
        })
    }

    pub fn remote_move(
        &mut self,
        mv: Move<I>,
    ) -> Result<Option<(usize, usize)>, BadId> {
        let old =
            self.sibling_set.find(&FindChildless(mv.old)).ok_or(BadId)?.node();
        let old = Node::new_location_or_self(old);
        let old_pos = PosMapNode::new(old, PosMapNodeKind::Normal);
        let new = self.alloc_node(Node::from_insertion(&mv.insertion));
        let new_pos = PosMapNode::new(old, PosMapNodeKind::Normal);
        Ok(
            if match mv.timestamp.cmp(&old.move_timestamp.get()) {
                Ordering::Greater => true,
                Ordering::Equal => old
                    .old_location
                    .get()
                    .map_or(true, |node| new.id > node.id),
                _ => false,
            } {
                old.new_location.with_mut(|n| n.set(Some(new)));
                self.pos_map.update(old_pos, |node| {
                    node.node().set_visibility(Visibility::Hidden);
                });
                let old_index = self.pos_map.position(old_pos).0;
                self.remote_insert_node(new, mv.insertion)?;
                Some((old_index, self.pos_map.position(new_pos).0))
            } else {
                new.set_visibility(Visibility::Hidden);
                self.remote_insert_node(new, mv.insertion)?;
                None
            },
        )
    }
}

unsafe impl<I, A> Send for Eips<I, A>
where
    I: Id + Send,
    A: Allocators + Send,
{
}

impl<I, A> Drop for Eips<I, A>
where
    I: Id,
    A: Allocators,
{
    fn drop(&mut self) {
        let mut head = None;
        unsafe { ManuallyDrop::drop(&mut self.pos_map) };
        for node in unsafe { ManuallyDrop::take(&mut self.sibling_set) } {
            if node.kind() == SiblingSetNodeKind::Childless {
                let node = node.node();
                node.old_location.set(head);
                head = Some(node);
            }
        }
        while let Some(node) = head {
            let next = node.old_location.get();
            unsafe { dealloc_value(node.ptr(), &self.node_alloc) };
            head = next;
        }
    }
}
