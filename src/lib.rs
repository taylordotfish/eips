#![cfg_attr(not(feature = "std"), no_std)]
#![deny(unsafe_op_in_unsafe_fn)]

use allocator_fallback::{alloc_value, dealloc_value};
use skip_list::SkipList;
use core::cmp::Ordering;
use core::mem::ManuallyDrop;
use core::num::Wrapping;

extern crate alloc;

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
use sibling_set::{FindChildless, SiblingSetKey, SiblingSetNormalKey};
use sibling_set::{SiblingSetNode, SiblingSetNodeKind};

pub trait Id: 'static + Sized + Clone + Ord + core::fmt::Debug {
    const FANOUT: usize = 16;
}

impl Id for u32 {}
impl Id for u64 {}
impl Id for u128 {}

pub struct Insertion<I> {
    pub id: I,
    pub parent: Option<I>,
    pub direction: Direction,
}

impl<I: Id> PartialEq<SiblingSetNode<I>> for Insertion<I> {
    fn eq(&self, other: &SiblingSetNode<I>) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl<I: Id> PartialOrd<SiblingSetNode<I>> for Insertion<I> {
    fn partial_cmp(&self, other: &SiblingSetNode<I>) -> Option<Ordering> {
        Some(
            SiblingSetKey::Normal(SiblingSetNormalKey {
                parent: self.parent.clone(),
                child: self.id.clone(),
                direction: self.direction,
            })
            .cmp(&other.key()),
        )
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
        let node = self
            .sibling_set
            .find(&FindChildless(id))
            .map_err(|_| BadId)?
            .node();
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
        node: Node<I>,
        insertion: Insertion<I>,
    ) -> Result<StaticNode<I>, BadId> {
        let sibling = match self.sibling_set.find(&insertion) {
            Ok(_) => Err(BadId),
            Err(s) => Ok(s),
        }?;

        let node = self.alloc_node(node);
        self.sibling_set
            .insert(SiblingSetNode::new(node, SiblingSetNodeKind::Childless))
            .map_err(|_| BadId)?;

        let nodes = [PosMapNodeKind::Normal, PosMapNodeKind::Marker]
            .map(|kind| PosMapNode::new(node, kind));
        let node_as_sibling =
            SiblingSetNode::new(node, SiblingSetNodeKind::Normal);

        let get_neighbor = |sibling: SiblingSetNode<I>| {
            PosMapNode::new(
                sibling.node(),
                match sibling.kind() {
                    SiblingSetNodeKind::Normal => PosMapNodeKind::Marker,
                    SiblingSetNodeKind::Childless => PosMapNodeKind::Normal,
                },
            )
        };

        if let Some(sibling) = sibling {
            self.sibling_set.insert_after(sibling, node_as_sibling);
            match insertion.direction {
                Direction::After => {
                    self.pos_map
                        .insert_after_from(get_neighbor(sibling), nodes);
                }
                Direction::Before => {
                    self.pos_map.insert_before_from(
                        get_neighbor(sibling),
                        nodes.into_iter().rev(),
                    );
                }
            }
        } else {
            debug_assert!(node.parent.is_none());
            self.sibling_set.insert_at_start(node_as_sibling);
            self.pos_map.insert_at_start_from(nodes);
        }
        Ok(node)
    }

    pub fn remote_insert(
        &mut self,
        insertion: Insertion<I>,
    ) -> Result<usize, BadId> {
        let node = self
            .remote_insert_node(Node::from_insertion(&insertion), insertion)?;
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
        let node = self
            .sibling_set
            .find(&FindChildless(id))
            .map_err(|_| BadId)?
            .node();
        let pos_node = PosMapNode::new(node, PosMapNodeKind::Normal);
        let index = self.pos_map.position(pos_node).0;
        self.pos_map.update(pos_node, || {
            node.set_visibility(Visibility::Hidden);
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

    #[cfg(disable)]
    pub fn remote_move(
        &mut self,
        mv: Move<I>,
    ) -> Result<Option<(usize, usize)>, BadId> {
        let old = self
            .sibling_set
            .find(&FindChildless(mv.old))
            .map_err(|_| BadId)?
            .node();
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
                node.other_location.with_mut(|ol| ol.set(head));
                head = Some(node);
            }
        }
        while let Some(node) = head {
            let next = node.other_location.get().get();
            unsafe { dealloc_value(node.ptr(), &self.node_alloc) };
            head = next;
        }
    }
}
