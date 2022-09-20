#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(feature = "allocator_api", feature(allocator_api))]
#![cfg_attr(feature = "rustdoc_internals", feature(rustdoc_internals))]
#![deny(unsafe_op_in_unsafe_fn)]

#[cfg(not(any(feature = "allocator_api", feature = "allocator-fallback")))]
compile_error!("allocator_api or allocator-fallback must be enabled");

use core::borrow::Borrow;
use core::cmp::Ordering;
use core::fmt::{self, Debug, Display};
use core::mem::ManuallyDrop;
use core::num;
use core::ptr::NonNull;
use core_alloc::alloc::Layout;
use core_alloc::collections::BTreeSet;
use core_alloc::rc::Rc;
use core_alloc::sync::Arc;
use core_alloc::vec::Vec;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use skip_list::{LeafRef, SkipList};

extern crate alloc as core_alloc;
#[cfg(feature = "allocator-fallback")]
extern crate allocator_fallback_crate as allocator_fallback;

mod align;
pub mod alloc;
#[cfg(eips_debug)]
pub mod debug;
mod node;
mod pos_map;
mod sibling_set;

use alloc::{Allocator, Allocators, GlobalAllocators};
pub use node::{Direction, Visibility};
use node::{Node, StaticNode};
use pos_map::{PosMapNode, PosMapNodeKind};
use sibling_set::{SiblingSetKey, SiblingSetNode, SiblingSetNodeKind};

pub trait Id: Clone + Ord {
    const FANOUT: usize = 4;
}

impl Id for num::NonZeroU8 {}
impl Id for num::NonZeroU16 {}
impl Id for num::NonZeroU32 {}
impl Id for num::NonZeroU64 {}
impl Id for num::NonZeroU128 {}
impl Id for num::NonZeroUsize {}

impl Id for num::NonZeroI8 {}
impl Id for num::NonZeroI16 {}
impl Id for num::NonZeroI32 {}
impl Id for num::NonZeroI64 {}
impl Id for num::NonZeroI128 {}
impl Id for num::NonZeroIsize {}

impl<T: ?Sized> Id for NonNull<T> {}
impl<T: Ord + ?Sized> Id for Rc<T> {}
impl<T: Ord + ?Sized> Id for Arc<T> {}
impl<T: Clone + Ord, const N: usize> Id for [T; N] {}

macro_rules! impl_id_for_tuple {
    (($($param:ident)*)) => {};

    (($($param:ident)*) $next:ident $($rest:ident)*) => {
        impl<$($param,)* $next> Id for ($($param,)* $next,)
        where
            $($param: Clone + Ord,)*
            $next: Clone + Ord,
        {
        }

        impl_id_for_tuple! {
            ($($param)* $next) $($rest)*
        }
    };
}

#[cfg_attr(
    feature = "rustdoc_internals",
    doc = "This trait is implemented for tuples up to twelve items long.",
    doc(fake_variadic)
)]
impl<T> Id for (T,) where T: Clone + Ord {}

#[cfg(not(all(feature = "rustdoc_internals", doc)))]
impl_id_for_tuple! {
    (T1) T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Slot<I> {
    pub id: I,
    pub parent: Option<I>,
    pub direction: Direction,
    pub visibility: Visibility,
    pub move_timestamp: usize,
    pub other_location: Option<I>,
}

impl<I> Slot<I> {
    fn from_insertion(insertion: Insertion<I>) -> Self {
        Self {
            id: insertion.id,
            parent: insertion.parent,
            direction: insertion.direction,
            visibility: Visibility::Visible,
            move_timestamp: 0,
            other_location: None,
        }
    }
}

impl<I: Id> Slot<I> {
    fn key(&self) -> SiblingSetKey<I> {
        SiblingSetKey::Normal {
            parent: self.parent.clone(),
            direction: self.direction,
            child: self.id.clone(),
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Insertion<I> {
    pub id: I,
    pub parent: Option<I>,
    pub direction: Direction,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug)]
pub struct Move<I> {
    pub insertion: Insertion<I>,
    pub old: I,
    pub timestamp: usize,
}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IndexError {
    pub index: usize,
}

impl Display for IndexError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "bad index: {}", self.index)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for IndexError {}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IdError<I> {
    pub id: I,
}

impl<I: Display> Display for IdError<I> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "bad id: {}", self.id)
    }
}

#[cfg(feature = "std")]
impl<I: Debug + Display> std::error::Error for IdError<I> {}

struct AllocatedNode<I> {
    pub node: StaticNode<I>,
    pub insertion_sibling: Option<SiblingSetNode<I>>,
    pub insertion_neighbor: Option<SiblingSetNode<I>>,
}

#[non_exhaustive]
enum AllocationError<I> {
    BadParentId(I),
    BadOtherLocation(I),
}

#[non_exhaustive]
enum CausalSlotApplied {
    Inserted(usize),
    Hidden,
    AlreadyPresent,
}

#[non_exhaustive]
enum CausalSlotError<I> {
    BadParentId(I),
    BadOtherLocation(I),
}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ApplySlotError<I> {
    MissingItem(usize),
    BadParentId(I),
    BadOtherLocation(I),
}

impl<I: Display> Display for ApplySlotError<I> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingItem(i) => write!(fmt, "missing item at index {i}"),
            Self::BadParentId(id) => write!(fmt, "bad parent id: {id}"),
            Self::BadOtherLocation(id) => {
                write!(fmt, "bad other location: {id}")
            }
        }
    }
}

#[cfg(feature = "std")]
impl<I: Debug + Display> std::error::Error for ApplySlotError<I> {}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RemoteInsertError<I> {
    BadParentId(I),
}

impl<I: Display> Display for RemoteInsertError<I> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadParentId(id) => write!(fmt, "bad parent id: {id}"),
        }
    }
}

#[cfg(feature = "std")]
impl<I: Debug + Display> std::error::Error for RemoteInsertError<I> {}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RemoteRemoveError<I> {
    BadId(I),
}

impl<I: Display> Display for RemoteRemoveError<I> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadId(id) => write!(fmt, "bad id: {id}"),
        }
    }
}

#[cfg(feature = "std")]
impl<I: Debug + Display> std::error::Error for RemoteRemoveError<I> {}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RemoteMoveError<I> {
    BadParentId(I),
    BadOldId(I),
}

impl<I: Display> Display for RemoteMoveError<I> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadParentId(id) => write!(fmt, "bad parent id: {id}"),
            Self::BadOldId(id) => write!(fmt, "bad old id: {id}"),
        }
    }
}

#[cfg(feature = "std")]
impl<I: Debug + Display> std::error::Error for RemoteMoveError<I> {}

#[must_use]
pub struct Slots<'a, I: Id, S> {
    pos_iter: skip_list::Iter<'a, PosMapNode<I>>,
    items: S,
}

impl<'a, I, S> Iterator for Slots<'a, I, S>
where
    I: Id,
    S: Iterator,
{
    type Item = (Slot<I>, Option<S::Item>);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self
            .pos_iter
            .by_ref()
            .find(|node| node.kind() == PosMapNodeKind::Normal)?;
        let item = (node.size() > 0).then(|| self.items.next()).flatten();
        Some((node.node().to_slot(), item))
    }
}

#[derive(Clone, Copy, Debug)]
struct CausalSlotWrapper<I, T> {
    pub slot: Slot<I>,
    pub item: Option<T>,
}

impl<I, T> CausalSlotWrapper<I, T> {
    fn key(&self) -> (&Option<I>, &I) {
        (&self.slot.parent, &self.slot.id)
    }
}

impl<I: Ord, T> Ord for CausalSlotWrapper<I, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key().cmp(&other.key())
    }
}

impl<I: Ord, T> PartialOrd for CausalSlotWrapper<I, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<I: Ord, T> Eq for CausalSlotWrapper<I, T> {}

impl<I: Ord, T> PartialEq for CausalSlotWrapper<I, T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl<I, T> Borrow<Option<I>> for CausalSlotWrapper<I, T> {
    fn borrow(&self) -> &Option<I> {
        &self.slot.parent
    }
}

pub struct NewItems<'a, I, A, T>
where
    I: Id,
    A: Allocators,
{
    eips: &'a mut Eips<I, A>,
    slots: &'a mut BTreeSet<CausalSlotWrapper<I, T>>,
    stack: Vec<(Slot<I>, Option<T>)>,
}

impl<'a, I, A, T> Iterator for NewItems<'a, I, A, T>
where
    I: Id,
    A: Allocators,
{
    type Item = Result<(usize, T), ApplySlotError<I>>;

    fn next(&mut self) -> Option<Self::Item> {
        use ApplySlotError as Error;
        use CausalSlotApplied as Applied;
        use CausalSlotError as CausalError;

        loop {
            let (top, item) = self.stack.pop()?;
            let id = Some(top.id.clone());
            while let Some(wrapper) = self.slots.take(&id) {
                self.stack.push((wrapper.slot, wrapper.item));
            }

            let result = self.eips.apply_causal_slot(top);
            let result = result.map_err(|e| match e {
                CausalError::BadParentId(id) => Error::BadParentId(id),
                CausalError::BadOtherLocation(id) => {
                    Error::BadOtherLocation(id)
                }
            });
            let result = result.and_then(|a| match (a, item) {
                (Applied::Inserted(i), Some(item)) => Ok(Some((i, item))),
                (Applied::Inserted(i), None) => Err(Error::MissingItem(i)),
                _ => Ok(None),
            });

            let result = result.transpose();
            if result.is_some() {
                return result;
            }
        }
    }
}

pub struct ApplySlotState<I, T> {
    slots: BTreeSet<CausalSlotWrapper<I, T>>,
}

impl<I, T> ApplySlotState<I, T> {
    pub fn new() -> Self {
        Self {
            slots: BTreeSet::new(),
        }
    }
}

impl<I, T> Default for ApplySlotState<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Eips<I, A = GlobalAllocators>
where
    I: Id,
    A: Allocators,
{
    node_alloc: A::Alloc1,
    pos_map: ManuallyDrop<SkipList<PosMapNode<I>, A::Alloc2>>,
    sibling_set: ManuallyDrop<SkipList<SiblingSetNode<I>, A::Alloc3>>,
}

impl<I: Id> Eips<I, GlobalAllocators> {
    pub fn new() -> Self {
        Self::new_in(GlobalAllocators)
    }
}

impl<I, A> Eips<I, A>
where
    I: Id,
    A: Allocators,
{
    pub fn new_in(allocators: A) -> Self {
        let allocators = allocators.into_allocators();
        Self {
            node_alloc: allocators.0,
            pos_map: ManuallyDrop::new(SkipList::new_in(allocators.1)),
            sibling_set: ManuallyDrop::new(SkipList::new_in(allocators.2)),
        }
    }

    fn allocate_raw(&self, node: Node<I>) -> StaticNode<I> {
        let mut ptr: NonNull<Node<I>> = self
            .node_alloc
            .allocate(Layout::for_value(&node))
            .expect("memory allocation failed")
            .cast();
        unsafe {
            ptr.as_ptr().write(node);
            StaticNode::new(ptr.as_mut())
        }
    }

    fn allocate(
        &mut self,
        slot: Slot<I>,
    ) -> Result<Option<AllocatedNode<I>>, AllocationError<I>> {
        use AllocationError as Error;
        let sibling =
            match self.sibling_set.find_with(&slot.key(), |n| n.key()) {
                Err(s) => s,
                Ok(_) => return Ok(None),
            };

        let neighbor = match slot.direction {
            Direction::After => sibling,
            Direction::Before => match sibling {
                Some(s) => Some(SkipList::next(s).unwrap()),
                None => self.sibling_set.first(),
            },
        };

        let parent = neighbor.and_then(|n| match n.kind() {
            SiblingSetNodeKind::Normal => n.node().parent(),
            SiblingSetNodeKind::Childless => Some(n.node().id()),
        });

        if parent != slot.parent {
            return Err(Error::BadParentId(slot.parent.expect(
                "insertion neighbor should always match for parentless nodes",
            )));
        }

        let other_location = slot
            .other_location
            .as_ref()
            .map(|id| {
                let key = SiblingSetKey::Childless(id.clone());
                self.sibling_set
                    .find_with(&key, SiblingSetNode::key)
                    .map_err(|_| Error::BadOtherLocation(id.clone()))
            })
            .transpose()?
            .map(|ol| ol.node());

        let node = self.allocate_raw(Node::from(slot));
        node.other_location.with_mut(|ol| ol.set(other_location));
        Ok(Some(AllocatedNode {
            node,
            insertion_sibling: sibling,
            insertion_neighbor: neighbor,
        }))
    }

    pub fn local_get(&self, index: usize) -> Result<I, IndexError> {
        Ok(self
            .pos_map
            .get(&index)
            .ok_or(IndexError {
                index,
            })?
            .node()
            .id())
    }

    pub fn remote_get(&self, id: I) -> Result<usize, IdError<I>> {
        let key = SiblingSetKey::Childless(id.clone());
        let node = self
            .sibling_set
            .find_with(&key, SiblingSetNode::key)
            .map_err(|_| IdError {
                id,
            })?
            .node();
        Ok(self
            .pos_map
            .position(PosMapNode::new(node, PosMapNodeKind::Normal)))
    }

    fn remote_insert_node(&mut self, node: AllocatedNode<I>) {
        self.sibling_set
            .insert(SiblingSetNode::new(
                node.node,
                SiblingSetNodeKind::Childless,
            ))
            .ok()
            .unwrap();

        self.sibling_set.insert_after_opt(
            node.insertion_sibling,
            SiblingSetNode::new(node.node, SiblingSetNodeKind::Normal),
        );

        let neighbor = node.insertion_neighbor.map(|n| {
            PosMapNode::new(
                n.node(),
                match n.kind() {
                    SiblingSetNodeKind::Normal => PosMapNodeKind::Marker,
                    SiblingSetNodeKind::Childless => PosMapNodeKind::Normal,
                },
            )
        });

        let pos_nodes = [PosMapNodeKind::Normal, PosMapNodeKind::Marker]
            .into_iter()
            .map(|kind| PosMapNode::new(node.node, kind));

        match node.node.direction() {
            Direction::After => {
                self.pos_map.insert_after_opt_from(neighbor, pos_nodes);
            }
            Direction::Before => {
                self.pos_map.insert_before_opt_from(neighbor, pos_nodes.rev());
            }
        }
    }

    fn apply_causal_slot(
        &mut self,
        slot: Slot<I>,
    ) -> Result<CausalSlotApplied, CausalSlotError<I>> {
        use CausalSlotApplied as Applied;
        use CausalSlotError as Error;

        let node = self.allocate(slot).map_err(|e| match e {
            AllocationError::BadParentId(id) => Error::BadParentId(id),
            AllocationError::BadOtherLocation(id) => {
                Error::BadOtherLocation(id)
            }
        })?;

        let node = if let Some(node) = node {
            node
        } else {
            return Ok(Applied::AlreadyPresent);
        };

        let static_node = node.node;
        self.remote_insert_node(node);
        if static_node.visibility() == Visibility::Hidden {
            return Ok(Applied::Hidden);
        }

        let pos = self
            .pos_map
            .position(PosMapNode::new(static_node, PosMapNodeKind::Normal));
        Ok(Applied::Inserted(pos))
    }

    pub fn slots<S>(&self, items: S) -> Slots<'_, I, S::IntoIter>
    where
        S: IntoIterator,
    {
        Slots {
            pos_iter: self.pos_map.iter(),
            items: items.into_iter(),
        }
    }

    pub fn apply_slot<'a, T>(
        &'a mut self,
        slot: Slot<I>,
        item: Option<T>,
        state: &'a mut ApplySlotState<I, T>,
    ) -> NewItems<'a, I, A, T> {
        let present = slot
            .parent
            .clone()
            .map(SiblingSetKey::Childless)
            .map(|key| self.sibling_set.find_with(&key, SiblingSetNode::key))
            .map_or(true, |result| result.is_ok());

        let mut stack = Vec::new();
        if present {
            stack.push((slot, item));
        } else {
            state.slots.insert(CausalSlotWrapper {
                slot,
                item,
            });
        }

        NewItems {
            eips: self,
            slots: &mut state.slots,
            stack,
        }
    }

    pub fn local_insert(
        &mut self,
        index: usize,
        id: I,
    ) -> Result<Insertion<I>, IndexError> {
        Ok(if let Some(index) = index.checked_sub(1) {
            let pos_node = self.pos_map.get(&index).ok_or(IndexError {
                index,
            })?;
            let node = pos_node.node();
            let child = SkipList::next(SiblingSetNode::new(
                node,
                SiblingSetNodeKind::Childless,
            ))
            .map(|n| n.node());

            if child.and_then(|c| c.parent()).as_ref() == Some(&node.id()) {
                let next = SkipList::next(pos_node).unwrap().node();
                Insertion {
                    id,
                    parent: Some(next.id()),
                    direction: Direction::Before,
                }
            } else {
                Insertion {
                    id,
                    parent: Some(node.id()),
                    direction: Direction::After,
                }
            }
        } else if let Some(node) = self.pos_map.get(&0).map(|n| n.node()) {
            Insertion {
                id,
                parent: Some(node.id()),
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

    pub fn remote_insert(
        &mut self,
        insertion: Insertion<I>,
    ) -> Result<Option<usize>, RemoteInsertError<I>> {
        use CausalSlotApplied as Applied;
        use CausalSlotError as SlotError;
        use RemoteInsertError as Error;

        self.apply_causal_slot(Slot::from_insertion(insertion))
            .map(|a| match a {
                Applied::Inserted(i) => Some(i),
                Applied::Hidden => unreachable!(),
                Applied::AlreadyPresent => None,
            })
            .map_err(|e| match e {
                SlotError::BadParentId(id) => Error::BadParentId(id),
                SlotError::BadOtherLocation(_) => unreachable!(),
            })
    }

    pub fn local_remove(&mut self, index: usize) -> Result<I, IndexError> {
        Ok(self
            .pos_map
            .get(&index)
            .ok_or(IndexError {
                index,
            })?
            .node()
            .id())
    }

    pub fn remote_remove(
        &mut self,
        id: I,
    ) -> Result<Option<usize>, RemoteRemoveError<I>> {
        let key = SiblingSetKey::Childless(id.clone());
        let node = self
            .sibling_set
            .find_with(&key, SiblingSetNode::key)
            .map_err(|_| RemoteRemoveError::BadId(id))?
            .node();
        if node.visibility() == Visibility::Hidden {
            return Ok(None);
        }
        let pos_node = PosMapNode::new(node, PosMapNodeKind::Normal);
        let index = self.pos_map.position(pos_node);
        SkipList::update(pos_node, || {
            node.set_visibility(Visibility::Hidden);
        });
        Ok(Some(index))
    }

    pub fn local_move(
        &mut self,
        old: usize,
        new: usize,
        id: I,
    ) -> Result<Move<I>, IndexError> {
        let node = self
            .pos_map
            .get(&old)
            .ok_or(IndexError {
                index: old,
            })?
            .node();
        Ok(Move {
            insertion: self.local_insert(new + (new > old) as usize, id)?,
            old: node.id(),
            timestamp: node.move_timestamp.get() + 1,
        })
    }

    pub fn remote_move(
        &mut self,
        mv: Move<I>,
    ) -> Result<Option<(usize, usize)>, RemoteMoveError<I>> {
        use RemoteMoveError as Error;
        let slot = Slot::from_insertion(mv.insertion);
        let node = self.allocate(slot).map_err(|e| match e {
            AllocationError::BadParentId(id) => Error::BadParentId(id),
            AllocationError::BadOtherLocation(_) => unreachable!(),
        })?;

        let mut node = if let Some(node) = node {
            node
        } else {
            return Ok(None);
        };

        let key = SiblingSetKey::Childless(mv.old.clone());
        let old = self
            .sibling_set
            .find_with(&key, SiblingSetNode::key)
            .map_err(|_| Error::BadOldId(mv.old))?
            .node();

        let old = old.other_location().unwrap_or(old);
        debug_assert!(old.other_location().is_none());
        let new = node.node;

        let old_timestamp = (old.move_timestamp.get(), old.id());
        let new_timestamp = (mv.timestamp, new.id());

        // `new` will always be the moved-from node because we need the memory
        // location of the target node (i.e., the possibly visible node to be
        // moved) to remain constant, so `new` should be hidden and point to
        // `old`. The question is whether we should replace its current ID with
        // that of `old` (i.e., actually perform the move because `mv`'s
        // timestamp is newer than `old`'s), or keep the ID because no move is
        // necessary.
        new.set_visibility(Visibility::Hidden);
        new.other_location.with_mut(|ol| ol.set(Some(old)));

        Ok(if old_timestamp < new_timestamp {
            let old_pos = PosMapNode::new(old, PosMapNodeKind::Normal);
            let new_pos = PosMapNode::new(new, PosMapNodeKind::Normal);
            let old_index = (old.visibility() == Visibility::Visible)
                .then(|| self.pos_map.position(old_pos));

            self.pos_map.replace(old_pos, new_pos);
            self.pos_map.replace(
                PosMapNode::new(old, PosMapNodeKind::Marker),
                PosMapNode::new(new, PosMapNodeKind::Marker),
            );

            self.sibling_set.replace(
                SiblingSetNode::new(old, SiblingSetNodeKind::Normal),
                SiblingSetNode::new(new, SiblingSetNodeKind::Normal),
            );
            self.sibling_set.replace(
                SiblingSetNode::new(old, SiblingSetNodeKind::Childless),
                SiblingSetNode::new(new, SiblingSetNodeKind::Childless),
            );

            // Swap IDs (and directions) between `new` and `old`.
            new.swap_id(&old);

            // At this point, `new` is basically what `old` was, except it's
            // hidden and points to the real `old` (via `other_location`).
            // Because it has `old`'s (former) ID, we know it's in the right
            // spot in `pos_map` and `sibling_set` after the calls to
            // `SkipList::replace` above.
            //
            // Similarly, `old`, despite its name, now conceptually represents
            // the *new* node, as it has the new ID from `mv`. However, we
            // still need to update the timestamp and re-insert it into the
            // tree (due to the new ID and the fact that the `replace`
            // operations above removed it).
            old.move_timestamp.set(mv.timestamp);

            node.node = old;
            self.remote_insert_node(node);
            old_index.map(|i| (i, self.pos_map.position(old_pos)))
        } else {
            debug_assert!(old_timestamp != new_timestamp);
            self.remote_insert_node(node);
            None
        })
    }
}

impl<I: Id> Default for Eips<I, GlobalAllocators> {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl<I, A> Send for Eips<I, A>
where
    I: Id + Send,
    A: Allocators + Send,
{
}

unsafe impl<I, A> Sync for Eips<I, A>
where
    I: Id + Sync,
    A: Allocators + Sync,
{
}

impl<I, A> Drop for Eips<I, A>
where
    I: Id,
    A: Allocators,
{
    fn drop(&mut self) {
        let mut head = None;
        unsafe {
            ManuallyDrop::drop(&mut self.pos_map);
        }
        for node in unsafe { ManuallyDrop::take(&mut self.sibling_set) } {
            if node.kind() == SiblingSetNodeKind::Childless {
                let node = node.node();
                node.other_location.with_mut(|ol| ol.set(head));
                head = Some(node);
            }
        }
        while let Some(node) = head {
            let next = node.other_location();
            let ptr = node.ptr();
            // This also drops the node.
            let layout = Layout::for_value(&unsafe { ptr.as_ptr().read() });
            unsafe {
                self.node_alloc.deallocate(ptr.cast(), layout);
            }
            head = next;
        }
    }
}
