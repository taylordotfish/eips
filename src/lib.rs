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

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(feature = "allocator_api", feature(allocator_api))]
#![cfg_attr(feature = "doc_cfg", feature(doc_cfg))]
#![deny(unsafe_op_in_unsafe_fn)]
// crate doc:
#![doc = include_str!("common-readme.md")]
//!
//! [btree-vec]: https://docs.rs/btree-vec
#![cfg_attr(
    not(feature = "serde"),
    doc = "
[serde]: https://docs.rs/serde/1/serde/
[`Serialize`]: https://docs.rs/serde/1/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/1/serde/trait.Deserialize.html
"
)]

#[cfg(not(any(feature = "allocator_api", feature = "allocator-fallback")))]
compile_error!("allocator_api or allocator-fallback must be enabled");

use alloc::alloc::Layout;
use core::mem::{self, ManuallyDrop};
use core::ptr::NonNull;
use fixed_bump::DynamicBump;
use fixed_typed_arena::manually_drop::ManuallyDropArena;
#[cfg(doc)]
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use skippy::{AllocItem, SkipList};

extern crate alloc;

pub mod changes;
#[cfg(all(eips_debug, feature = "std"))]
pub mod debug;
pub mod error;
pub mod iter;
mod node;
pub mod options;
mod pos_map;
mod sibling_set;

pub use changes::{LocalChange, RemoteChange};
use error::{ChangeError, IdError, IndexError};
use iter::Changes;
use node::{Direction, Node, StaticNode, Visibility};
use options::NodeAllocOptions;
pub use options::{EipsOptions, Options};
use pos_map::{PosMapNode, PosMapNodeKind};
use sibling_set::{SiblingSetKey, SiblingSetNode, SiblingSetNodeKind};

/// The trait that ID types must implement.
///
/// This is effectively an alias of <code>[Clone] + [Ord]</code>; it is
/// automatically implemented for all types that implement those traits.
///
/// Although not strictly required, ID types should be small in size and cheap
/// to clone ([`Copy`] is ideal), and
/// <code>[size_of]::<[Option]\<[Self]>>()</code> should be the same as
/// <code>[size_of]::\<[Self]>()</code>. This can be achieved by, e.g.,
/// including a [`NonZeroUsize`] in your ID type.
///
/// [size_of]: core::mem::size_of
/// [`NonZeroUsize`]: core::num::NonZeroUsize
pub trait Id: Clone + Ord {}

impl<T: Clone + Ord> Id for T {}

struct ValidatedNode<Id, Opt> {
    pub node: Node<Id, Opt>,
    pub insertion_sibling: Option<SiblingSetNode<Id, Opt>>,
    pub insertion_neighbor: Option<SiblingSetNode<Id, Opt>>,
}

enum ValidationSuccess<Id, Opt> {
    New(ValidatedNode<Id, Opt>),
    Existing(LocalChange),
}

/// An intention-preserving sequence CRDT.
///
/// `Id` is the ID data type. Each item in an Eips sequence has a unique ID.
/// `Id` must implement [`Clone`] and [`Ord`] and should be small and cheap to
/// clone. Additionally, [`Option<Id>`] should be the same size as `Id`. See
/// the [`Id`] trait for details.
///
/// # Mathematical variables
///
/// The following variables may be used to specify the time and space
/// complexity of various operations and types:
///
/// * *h*, the total number of items ever inserted in the sequence.
/// * *n*, the number of visible (non-deleted) items in the sequence.
///
/// # Space complexity
///
/// Θ(*[h](#mathematical-variables)*). Note that some of the options in `Opt`
/// can scale memory usage by a small constant amount by affecting the amount
/// of auxiliary memory used; see [`EipsOptions`] for details.
pub struct Eips<Id, Opt = Options>
where
    Opt: EipsOptions,
{
    pos_map: ManuallyDrop<SkipList<PosMapNode<Id, Opt>, DynamicBump>>,
    sibling_set: ManuallyDrop<SkipList<SiblingSetNode<Id, Opt>, DynamicBump>>,
    node_alloc: ManuallyDropArena<Node<Id, Opt>, NodeAllocOptions<Id, Opt>>,
}

impl<Id, Opt> Eips<Id, Opt>
where
    Id: self::Id,
    Opt: EipsOptions,
{
    /// Creates a new [`Eips`].
    pub fn new() -> Self {
        let pos_map_layout = Layout::from_size_align(
            mem::size_of::<AllocItem<PosMapNode<Id, Opt>>>()
                .checked_mul(options::chunk_size::<Opt>())
                .unwrap(),
            mem::align_of::<AllocItem<PosMapNode<Id, Opt>>>(),
        )
        .unwrap();

        let sibling_set_layout = Layout::from_size_align(
            mem::size_of::<AllocItem<SiblingSetNode<Id, Opt>>>()
                .checked_mul(options::chunk_size::<Opt>())
                .unwrap(),
            mem::align_of::<AllocItem<SiblingSetNode<Id, Opt>>>(),
        )
        .unwrap();

        let pos_map_bump = DynamicBump::new(pos_map_layout);
        let sibling_set_bump = DynamicBump::new(sibling_set_layout);
        Self {
            pos_map: ManuallyDrop::new(SkipList::new_in(pos_map_bump)),
            sibling_set: ManuallyDrop::new(SkipList::new_in(sibling_set_bump)),
            node_alloc: ManuallyDropArena::new(),
        }
    }

    /// Gets the number of (non-deleted) items in the sequence.
    ///
    /// # Time complexity
    ///
    /// Constant.
    pub fn len(&self) -> usize {
        self.pos_map.size()
    }

    /// Checks if there are no (non-deleted) items in the sequence.
    ///
    /// # Time complexity
    ///
    /// Constant.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn merge(
        &mut self,
        change: RemoteChange<Id>,
        node: StaticNode<Id, Opt>,
    ) -> Result<ValidationSuccess<Id, Opt>, ChangeError<Id>> {
        if change.old_location.as_ref()
            != node.other_location().as_ref().map(|n| &n.id)
            || change.move_timestamp != node.move_timestamp.get()
        {
            return Err(ChangeError::MergeConflict(change.id));
        }

        let newest = node.newest_location();
        if change.visibility >= newest.visibility() {
            return Ok(ValidationSuccess::Existing(
                LocalChange::AlreadyApplied,
            ));
        }

        debug_assert_eq!(change.visibility, Visibility::Hidden);
        let pos_node = PosMapNode::new(newest, PosMapNodeKind::Normal);
        self.pos_map.update(pos_node, || {
            newest.set_visibility(Visibility::Hidden);
        });
        Ok(ValidationSuccess::Existing(LocalChange::Remove(
            self.index(newest),
        )))
    }

    fn validate(
        &mut self,
        mut change: RemoteChange<Id>,
    ) -> Result<ValidationSuccess<Id, Opt>, ChangeError<Id>> {
        use ChangeError as Error;
        if change.old_location.is_some()
            && change.visibility == Visibility::Hidden
        {
            return Err(Error::HiddenMove(change.id));
        }

        let sibling = match self.sibling_set.find_with(&change.key()) {
            Err(s) => s,
            Ok(s) => return self.merge(change, s.node()),
        };

        let neighbor = match change.direction {
            Direction::After => sibling,
            Direction::Before => match sibling {
                Some(s) => Some(SkipList::next(s).unwrap()),
                None => self.sibling_set.first(),
            },
        };

        let parent = neighbor.as_ref().and_then(|n| match n.kind() {
            SiblingSetNodeKind::Normal => n.as_node().parent.as_ref(),
            SiblingSetNodeKind::Childless => Some(&n.as_node().id),
        });

        if parent != change.parent.as_ref() {
            return Err(Error::BadParentId(change.parent.expect(
                "insertion neighbor should always match for parentless nodes",
            )));
        }

        let other_location = if let Some(old) = change.old_location {
            let node = if let Some(node) = self.find(&old) {
                node
            } else {
                return Err(Error::BadOldLocation(old));
            };
            if node.old_location().is_some() {
                return Err(Error::OldLocationIsMove(old));
            }
            change.old_location = Some(old);
            Some(node)
        } else {
            None
        };

        let node = Node::from(change);
        node.other_location.with_mut(|ol| ol.set(other_location));

        Ok(ValidationSuccess::New(ValidatedNode {
            node,
            insertion_sibling: sibling,
            insertion_neighbor: neighbor,
        }))
    }

    fn allocate(&mut self, node: Node<Id, Opt>) -> StaticNode<Id, Opt> {
        unsafe {
            StaticNode::new(NonNull::from(self.node_alloc.alloc_shared(node)))
        }
    }

    fn find_sibling(
        &self,
        id: &Id,
    ) -> Result<SiblingSetNode<Id, Opt>, Option<SiblingSetNode<Id, Opt>>> {
        self.sibling_set.find_with(&SiblingSetKey::Childless(id))
    }

    fn find(&self, id: &Id) -> Option<StaticNode<Id, Opt>> {
        self.find_sibling(id).ok().map(|s| s.node())
    }

    fn index(&self, node: StaticNode<Id, Opt>) -> usize {
        self.pos_map.index(PosMapNode::new(node, PosMapNodeKind::Normal))
    }

    fn get_pos_node(
        &self,
        index: usize,
    ) -> Result<PosMapNode<Id, Opt>, IndexError> {
        self.pos_map
            .get(&index)
            .filter(|n| n.kind() == PosMapNodeKind::Normal)
            .ok_or(IndexError {
                index,
            })
    }

    fn get_oldest_node(
        &self,
        index: usize,
    ) -> Result<StaticNode<Id, Opt>, IndexError> {
        Ok(self.get_pos_node(index)?.node().oldest_location())
    }

    /// Gets the ID of the item at `index`.
    ///
    /// The ID can be turned back into a local index with [`Self::remote_get`].
    ///
    /// # Errors
    ///
    /// Returns an error if `index` is out of bounds.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn get(&self, index: usize) -> Result<Id, IndexError> {
        self.get_oldest_node(index).map(|n| n.id.clone())
    }

    /// Gets the index of the item with ID `id`.
    ///
    /// Returns [`None`] if the item has been deleted.
    ///
    /// # Errors
    ///
    /// Returns an error if there is no item with the given ID.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn remote_get<'a>(
        &self,
        id: &'a Id,
    ) -> Result<Option<usize>, IdError<&'a Id>> {
        let node = self.find(id).ok_or(IdError {
            id,
        })?;
        let node = node.newest_location();
        Ok(match node.visibility() {
            Visibility::Visible => Some(self.index(node)),
            Visibility::Hidden => None,
        })
    }

    /// Returns the old index if a move was performed.
    fn update_move_locations(
        &mut self,
        node: StaticNode<Id, Opt>,
    ) -> Option<usize> {
        debug_assert!(node.new_location().is_none());
        let old = node.old_location()?;
        debug_assert!(node.visibility() == Visibility::Visible);
        let newest = old.newest_location();

        let node_timestamp = (node.move_timestamp.get(), &node.id);
        let newest_timestamp = (newest.move_timestamp.get(), &newest.id);
        debug_assert!(node_timestamp != newest_timestamp);

        if newest.visibility() == Visibility::Hidden
            || node_timestamp < newest_timestamp
        {
            node.set_visibility(Visibility::Hidden);
            node.other_location.with_mut(|ol| ol.set(None));
            return None;
        }

        if let Some(new) = old.new_location() {
            new.other_location.with_mut(|ol| ol.set(None));
        }
        old.other_location.with_mut(|ol| ol.set(Some(node)));

        let pos_newest = PosMapNode::new(newest, PosMapNodeKind::Normal);
        let index = self.index(newest);
        self.pos_map.update(pos_newest, || {
            newest.set_visibility(Visibility::Hidden);
        });
        Some(index)
    }

    fn insert_node(&mut self, node: ValidatedNode<Id, Opt>) -> LocalChange {
        let ValidatedNode {
            node,
            insertion_sibling,
            insertion_neighbor,
        } = node;

        let node = self.allocate(node);
        let old_index = self.update_move_locations(node);

        self.sibling_set
            .insert(SiblingSetNode::new(node, SiblingSetNodeKind::Childless))
            .ok()
            .unwrap();

        self.sibling_set.insert_after_opt(
            insertion_sibling,
            SiblingSetNode::new(node, SiblingSetNodeKind::Normal),
        );

        let neighbor = insertion_neighbor.map(|n| {
            PosMapNode::new(n.node(), match n.kind() {
                SiblingSetNodeKind::Normal => PosMapNodeKind::Marker,
                SiblingSetNodeKind::Childless => PosMapNodeKind::Normal,
            })
        });

        let pos_nodes = [PosMapNodeKind::Normal, PosMapNodeKind::Marker]
            .into_iter()
            .map(|kind| PosMapNode::new(node, kind));

        match node.direction() {
            Direction::After => {
                self.pos_map.insert_after_opt_from(neighbor, pos_nodes);
            }
            Direction::Before => {
                self.pos_map.insert_before_opt_from(neighbor, pos_nodes.rev());
            }
        }

        if let Some(old) = old_index {
            LocalChange::Move {
                old,
                new: self.index(node),
            }
        } else if node.visibility() == Visibility::Hidden {
            LocalChange::None
        } else {
            LocalChange::Insert(self.index(node))
        }
    }

    /// Inserts an item at index `index`.
    ///
    /// The item's index will be `index` after the change is applied. `id` is
    /// the ID the item will have. It must be unique.
    ///
    /// # Errors
    ///
    /// Returns an error if `index` is out of bounds.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn insert(
        &mut self,
        index: usize,
        id: Id,
    ) -> Result<RemoteChange<Id>, IndexError> {
        let parent;
        let direction;

        if let Some(index) = index.checked_sub(1) {
            let pos_node = self.get_pos_node(index)?;
            let node = pos_node.node();
            // Possibly a child of `node`.
            let next = SkipList::next(SiblingSetNode::new(
                node,
                SiblingSetNodeKind::Childless,
            ));

            if match next {
                None => false,
                Some(n) if n.kind() == SiblingSetNodeKind::Childless => false,
                Some(n) if n.node().direction() == Direction::Before => {
                    debug_assert!(n.node().parent.as_ref() != Some(&node.id));
                    false
                }
                Some(n) => {
                    if let Some(parent) = &n.node().parent {
                        debug_assert!(parent == &node.id);
                    }
                    true
                }
            } {
                let next = SkipList::next(pos_node).unwrap().node();
                parent = Some(next.id.clone());
                direction = Direction::Before;
            } else {
                parent = Some(node.id.clone());
                direction = Direction::After;
            }
        } else if let Ok(node) = self.get_pos_node(0).map(|n| n.node()) {
            parent = Some(node.id.clone());
            direction = Direction::Before;
        } else {
            parent = None;
            direction = Direction::After;
        }

        Ok(RemoteChange {
            id,
            parent,
            direction,
            visibility: Visibility::Visible,
            move_timestamp: 0,
            old_location: None,
        })
    }

    /// Removes the item at index `index`.
    ///
    /// # Errors
    ///
    /// Returns an error if `index` is out of bounds.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn remove(
        &mut self,
        index: usize,
    ) -> Result<RemoteChange<Id>, IndexError> {
        let node = self.get_oldest_node(index)?;
        Ok(RemoteChange {
            id: node.id.clone(),
            parent: node.parent.clone(),
            direction: node.direction(),
            visibility: Visibility::Hidden,
            move_timestamp: node.move_timestamp.get(),
            old_location: None,
        })
    }

    /// Moves the item at index `old` to index `new`.
    ///
    /// The item will be at index `new` once the change is applied.
    ///
    /// # Errors
    ///
    /// Returns an error if `old` or `new` are out of bounds.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn mv(
        &mut self,
        old: usize,
        new: usize,
        id: Id,
    ) -> Result<RemoteChange<Id>, IndexError> {
        let node = self
            .pos_map
            .get(&old)
            .ok_or(IndexError {
                index: old,
            })?
            .node();
        let oldest = node.oldest_location();
        let newest = node.newest_location();
        let mut change = self.insert(new + (new > old) as usize, id)?;
        change.old_location = Some(oldest.id.clone());
        change.move_timestamp = newest.move_timestamp.get() + 1;
        Ok(change)
    }

    /// Applies a remote change generated by methods like [`Self::insert`] and
    /// [`Self::remove`].
    ///
    /// Returns the change that should be made to the corresponding local
    /// sequence of items.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn apply_change(
        &mut self,
        change: RemoteChange<Id>,
    ) -> Result<LocalChange, ChangeError<Id>> {
        Ok(match self.validate(change)? {
            ValidationSuccess::New(node) => self.insert_node(node),
            ValidationSuccess::Existing(change) => change,
        })
    }

    /// Returns all items in the sequence as [`RemoteChange`]s.
    ///
    /// This method returns an iterator that yields `(change, index)` tuples,
    /// where `change` is the [`RemoteChange`] and `index` is the local index
    /// corresponding to the change (or [`None`] if the change represents a
    /// deleted item).
    ///
    /// # Time complexity
    ///
    /// Iteration over the entire sequence is Θ(*[h]* + *[n]* log *[h]*). If
    /// Θ(*[h]*) iteration (not dependent on *[n]*) is needed, it is
    /// recommended to maintain a separate list of all [`RemoteChange`]s and
    /// corresponding items, at the cost of using more memory.
    ///
    /// [h]: #mathematical-variables
    /// [n]: #mathematical-variables
    pub fn changes(&self) -> Changes<'_, Id, Opt> {
        Changes {
            nodes: self.node_alloc.iter(),
            eips: self,
        }
    }

    /// Gets the item with ID `id` as a [`RemoteChange`].
    ///
    /// Returns a `(change, index)` tuple, where `change` is the
    /// [`RemoteChange`] and `index` is the local index corresponding to the
    /// change (or [`None`] if the change represents a deleted item).
    ///
    /// # Errors
    ///
    /// Returns an error if there is no item with the given ID.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn get_change<'a>(
        &self,
        id: &'a Id,
    ) -> Result<(RemoteChange<Id>, Option<usize>), IdError<&'a Id>> {
        let node = self.find(id).ok_or(IdError {
            id,
        })?;
        let index = (node.visibility() == Visibility::Visible)
            .then(|| self.index(node));
        Ok((node.to_change(), index))
    }
}

impl<Id, Opt> Drop for Eips<Id, Opt>
where
    Opt: EipsOptions,
{
    fn drop(&mut self) {
        unsafe {
            ManuallyDrop::drop(&mut self.pos_map);
            ManuallyDrop::drop(&mut self.sibling_set);
            self.node_alloc.drop();
        }
    }
}

impl<Id, Opt> Default for Eips<Id, Opt>
where
    Id: self::Id,
    Opt: EipsOptions,
{
    fn default() -> Self {
        Self::new()
    }
}

// We don't provide references or pointers to data that aren't bound to the
// life of `self` with standard borrowing rules, so if we have ownership of
// this type to send to another thread, we know no other thread can possibly
// access internal data (including skip list nodes) concurrently.
unsafe impl<Id, Opt> Send for Eips<Id, Opt>
where
    Id: Send,
    Opt: EipsOptions,
{
}

// This type has no `&self` methods that mutate data, so if shared references
// are sent to other threads, borrowing rules guarantee that no thread will
// mutate any data (including skip list nodes). There may be concurrent reads,
// which is fine.
unsafe impl<Id, Opt> Sync for Eips<Id, Opt>
where
    Id: Sync,
    Opt: EipsOptions,
{
}

impl<Id, Opt> Unpin for Eips<Id, Opt> where Opt: EipsOptions {}
