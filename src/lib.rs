/*
 * Copyright (C) 2025 taylor.fish <contact@taylor.fish>
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
#![warn(clippy::missing_safety_doc)]
// crate doc:
#![doc = include_str!("lib.md")]
#![cfg_attr(
    not(feature = "serde"),
    doc = "[serde]: https://docs.rs/serde/1/serde/
[`Serialize`]: https://docs.rs/serde/1/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/1/serde/trait.Deserialize.html"
)]

#[cfg(not(any(feature = "allocator_api", feature = "allocator-fallback")))]
compile_error!("allocator_api or allocator-fallback must be enabled");

extern crate alloc;

use alloc::alloc::Layout;
#[cfg(all(doc, not(feature = "std")))]
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::mem::{self, ManuallyDrop};
use core::ptr::NonNull;
use fixed_bump::DynamicBump;
use fixed_typed_arena::manually_drop::ManuallyDropArena;
use integral_constant::Bool;
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use skippy::{AllocItem, SkipList};

pub mod change;
#[cfg(all(eips_debug, feature = "std"))]
pub mod debug;
pub mod error;
pub mod iter;
mod node;
pub mod options;
mod pos_map;
mod sibling_set;

use change::MoveInfo;
pub use change::{LocalChange, RemoteChange};
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
/// to clone ([`Copy`] is strongly encouraged).
pub trait Id: Clone + Ord {}

impl<T: Clone + Ord> Id for T {}

pub(crate) type MoveTimestamp = u64;
pub(crate) type NonZeroMoveTimestamp = core::num::NonZeroU64;

struct ValidatedNode<Id, Opt: EipsOptions> {
    pub node: Node<Id, Opt>,
    pub insertion_sibling: Option<SiblingSetNode<Id, Opt>>,
    pub insertion_neighbor: Option<SiblingSetNode<Id, Opt>>,
}

enum ValidationSuccess<Id, Opt: EipsOptions> {
    New(ValidatedNode<Id, Opt>),
    Existing(LocalChange),
}

/// An intention-preserving sequence CRDT.
///
/// `Id` is the ID data type. Each item in an Eips sequence has a unique ID.
/// `Id` must implement [`Clone`] and [`Ord`] and should be small and cheap to
/// clone ([`Copy`] is ideal).
///
/// # Mathematical variables
///
/// The following variables may be used to specify the time and space
/// complexity of various operations and types:
///
/// * *h*, the total number of items ever inserted in the sequence.
/// * *n*, the number of visible (non-deleted) items in the sequence.
///
/// Note that [moving](Eips::mv) an element increases *h* by 1, but does not
/// affect *n*.
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

    fn find_sibling(
        &self,
        id: &Id,
    ) -> Result<SiblingSetNode<Id, Opt>, Option<SiblingSetNode<Id, Opt>>> {
        self.sibling_set.find_with(&SiblingSetKey::Parent(id))
    }

    fn find(&self, id: &Id) -> Option<StaticNode<Id, Opt>> {
        self.find_sibling(id).ok().map(|s| s.node())
    }

    fn index(&self, node: StaticNode<Id, Opt>) -> usize {
        SkipList::index(PosMapNode::new(node, PosMapNodeKind::Normal))
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

    /// Returns the index immediately *after* the rightmost descendant of the
    /// subtree rooted at `node`. If `node` does not have a right child, this
    /// method will panic or return an incorrect result.
    fn subtree_right_index(&self, node: StaticNode<Id, Opt>) -> usize {
        // If the node is itself a right child, we can use its marker node.
        if node.direction() == Direction::After {
            return SkipList::index(PosMapNode::new(
                node,
                PosMapNodeKind::Marker,
            ));
        }
        // Otherwise, use the marker node of its last right child.
        let id = &node.id;
        let sib_node = SiblingSetNode::new(node, SiblingSetNodeKind::Parent);
        let result = SkipList::find_after_with_cmp(sib_node, |other| {
            let parent_id = match other.kind() {
                SiblingSetNodeKind::Child => other.as_node().parent(),
                SiblingSetNodeKind::Parent => Some(&other.as_node().id),
            };
            match parent_id.cmp(&Some(id)) {
                Ordering::Equal => Ordering::Less,
                ord => ord,
            }
        });
        let descendant = match result {
            Ok(_) => unreachable!(),
            Err(Some(descendant)) => descendant,
            Err(None) => unreachable!(),
        };
        debug_assert_eq!(
            descendant.kind(),
            SiblingSetNodeKind::Child,
            "node has no right child",
        );
        debug_assert_eq!(descendant.node().direction(), Direction::After);
        SkipList::index(PosMapNode::new(
            descendant.node(),
            PosMapNodeKind::Marker,
        ))
    }

    /// Inserts an item at a particular index.
    ///
    /// The item's index will be `index` once the resulting [`RemoteChange`] is
    /// [applied](Self::apply_change).
    ///
    /// `id` must be a new, unique ID.
    ///
    /// # Errors
    ///
    /// Returns an error if `index` is out of bounds.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn insert(
        &self,
        index: usize,
        id: Id,
    ) -> Result<RemoteChange<Id>, IndexError> {
        let parent;
        let direction;

        if let Some(prev_index) = index.checked_sub(1) {
            let pos_node = self.get_pos_node(prev_index)?;
            let node = pos_node.node();
            let next = if index < self.len() {
                // Possibly a child of `node`.
                SkipList::next(SiblingSetNode::new(
                    node,
                    SiblingSetNodeKind::Parent,
                ))
            } else {
                None
            };

            // Check whether `node` has a visible right descendant.
            if match next {
                None => false,
                Some(n) if n.kind() == SiblingSetNodeKind::Parent => false,
                Some(n) if n.node().direction() == Direction::Before => {
                    debug_assert!(n.node().parent() != Some(&node.id));
                    false
                }
                Some(n) => {
                    debug_assert!(if let Some(parent) = n.node().parent() {
                        *parent == node.id
                    } else {
                        true
                    });
                    let subtree_end = self.subtree_right_index(node);
                    debug_assert!(subtree_end >= index);
                    subtree_end > index
                }
            } {
                // `node` has a visible right descendant; find the first such
                // descendant (the first visible node after `node` in an
                // in-order traversal).
                let next = SkipList::get_after(pos_node, &1)
                    .expect("node N+1 should exist given visible right child")
                    .node();
                parent = Some(next.id.clone());
                direction = Direction::Before;
            } else {
                // `node` doesn't have a visible right descendant; the new node
                // will be inserted as a right child.
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
            raw_parent: parent.unwrap_or_else(|| id.clone()),
            id,
            direction,
            visibility: Visibility::Visible,
            move_info: None,
        })
    }

    /// Removes the item at a particular index.
    ///
    /// No actual modification takes place until the resulting [`RemoteChange`]
    /// is [applied](Self::apply_change).
    ///
    /// # Errors
    ///
    /// Returns an error if `index` is out of bounds.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn remove(
        &self,
        index: usize,
    ) -> Result<RemoteChange<Id>, IndexError> {
        let node = self.get_oldest_node(index)?;
        Ok(RemoteChange {
            id: node.id.clone(),
            raw_parent: node.raw_parent.clone(),
            direction: node.direction(),
            visibility: Visibility::Hidden,
            move_info: None,
        })
    }

    /// Moves an item to a new index.
    ///
    /// The item currently at index `old` will reside at index `new` once the
    /// resulting [`RemoteChange`] is [applied](Self::apply_change).
    ///
    /// `id` must be a new, unique ID.
    ///
    /// # Errors
    ///
    /// Returns an error if `old` or `new` are out of bounds.
    ///
    /// # Time complexity
    ///
    /// Θ(log *[h](#mathematical-variables)*).
    pub fn mv(
        &self,
        old: usize,
        new: usize,
        id: Id,
    ) -> Result<RemoteChange<Id>, IndexError>
    where
        Opt: EipsOptions<SupportsMove = Bool<true>>,
    {
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
        change.move_info = Some(MoveInfo {
            timestamp: (newest.move_timestamp() + 1)
                .try_into()
                .expect("must be non-zero after adding 1"),
            old_location: oldest.id.clone(),
        });
        Ok(change)
    }

    fn merge(
        &mut self,
        change: RemoteChange<Id>,
        node: StaticNode<Id, Opt>,
    ) -> Result<ValidationSuccess<Id, Opt>, ChangeError<Id>> {
        use ChangeError as Error;
        if cfg!(debug_assertions) && change.move_info.is_some() {
            debug_assert!(node.supports_move());
            debug_assert_eq!(change.visibility, Visibility::Visible);
        }

        if node.visibility() == Visibility::Hidden {
            debug_assert!(node.old_location().is_none());
        } else if let Some(mv) = change.move_info {
            if node.other_location().as_ref().map(|n| &n.id)
                != Some(&mv.old_location)
                || mv.timestamp.get() != node.move_timestamp()
            {
                return Err(Error::MergeConflict(change.id));
            }
        } else if let Some(1..) = node.move_timestamp_or_none() {
            return Err(Error::UnexpectedMove(change.id));
        }

        let newest = node.newest_location();
        if change.visibility >= newest.visibility() {
            return Ok(ValidationSuccess::Existing(
                LocalChange::AlreadyApplied,
            ));
        }

        debug_assert_eq!(change.visibility, Visibility::Hidden);
        let pos_newest = PosMapNode::new(newest, PosMapNodeKind::Normal);
        self.pos_map.update(pos_newest, || newest.hide());
        node.clear_move_info();
        if newest.ptr() != node.ptr() {
            newest.clear_move_info();
        }
        Ok(ValidationSuccess::Existing(LocalChange::Remove(
            self.index(newest),
        )))
    }

    fn validate(
        &mut self,
        mut change: RemoteChange<Id>,
    ) -> Result<ValidationSuccess<Id, Opt>, ChangeError<Id>> {
        use ChangeError as Error;
        if change.move_info.is_none() {
        } else if !Node::<Id, Opt>::SUPPORTS_MOVE {
            return Err(Error::UnsupportedMove(change.id));
        } else if change.visibility == Visibility::Hidden {
            return Err(Error::HiddenMove(change.id));
        }
        if change.parent().is_none() && change.direction == Direction::Before {
            return Err(Error::BadDirection(change.id));
        }

        // This won't actually be a true sibling in the following cases (even
        // assuming a valid parent ID):
        //
        // 1. The new node will become the first right child of its parent,
        //    in which case `sibling` will be the parent itself (with
        //    `SiblingSetNodeKind::Parent`), or `None` if the node has no
        //    parent.
        //
        // 2. The new node will become the first left child of its parent,
        //    in which case `sibling` will be an unrelated node that happens
        //    to exist immediately before the position at which the new node
        //    should be inserted in the sibling set (as a sibling set node with
        //    kind `Child`).
        let sibling = match self.sibling_set.find_with(&change.key()) {
            Err(s) => s,
            Ok(s) => return self.merge(change, s.node()),
        };

        // Assuming a valid parent ID, this is definitely either a true sibling
        // or the parent itself.
        let neighbor = match change.direction {
            Direction::After => sibling,
            Direction::Before => match sibling {
                Some(s) => SkipList::next(s),
                None => self.sibling_set.first(),
            },
        };

        let parent = neighbor.as_ref().and_then(|n| match n.kind() {
            SiblingSetNodeKind::Child => n.as_node().parent(),
            SiblingSetNodeKind::Parent => Some(&n.as_node().id),
        });

        let change_parent = change.parent();
        if change_parent != parent {
            debug_assert!(
                change_parent.is_some(),
                "parentless nodes should never cause a parent ID mismatch",
            );
            return Err(Error::BadParentId(change.raw_parent));
        }
        debug_assert!(match change.direction {
            Direction::After => true,
            Direction::Before => neighbor.is_some(),
        });

        let old_location = if let Some(mv) = change.move_info {
            let Some(node) = self.find(&mv.old_location) else {
                return Err(Error::BadOldLocation(mv.old_location));
            };
            if node.old_location().is_some() {
                return Err(Error::OldLocationIsMove(mv.old_location));
            }
            change.move_info = Some(mv);
            Some(node)
        } else {
            None
        };

        let node = Node::try_from(change)?;
        if old_location.is_some() {
            node.set_other_location(old_location);
        }

        Ok(ValidationSuccess::New(ValidatedNode {
            node,
            insertion_sibling: sibling,
            insertion_neighbor: neighbor,
        }))
    }

    fn allocate(&mut self, node: Node<Id, Opt>) -> StaticNode<Id, Opt> {
        let ptr = NonNull::from(self.node_alloc.alloc_shared(node));
        // SAFETY: We don't drop the arena allocator until `self` is dropped,
        // and because we don't provide public access to `StaticNode`s (except
        // possibly as an internal implementation detail of types that borrow
        // `self`, and thus cannot exist when `self` is dropped), we know that
        // the arena won't be dropped while `StaticNode`s exist. Mutable
        // references to the node do not and will not exist (the arena doesn't
        // support them).
        unsafe { StaticNode::new(ptr) }
    }

    /// Returns the old index if a move was performed.
    fn update_move_info(
        &mut self,
        node: StaticNode<Id, Opt>,
    ) -> Option<usize> {
        debug_assert!(node.new_location().is_none());
        let old = node.old_location()?;
        debug_assert!(node.visibility() == Visibility::Visible);
        let newest = old.newest_location();

        let node_timestamp = (node.move_timestamp(), &node.id);
        let newest_timestamp = (newest.move_timestamp(), &newest.id);
        debug_assert!(node_timestamp != newest_timestamp);

        if newest.visibility() == Visibility::Hidden
            || node_timestamp < newest_timestamp
        {
            node.hide();
            node.clear_move_info();
            return None;
        }

        let index = self.index(newest);
        let pos_newest = PosMapNode::new(newest, PosMapNodeKind::Normal);
        self.pos_map.update(pos_newest, || newest.hide());
        if newest.ptr() != old.ptr() {
            newest.clear_move_info();
        }
        old.set_other_location(Some(node));
        Some(index)
    }

    fn insert_node(&mut self, node: ValidatedNode<Id, Opt>) -> LocalChange {
        let ValidatedNode {
            node,
            insertion_sibling,
            insertion_neighbor,
        } = node;

        let node = self.allocate(node);
        let old_index = self.update_move_info(node);

        self.sibling_set.insert_after_opt(
            insertion_sibling,
            SiblingSetNode::new(node, SiblingSetNodeKind::Child),
        );

        self.sibling_set
            .insert(SiblingSetNode::new(node, SiblingSetNodeKind::Parent))
            .ok()
            .unwrap();

        let neighbor = insertion_neighbor.map(|n| {
            PosMapNode::new(n.node(), match n.kind() {
                SiblingSetNodeKind::Child => PosMapNodeKind::Marker,
                SiblingSetNodeKind::Parent => PosMapNodeKind::Normal,
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
                let neighbor = neighbor
                    .expect("'before' nodes should always have a neighbor");
                self.pos_map.insert_before_from(neighbor, pos_nodes.rev());
            }
        }

        if let Some(old) = old_index {
            let new = self.index(node);
            if old == new {
                LocalChange::None
            } else {
                LocalChange::Move {
                    old,
                    new,
                }
            }
        } else if node.visibility() == Visibility::Hidden {
            LocalChange::None
        } else {
            LocalChange::Insert(self.index(node))
        }
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
    /// deleted or moved item).
    ///
    /// # Time complexity
    ///
    /// Iteration over the entire sequence is Θ(*[h]* + *[n]* log *[h]*).
    ///
    /// If you want to send the entire sequence to another client that does not
    /// yet have their own copy, or save the contents to disk, it is faster to
    /// [serialize] this [`Eips`] structure and the local list of values and
    /// send or save them together. (This cannot be used to merge changes from
    /// multiple sources; it can only be used to create the initial [`Eips`]
    /// object on a client that has not yet made or received any other
    /// changes.)
    ///
    /// [h]: #mathematical-variables
    /// [n]: #mathematical-variables
    #[cfg_attr(feature = "serde", doc = "[serialize]: Eips::serialize")]
    #[cfg_attr(
        not(feature = "serde"),
        doc = "[serialize]: https://docs.rs/serde/1/serde/trait.Serialize.html"
    )]
    pub fn changes(&self) -> Changes<'_, Id, Opt> {
        Changes {
            nodes: self.node_alloc.iter(),
            eips: self,
        }
    }

    /// Gets the item with ID `id` as a [`RemoteChange`].
    ///
    /// Returns a `(change, index)` tuple, where `change` is the
    /// [`RemoteChange`]. For changes that represent the insertion of a
    /// non-deleted item, `index` the local index of the item. Otherwise, for
    /// deleted items and changes corresponding to move operations, it is
    /// [`None`].
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
        self.find(id).map(|n| self.node_to_indexed_change(n)).ok_or(IdError {
            id,
        })
    }

    fn node_to_change(&self, node: StaticNode<Id, Opt>) -> RemoteChange<Id> {
        let move_info = node.old_location().map(|old| MoveInfo {
            timestamp: node
                .move_timestamp()
                .try_into()
                .expect("old_location implies non-zero timestamp"),
            old_location: old.id.clone(),
        });
        RemoteChange {
            id: node.id.clone(),
            raw_parent: node.raw_parent.clone(),
            direction: node.direction(),
            visibility: node.newest_location().visibility(),
            move_info,
        }
    }

    fn node_to_indexed_change(
        &self,
        node: StaticNode<Id, Opt>,
    ) -> (RemoteChange<Id>, Option<usize>) {
        let change = self.node_to_change(node);
        let is_visible_insertion = change.visibility == Visibility::Visible
            && change.move_info.is_none();
        let index =
            is_visible_insertion.then(|| self.index(node.newest_location()));
        (change, index)
    }

    fn plain_changes(&self) -> impl Iterator<Item = RemoteChange<Id>> + '_ {
        self.node_alloc.iter().map(|n| {
            // SAFETY: The pointer is valid because it comes from a reference.
            // It will remain valid for the life of the `StaticNode` because
            // we don't drop the arena allocator. No mutable references to the
            // node exist (the arena doesn't support them).
            let node = unsafe { StaticNode::new(n.into()) };
            self.node_to_change(node)
        })
    }
}

impl<Id, Opt> Drop for Eips<Id, Opt>
where
    Opt: EipsOptions,
{
    fn drop(&mut self) {
        // SAFETY: This is the only place were we call `ManuallyDrop::drop` on
        // these members. The `ManuallyDrop` instances will not exist after
        // this function (`Eips::drop`) returns.
        unsafe {
            ManuallyDrop::drop(&mut self.pos_map);
            ManuallyDrop::drop(&mut self.sibling_set);
        }

        // SAFETY: We don't provide public access to any type that contains
        // references or pointers to `Node`s (except potentially as an internal
        // implementation detail of types that borrow `self`, and thus cannot
        // exist when `self` is dropped), so the only references that could
        // exist are in `self`. No parts of `self` contain references to
        // `Node`s; some types contain pointers to `Node`s, but these are not
        // accessed during their destructors.
        unsafe {
            self.node_alloc.drop();
        }
    }
}

impl<Id, Opt> Clone for Eips<Id, Opt>
where
    Id: self::Id,
    Opt: EipsOptions,
{
    fn clone(&self) -> Self {
        let mut new = Self::new();
        self.plain_changes().for_each(|change| {
            let result = new.apply_change(change);
            if cfg!(debug_assertions) {
                result.map_err(|e| e.to_basic()).unwrap();
            }
        });
        new
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

// SAFETY: We don't provide references or pointers to data that aren't bound to
// the life of `self` with standard borrowing rules, so if we have ownership of
// this type to send to another thread, we know no other thread can possibly
// access internal data (including skip list nodes) concurrently.
unsafe impl<Id, Opt> Send for Eips<Id, Opt>
where
    Id: Send,
    Opt: EipsOptions,
{
}

// SAFETY: This type has no `&self` methods that mutate data, so if shared
// references are sent to other threads, borrowing rules guarantee that no
// thread will mutate any data (including skip list nodes). There may be
// concurrent reads, which is fine.
unsafe impl<Id, Opt> Sync for Eips<Id, Opt>
where
    Id: Sync,
    Opt: EipsOptions,
{
}

impl<Id, Opt: EipsOptions> Unpin for Eips<Id, Opt> {}

#[cfg(feature = "serde")]
#[cfg_attr(feature = "doc_cfg", doc(cfg(feature = "serde")))]
impl<Id, Opt> Serialize for Eips<Id, Opt>
where
    Id: self::Id + Serialize,
    Opt: EipsOptions,
{
    /// Serializes this instance of [`Eips`].
    ///
    /// The local list of values should usually be serialized at the same time,
    /// as the serialized [`Eips`] structure will produce incorrect results
    /// unless it is used with the particular local list of values that existed
    /// at the time of serialization.
    ///
    /// # Time complexity
    ///
    /// Θ(*[h](#mathematical-variables)*), assuming the time complexity of
    /// individual calls to the serializer does not depend on the amount of
    /// data that has been serialized so far.
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.node_alloc.len()))?;
        for change in self.plain_changes() {
            seq.serialize_element(&change)?;
        }
        seq.end()
    }
}

#[cfg(feature = "serde")]
#[cfg_attr(feature = "doc_cfg", doc(cfg(feature = "serde")))]
impl<'a, Id, Opt> Deserialize<'a> for Eips<Id, Opt>
where
    Id: self::Id + Deserialize<'a>,
    Opt: EipsOptions,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        use core::fmt;
        use core::marker::PhantomData;
        use serde::de::{self, Error, SeqAccess};

        struct Visitor<Id, Opt>(PhantomData<fn() -> (Id, Opt)>);

        impl<'a, Id, Opt> de::Visitor<'a> for Visitor<Id, Opt>
        where
            Id: crate::Id + Deserialize<'a>,
            Opt: EipsOptions,
        {
            type Value = Eips<Id, Opt>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "sequence of Eips changes")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'a>,
            {
                let mut eips = Eips::new();
                while let Some(elem) = seq.next_element()? {
                    eips.apply_change(elem)
                        .map_err(|e| Error::custom(e.to_basic()))?;
                }
                Ok(eips)
            }
        }

        deserializer.deserialize_seq(Visitor(PhantomData))
    }
}

/// Silence unused function warnings. This is better than annotating the
/// function with `#[allow(dead_code)]` because that would also silence unused
/// code warnings in the body of the function.
#[allow(dead_code)]
fn allow_unused() {
    let _ = PosMapNode::<u32, Options>::as_node;
}
