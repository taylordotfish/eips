/*
 * Copyright (C) 2025 taylor.fish <contact@taylor.fish>
 *
 * This file is part of Eips.
 *
 * Eips is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Eips is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
 * Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with Eips. If not, see <https://www.gnu.org/licenses/>.
 */

use btree_vec::BTreeVec;
use eips::{Eips, LocalChange, RemoteChange};

type EipsOptions = eips::Options</* SUPPORTS_MOVE */ false>;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Id {
    pub client_id: u64,
    pub counter: u64,
}

impl Id {
    fn increment(&mut self) -> Self {
        let id = *self;
        self.counter += 1;
        id
    }
}

pub struct Client {
    eips: Eips<Id, EipsOptions>,
    text: BTreeVec<char, 32>,
    next_id: Id,
}

#[derive(Copy, Clone)]
pub struct Update {
    change: RemoteChange<Id>,
    item: Option<char>,
}

impl super::Client for Client {
    type Update = Update;

    fn new(client_id: u64) -> Self {
        Self {
            eips: Eips::new(),
            text: Default::default(),
            next_id: Id {
                client_id,
                counter: 0,
            },
        }
    }

    fn len(&self) -> usize {
        self.text.len()
    }

    fn text(&self) -> impl Iterator<Item = char> + '_ {
        self.text.iter().copied()
    }

    fn insert(&mut self, index: usize, c: char) -> Self::Update {
        let update = Update {
            change: self
                .eips
                .insert(index, self.next_id.increment())
                .expect("bad index"),
            item: Some(c),
        };
        self.apply(update);
        update
    }

    fn remove(&mut self, index: usize) -> Self::Update {
        let update = Update {
            change: self.eips.remove(index).expect("bad index"),
            item: None,
        };
        self.apply(update);
        update
    }

    fn apply(&mut self, update: Self::Update) {
        let local =
            self.eips.apply_change(update.change).expect("bad remote change");
        match local {
            LocalChange::Insert(index) => {
                self.text.insert(
                    index,
                    update.item.expect("insertion should have item"),
                );
            }
            LocalChange::Remove(index) => {
                debug_assert!(update.item.is_none());
                self.text.remove(index);
            }
            LocalChange::Move {
                ..
            } => unreachable!("move operations not supported"),
            LocalChange::None => {}
            LocalChange::AlreadyApplied => {}
        }
    }
}
