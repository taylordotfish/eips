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

use std::collections::{BTreeSet, VecDeque};
use std::iter;
use std::mem::ManuallyDrop;
use std::num::Wrapping;
use std::sync::{Arc, RwLock, RwLockReadGuard};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Client {
    pub index: Wrapping<usize>,
    pub id: usize,
}

struct Shared<T> {
    pub buffer: VecDeque<T>,
    pub offset: Wrapping<usize>,
    pub clients: BTreeSet<Client>,
    pub next_id: Wrapping<usize>,
}

pub struct Sender<T> {
    shared: Arc<RwLock<Shared<T>>>,
}

impl<T> Sender<T> {
    pub fn new() -> Self {
        let shared = Shared {
            buffer: VecDeque::new(),
            offset: Wrapping(0),
            clients: BTreeSet::new(),
            next_id: Wrapping(0),
        };
        Self {
            shared: Arc::new(RwLock::new(shared)),
        }
    }

    pub fn new_receiver(&mut self) -> Receiver<T> {
        let mut guard = self.shared.write().unwrap();
        let id = guard.next_id.0;
        guard.next_id += 1;

        let client = Client {
            index: guard.offset,
            id,
        };

        let inserted = guard.clients.insert(client);
        debug_assert!(inserted);
        Receiver {
            shared: self.shared.clone(),
            client,
        }
    }

    pub fn send(&mut self, item: T) {
        self.send_all(iter::once(item));
    }

    pub fn send_all<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        self.shared.write().unwrap().buffer.extend(iter);
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Self {
            shared: self.shared.clone(),
        }
    }
}

#[must_use]
pub struct Receiver<T> {
    shared: Arc<RwLock<Shared<T>>>,
    client: Client,
}

impl<T> Receiver<T> {
    pub fn recv(&mut self) -> Recv<'_, T> {
        Recv {
            shared: &self.shared,
            guard: ManuallyDrop::new(self.shared.read().unwrap()),
            start: self.client.index,
            client: &mut self.client,
        }
    }
}

#[must_use]
pub struct Recv<'a, T> {
    shared: &'a RwLock<Shared<T>>,
    guard: ManuallyDrop<RwLockReadGuard<'a, Shared<T>>>,
    start: Wrapping<usize>,
    client: &'a mut Client,
}

impl<T: Clone> Iterator for Recv<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let index = (self.client.index - self.guard.offset).0;
        let id = self.guard.buffer.get(index)?;
        self.client.index += 1;
        Some(id.clone())
    }
}

impl<T> Drop for Recv<'_, T> {
    fn drop(&mut self) {
        unsafe {
            ManuallyDrop::drop(&mut self.guard);
        }

        if self.client.index == self.start {
            return;
        }

        let mut guard = self.shared.write().unwrap();
        let removed = guard.clients.remove(&Client {
            index: self.start,
            id: self.client.id,
        });
        debug_assert!(removed);
        let inserted = guard.clients.insert(*self.client);
        debug_assert!(inserted);

        let start = Client {
            index: guard.offset,
            id: 0,
        };
        let min = guard
            .clients
            .range(start..)
            .next()
            .unwrap_or_else(|| guard.clients.iter().next().unwrap())
            .index
            - guard.offset;
        guard.buffer.drain(0..min.0);
        guard.offset += min;
    }
}
