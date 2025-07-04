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

use super::condvar::RwCondvar;
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

impl<T> Shared<T> {
    fn prune(&mut self) {
        let start = Client {
            index: self.offset,
            id: 0,
        };
        if let Some(min) = self
            .clients
            .range(start..)
            .next()
            .or_else(|| self.clients.iter().next())
            .map(|c| c.index - self.offset)
        {
            self.buffer.drain(0..min.0);
            self.offset += min;
        } else {
            self.buffer.clear();
            self.offset = Wrapping(0);
        }
    }
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
        let mut shared = self.shared.write().unwrap();
        let id = shared.next_id.0;
        shared.next_id += 1;

        let client = Client {
            index: shared.offset,
            id,
        };

        let inserted = shared.clients.insert(client);
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
        let mut shared = self.shared.write().unwrap();
        if !shared.clients.is_empty() {
            shared.buffer.extend(iter);
        }
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

impl<T> Drop for Receiver<T> {
    fn drop(&mut self) {
        let mut shared = self.shared.write().unwrap();
        let removed = shared.clients.remove(&self.client);
        debug_assert!(removed);
        shared.prune();
    }
}

#[must_use]
pub struct Recv<'a, T> {
    shared: &'a RwLock<Shared<T>>,
    guard: ManuallyDrop<RwLockReadGuard<'a, Shared<T>>>,
    start: Wrapping<usize>,
    client: &'a mut Client,
}

impl<T> Recv<'_, T> {
    pub fn wait(mut self, cond: &RwCondvar) -> Self {
        let guard = cond.wait(
            self.shared,
            // SAFETY: We do not use this `ManuallyDrop` again; rather, we
            // replace `self.guard` with a new one.
            unsafe { ManuallyDrop::take(&mut self.guard) },
        );
        self.guard = ManuallyDrop::new(guard.unwrap());
        self
    }
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
        // SAFETY: We drop this member only here, in the destructor, where it
        // won't be accessed again.
        unsafe {
            ManuallyDrop::drop(&mut self.guard);
        }

        if self.client.index == self.start {
            return;
        }

        let mut shared = self.shared.write().unwrap();
        let removed = shared.clients.remove(&Client {
            index: self.start,
            id: self.client.id,
        });
        debug_assert!(removed);
        let inserted = shared.clients.insert(*self.client);
        debug_assert!(inserted);
        shared.prune();
    }
}
