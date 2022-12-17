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

use std::sync::{Condvar, LockResult, Mutex, RwLock, RwLockReadGuard};

pub struct RwCondvar {
    cond: Condvar,
    mutex: Mutex<()>,
}

impl RwCondvar {
    pub fn new() -> Self {
        Self {
            cond: Condvar::new(),
            mutex: Mutex::new(()),
        }
    }

    pub fn wait<'a, T>(
        &self,
        lock: &'a RwLock<T>,
        guard: RwLockReadGuard<'a, T>,
    ) -> LockResult<RwLockReadGuard<'a, T>> {
        let mutex_guard = self.mutex.lock().unwrap();
        drop(guard);
        let _mutex_guard = self.cond.wait(mutex_guard).unwrap();
        lock.read()
    }

    pub fn notify_all(&self) {
        let _guard = self.mutex.lock().unwrap();
        self.cond.notify_all();
    }
}
