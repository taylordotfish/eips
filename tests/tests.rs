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

use rand::Rng;
#[allow(dead_code)]
mod common;
use btree_vec::BTreeVec;
use eips::Eips;

#[test]
fn get() {
    const NUM_INSERT: usize = 1000;
    const NUM_REMOVE: usize = 300;
    const NUM_MOVE: usize = 250;

    let mut rng = common::make_rng();
    let mut items = BTreeVec::<u128, 16>::create();
    let mut eips = Eips::<u128>::new();

    for i in 0..NUM_INSERT {
        let id = rng.random();
        let index = rng.random_range(0..i + 1);
        let change = eips.insert(index, id).unwrap();
        assert_eq!(change.id, id);
        eips.apply_change(change).unwrap();
        items.insert(index, id);
    }

    let mut removed = Vec::new();
    for i in 0..NUM_REMOVE {
        let index = rng.random_range(0..NUM_INSERT - i);
        let change = eips.remove(index).unwrap();
        eips.apply_change(change).unwrap();
        let id = items.remove(index);
        assert_eq!(change.id, id);
        removed.push(id);
    }

    const LEN: usize = NUM_INSERT - NUM_REMOVE;
    for _ in 0..NUM_MOVE {
        let old = rng.random_range(0..LEN);
        let new = rng.random_range(0..LEN);
        let id = rng.random();
        let change = eips.mv(old, new, id).unwrap();
        assert_eq!(change.id, id);
        eips.apply_change(change).unwrap();
        let old_id = items.remove(old);
        items.insert(new, old_id);
    }

    // These changes are intentionally not applied.
    for _ in 0..NUM_REMOVE {
        let index = rng.random_range(0..LEN);
        let change = eips.remove(index).unwrap();
        assert_eq!(change.id, items[index]);
    }

    assert_eq!(eips.len(), LEN);
    assert_eq!(items.len(), LEN);
    for (i, id) in items.iter().copied().enumerate() {
        assert_eq!(eips.get(i).unwrap(), id);
        assert_eq!(eips.remote_get(&id).unwrap(), Some(i));
        let (change, index) = eips.get_change(&id).unwrap();
        assert_eq!(change.id, id);
        assert_eq!(index, Some(i));
    }
    assert!(eips.get(LEN).is_err());
    assert!(eips.remote_get(&rng.random()).is_err());
    assert!(eips.get_change(&rng.random()).is_err());
    for id in removed {
        assert_eq!(eips.remote_get(&id).unwrap(), None);
        let (change, index) = eips.get_change(&id).unwrap();
        assert_eq!(change.id, id);
        assert_eq!(index, None);
    }
}
