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
use rand_chacha::ChaChaRng;
#[allow(dead_code)]
mod common;
use common::{AllowedOps, Client, Update};
use std::collections::VecDeque;

const BUFFER_MAX_RATIO: usize = 3;

struct State {
    clients: Box<[Client]>,
    buffers: Box<[VecDeque<Update>]>,
    rng: ChaChaRng,
}

impl State {
    fn new(num_clients: usize) -> Self {
        let mut clients = Vec::with_capacity(num_clients);
        for i in 0..num_clients.try_into().unwrap() {
            clients.push(Client::new(i));
        }
        let mut buffers = Vec::new();
        buffers.resize(num_clients, VecDeque::new());
        Self {
            clients: clients.into(),
            buffers: buffers.into(),
            rng: common::make_rng(),
        }
    }

    fn tick(&mut self) {
        let num_clients = self.clients.len();
        let buffer_max = num_clients * BUFFER_MAX_RATIO;
        for (i, client) in self.clients.iter_mut().enumerate() {
            let update = client.random_op(AllowedOps::ALL, &mut self.rng);
            client.apply(update);
            for (j, buffer) in self.buffers.iter_mut().enumerate() {
                if i != j {
                    buffer.push_back(update);
                }
            }
        }
        let iter = self.clients.iter_mut().zip(&mut self.buffers);
        for (client, buffer) in iter {
            const BITS: u32 = 7;
            const POW: u32 = 4;
            let r: usize = self.rng.random_range(0..=(1 << BITS));
            // Number of items from the buffer to apply. This calculation
            // produces a random distribution biased towards zero but with an
            // expected value of `num_clients` and a maximum of
            // `num_clients * (POW + 1)` (inclusive).
            let mut n = (r.pow(POW) * (POW as usize + 1) * num_clients
                + (1 << (BITS * POW - 1)))
                >> (BITS * POW);
            if let Some(excess) = buffer.len().checked_sub(buffer_max) {
                n = n.max(excess);
            }
            for update in buffer.drain(0..n.min(buffer.len())) {
                client.apply(update);
            }
        }
    }

    fn finalize(&mut self) {
        let iter = self.clients.iter_mut().zip(&mut self.buffers);
        for (client, buffer) in iter {
            for update in buffer.drain(..) {
                client.apply(update);
            }
        }
    }
}

fn run(num_clients: usize, num_iterations: usize) {
    let mut state = State::new(num_clients);
    for _ in 0..num_iterations {
        state.tick();
    }
    state.finalize();
    let text = state.clients[0].text();
    for client in state.clients.iter().skip(1) {
        assert!(text == client.text());
    }
}

#[test]
fn two() {
    run(2, 600);
}

#[test]
fn three() {
    run(3, 400);
}

#[test]
fn ten() {
    run(10, 50);
}
