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

use benchmark::*;
use cpu_time::ThreadTime;
use rand::Rng;
use std::collections::VecDeque;
use std::process::ExitCode;

// The length of update buffers will be limited to this number times the number
// of clients.
const BUFFER_MAX_RATIO: usize = 10;

fn benchmark<C: Client>(
    num_clients: usize,
    num_iterations: usize,
    allowed_ops: AllowedOps,
) {
    let mut rng = make_rng();
    let mut clients: Vec<C> =
        (0..num_clients.try_into().unwrap()).map(C::new).collect();
    let mut buffers = vec![VecDeque::new(); num_clients];
    let buffer_max = num_clients * BUFFER_MAX_RATIO;
    let start_time = ThreadTime::now();

    for _ in 0..num_iterations {
        for (i, client) in clients.iter_mut().enumerate() {
            let update = client.random_op(allowed_ops, &mut rng);
            // Add update to other clients' buffers.
            let mut iter = buffers
                .iter_mut()
                .enumerate()
                .filter_map(|(j, b)| (j != i).then_some(b));
            let Some(first) = iter.next() else {
                continue;
            };
            // Handle the first buffer specially to avoid an extra clone.
            first.push_back(update);
            let update_ref = first.back().unwrap();
            for buffer in iter {
                buffer.push_back(update_ref.clone());
            }
        }
        for (client, buffer) in clients.iter_mut().zip(&mut buffers) {
            const BITS: u32 = 7;
            const POW: u32 = 4;
            let r: usize = rng.random_range(0..=(1 << BITS));
            // Number of items from the buffer to apply. This calculation
            // produces a random distribution biased towards zero but with an
            // expected value of `num_clients` and a maximum of
            // `num_clients * (POW + 1)` (inclusive).
            let mut n = (r.pow(POW) * (POW as usize + 1) * num_clients
                + (1 << (BITS * POW - 1)))
                >> (BITS * POW);
            // Ensure the buffer never has more than `buffer_max` items at the
            // start of the next iteration.
            if let Some(excess) = buffer.len().checked_sub(buffer_max) {
                n = n.max(excess);
            }
            for update in buffer.drain(0..n.min(buffer.len())) {
                client.apply(update);
            }
        }
    }

    for (client, buffer) in clients.iter_mut().zip(&mut buffers) {
        for update in buffer.drain(..) {
            client.apply(update);
        }
    }

    std::hint::black_box(&clients);
    let elapsed = start_time.elapsed();
    println!("time elapsed: {} seconds", elapsed.as_secs_f64());

    drop(buffers);
    show_memory_use();
    std::hint::black_box(&clients);
    if let Some(client) = clients.first() {
        println!("len: {}", client.len());
    }
}

const USAGE: &str = "\
Usage: benchmark <crdt> <num-clients> <num-iterations> [allowed-ops]
* <crdt> is 'eips' or 'diamond'
* [allowed-ops] is 'insert' or 'all' (default)
";

fn run() -> ExitCode {
    let mut args = std::env::args().skip(1);
    let Some(crdt) = args.next() else {
        eprintln!("error: missing <crdt>");
        return ExitCode::FAILURE;
    };
    if !matches!(&*crdt, "eips" | "diamond") {
        eprintln!("error: invalid <crdt>");
        return ExitCode::FAILURE;
    }
    let Some(num_clients) = args.next().and_then(|a| a.parse().ok()) else {
        eprintln!("error: missing/invalid <num-clients>");
        return ExitCode::FAILURE;
    };
    let Some(num_iterations) = args.next().and_then(|a| a.parse().ok()) else {
        eprintln!("error: missing/invalid <num-iterations>");
        return ExitCode::FAILURE;
    };
    let allowed = match args.next().as_deref() {
        Some("insert") => AllowedOps::InsertOnly,
        None | Some("all") => AllowedOps::All,
        _ => {
            eprintln!("invalid [allowed-ops]");
            return ExitCode::FAILURE;
        }
    };
    match &*crdt {
        "eips" => {
            benchmark::<eips::Client>(num_clients, num_iterations, allowed);
        }
        "diamond" => {
            benchmark::<diamond::Client>(num_clients, num_iterations, allowed);
        }
        _ => unreachable!(),
    }
    ExitCode::SUCCESS
}

fn main() -> ExitCode {
    let code = run();
    if code == ExitCode::FAILURE {
        eprint!("\n{USAGE}");
    }
    code
}
