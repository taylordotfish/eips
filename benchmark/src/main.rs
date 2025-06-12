use benchmark::*;
use cpu_time::ThreadTime;
use rand::Rng;
use std::collections::VecDeque;
use std::process::ExitCode;

const BUFFER_MAX: usize = 20;

fn benchmark<C: Client>(
    num_clients: usize,
    num_iterations: usize,
    allowed_ops: AllowedOps,
) {
    let mut rng = make_rng();
    let mut clients: Vec<C> =
        (0..num_clients.try_into().unwrap()).map(C::new).collect();
    let mut buffers = vec![VecDeque::new(); num_clients];
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
            // Number of items from buffer to apply. The average of this is 1.
            let mut n = match rng.random_range(0..20) {
                0..=9 => 0,
                10..=13 => 1,
                14..=16 => 2,
                17..=18 => 3,
                19 => 4,
                _ => unreachable!(),
            };
            // Don't let the buffer grow beyond `BUFFER_MAX`.
            if n == 0 && buffer.len() >= BUFFER_MAX {
                n = 1;
            }
            for update in buffer.drain(0..n.min(buffer.len())) {
                client.apply(update);
            }
        }
    }
    for (client, buffer) in clients.iter_mut().zip(buffers) {
        for update in buffer {
            client.apply(update);
        }
    }
    let elapsed = start_time.elapsed();
    println!("time elapsed: {}", elapsed.as_secs_f64());
    show_memory_use();
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
