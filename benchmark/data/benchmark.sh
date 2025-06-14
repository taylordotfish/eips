#!/bin/sh
# Copyright (C) 2025 taylor.fish <contact@taylor.fish>
#
# This file is part of Eips.
#
# Eips is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Eips is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
# Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with Eips. If not, see <https://www.gnu.org/licenses/>.

set -euf

seed=${BENCHMARK_SEED-123456789}
parallel_jobs=${BENCHMARK_JOBS-4}

script_dir=$(dirname "$0")
root_dir=$script_dir/../../
benchmark_bin=$root_dir/target/release/benchmark

run_once() {
    printf '%s,' "$@"
    EIPS_SEED=$seed "$benchmark_bin" "$@" |
        grep -E '^(time|memory)[^A-Za-z]' |
        sed 's/[^0-9.]//g; $!s/$/,/' |
        tr -d '\n'
    printf '\n'
}

if [ "${1-}" = __run ]; then
    iterations=$2
    shift 2
    while [ "$iterations" -gt 0 ]; do
        run_once "$@"
        : $((iterations -= 1))
    done
    exit
fi

if ! command -v parallel > /dev/null; then
    printf >&2 '%s\n' "error: missing GNU Parallel"
    exit 1
fi

if [ -z "${BENCHMARK_NO_REBUILD-}" ]; then
    (cd "$script_dir" && cargo b --release)
fi
if ! [ -x "$benchmark_bin" ]; then
    printf >&2 '%s\n' "error: cannot find benchmark binary"
    exit 1
fi

run() {
    printf '%s\n' "$0 __run $*"
}

printf '%s\n' "seed,$seed"
printf '%s\n' "commit,$(git rev-parse HEAD)"
if sed -n '/^name = "skippy"/,$ {
    /^version/ { p; q }
}' "$root_dir"/Cargo.lock | grep -q -- -dev; then
    printf '%s\n' "skippy,$(git -C "$root_dir"/../skippy rev-parse HEAD)"
fi
printf '%s\n' "rust,$(rustc -Vv | sed '$!s/$/,/' | tr -d '\n')"
printf '\n'
printf '%s\n' "crdt,clients,iterations,time (s),memory (B)"
{
    run 50 eips 2 10000
    run 10 eips 2 100000
    run 10 eips 2 1000000

    run 10 diamond 2 10000
    run 10 diamond 2 100000
    run 3 diamond 2 1000000

    run 100 eips 10 20
    run 100 eips 10 60
    run 50 eips 10 200
    run 10 eips 10 2000

    run 10 diamond 10 20
    run 3 diamond 10 60
    run 1 diamond 10 200
    run 1 diamond 10 2000
} | parallel -j"$parallel_jobs" --line-buffer
