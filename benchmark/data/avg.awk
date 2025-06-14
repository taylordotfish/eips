#!/usr/bin/env -S awk -f
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

BEGIN {
    FS = ","
    prefix_len = 3
}

state == 2 {
    prefix = ""
    for (i = 1; i <= prefix_len; ++i) {
        prefix = prefix $(i) ","
    }
    time_sums[prefix] += $4
    memory_sums[prefix] += $5
    ++counts[prefix]
    next
}

state == 1 {
    # Skip line with column names
    state = 2
}

state == 0 && $0 == "" {
    # Header done
    state = 1
}

{ print }

END {
    for (prefix in counts) {
        count = counts[prefix]
        time = time_sums[prefix] / count
        memory = memory_sums[prefix] / count
        printf "%s%g,%d\n", prefix, time, memory | \
            "sort -t, -k1,1 -k2n,2 -k3n,3"
    }
}
