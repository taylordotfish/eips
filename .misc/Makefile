# Copyright (C) 2024 taylor.fish <contact@taylor.fish>
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

.PHONY: all
all: ../README.md package-readme.md

SOURCES = README.m4 ../Cargo.toml ../src/common-readme.md

../README.md: $(SOURCES)
	m4 $< > $@

package-readme.md: $(SOURCES)
	m4 -DRUST $< > $@
