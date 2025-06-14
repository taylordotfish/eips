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

.PHONY: all
all: html

.PHONY: html
html: design.html

define HTML_HEADER
<!DOCTYPE html>
<meta charset="utf-8" />
<title>Design</title>
<style>
    html {
        font-size: 16px;
        font-family: sans-serif;
        padding: 2em 1em;
    }
    body {
        max-width: 55em;
        margin-left: auto;
        margin-right: auto;
        line-height: 1.5;
    }
    img {
        display: block;
        margin-left: auto;
        margin-right: auto;
    }
    table {
        border-collapse: collapse;
        margin-left: auto;
        margin-right: auto;
    }
    th, td {
        padding: 0.2em 1em;
    }
    td {
        border-top: 1px solid #cccccc;
        border-collapse: collapse;
    }
</style>
endef
export HTML_HEADER

design.html: design.md
	@set -euf; \
	cmark="cmark --unsafe"; \
	if command -v pulldown-cmark > /dev/null; then \
		cmark=pulldown-cmark; \
	fi; \
	exec > $@; \
	printf '%s\n' "$$HTML_HEADER"; \
	set -x; \
	$$cmark $^; \

.PHONY: clean
clean:
	rm -f design.html

.DELETE_ON_ERROR:
