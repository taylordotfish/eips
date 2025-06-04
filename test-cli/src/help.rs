/*
 * Copyright (C) [unpublished] taylor.fish <contact@taylor.fish>
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

use std::fmt::{self, Display};

pub struct Command {
    pub name: &'static str,
    pub args: &'static str,
    pub help: &'static str,
}

impl Command {
    pub fn short_display(&self) -> ShortDisplay<'_> {
        ShortDisplay(self)
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n\n{}", self.short_display(), self.help)
    }
}

pub struct ShortDisplay<'a>(&'a Command);

impl Display for ShortDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.name)?;
        if !self.0.args.is_empty() {
            write!(f, " {}", self.0.args)?;
        }
        Ok(())
    }
}

pub static COMMANDS: [Command; 15 + cfg!(eips_debug) as usize] = [
    Command {
        name: "help",
        args: "[command]",
        help: "\
Show help for [command]. If [command] is not given, show a list of all
commands.",
    },
    Command {
        name: "connect",
        args: "<port>",
        help: "Connect to the node running on port <port>.",
    },
    Command {
        name: "disconnect",
        args: "<node>",
        help: "Disconnect from the node with ID <node>.",
    },
    Command {
        name: "insert",
        args: "<index> <text>",
        help: "Insert <text> at index <index>.",
    },
    Command {
        name: "insert-reverse",
        args: "<index> <text>",
        help: "\
Insert <text> at index <index>, but in reverse order, starting with the
rightmost character. The final result will still be the same.",
    },
    Command {
        name: "remove",
        args: "<start> <len>",
        help: "Remove <len> characters starting at index <start>.",
    },
    Command {
        name: "move",
        args: "<start> <len> <dest>",
        help: "\
Move the first <len> characters at index <start> to index <dest>. The
index of the text after being moved will be <dest>, which is different
from moving the text to be adjacent to the text *currently* at <dest>.",
    },
    Command {
        name: "show",
        args: "[start [end]]",
        help: "\
Show the document. If [start] is provided, start at index [start]; if
[end] is provided, end at index [end] (exclusive).",
    },
    Command {
        name: "search",
        args: "<substring>",
        help: "Show the index of <substring> in the document.",
    },
    Command {
        name: "changes",
        args: "",
        help: "Show the document as a list of Eips RemoteChange objects.",
    },
    Command {
        name: "outgoing",
        args: "",
        help: "Show the outgoing nodes to which this node is connected.",
    },
    Command {
        name: "incoming",
        args: "",
        help: "Show the incoming nodes that are connected to this node.",
    },
    Command {
        name: "block",
        args: "[node]",
        help: "\
Block the outgoing node [node] from receiving changes from this node. If
[node] is not given, block all outgoing nodes.",
    },
    Command {
        name: "unblock",
        args: "[node]",
        help: "\
Unblock outgoing node [node] so that it once again receives changes from
this node. If [node] is not given, unblock all outgoing nodes.",
    },
    Command {
        name: "source",
        args: "<file>",
        help: "Read and execute commands from <file>.",
    },
    #[cfg(eips_debug)]
    Command {
        name: "debug",
        args: "",
        help: "Write debug information to ./debug/.",
    },
];
