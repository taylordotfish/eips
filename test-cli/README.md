Test CLI
========

This directory contains an interactive command-line program designed to test
and demonstrate Eips. Each running instance of the program represents a client,
or “node”, with its own copy of a shared document. Nodes can be connected to
other nodes, and will transmit updates they produce or receive to those nodes.

Two nodes that have received the same set of updates will see the exact same
document, regardless of the order in which the updates were received (this is
the *eventual consistency* guarantee). Once all nodes have received all other
nodes’ updates, every node’s copy of the document will be identical.

Dependencies
------------

Development files for GNU Readline must be installed (e.g., `libreadline-dev`
on Debian).

This program is designed to work on Unix-like systems. It has been tested on
Debian GNU/Linux.

Usage
-----

`cd` into this directory and run `cargo run -- --help` for usage information.
Once the program is running, type `help` for a list of commands.
