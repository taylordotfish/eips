Eips
====

Eips is the *efficient intention-preserving sequence*: a sequence
<abbr title="conflict-free replicated data type">CRDT</abbr> with **worst-case
non-amortized logarithmic-time** operations, minimal memory usage, and no
concurrent interleaving issues or duplications from concurrent moves as seen in
some other CRDTs.

Features
--------

* No interleaving of characters when multiple users insert text at the same
  position, even when text is typed in reverse (by typing a letter, moving the
  cursor back one, typing the next letter, etc.)
* Support for move operations. Items can be moved to another position in the
  sequence and will not be duplicated if multiple users try to move the same
  item concurrently.
* Insertions, deletions, moves, and accesses are worst-case non-amortized
  O(log *h*), where *h* is the total number of items ever inserted in the
  document.
* Constant memory use per item. Even as the editing history grows, and even
  with huge numbers of clients and concurrent edits, changes always use the
  same amount of memory. This applies to the number of bytes it takes to
  communicate changes to other clients, too.
* The [CRDT structure][Eips] doesn’t store items directly, but rather
  translates between *[local changes][LocalChange]* (which use simple integer
  indices) and *[remote changes][RemoteChange]* (which use IDs and are suitable
  for sending over a network). This means the items themselves may be stored in
  any plain list-like type, such as a simple growable array ([`Vec`]) or an
  [unsorted counted B-tree][cb] like [btree-vec]. The time complexity of local
  operations on the sequence then depends only on the number of *visible*
  items—tombstones don’t cause a performance penalty.
* Simple API. Three functions provide the ability to insert, delete, and move
  elements, and one function applies changes from remote clients. A basic use
  case won’t need much else. (Also, the [function][apply_change] that applies
  changes is the only one that can mutate the CRDT structure, making it easy to
  reason about the state of the document.)

[cb]: https://www.chiark.greenend.org.uk/~sgtatham/algorithms/cbtree.html

Requirements
------------

* As with many sequence CRDTs, Eips assumes changes are delivered in causal
  order.
* Clients must be capable of generating unique IDs. If each client already has
  a unique client ID, a common approach is to use (*client-id*, *counter*)
  pairs, where *counter* is a simple per-client increasing integer. UUIDs may
  be used in cases where this isn’t possible.

Serialization
-------------

When the crate feature `serde` is enabled, [`RemoteChange`][RemoteChange] (and
types it contains) will implement [Serde][serde]’s [`Serialize`] and
[`Deserialize`] traits.

[LocalChange]: https://docs.rs/eips/0.2/eips/changes/enum.LocalChange.html
[RemoteChange]: https://docs.rs/eips/0.2/eips/changes/struct.RemoteChange.html
[btree-vec]: https://github.com/taylordotfish/btree-vec
[apply_change]: https://docs.rs/eips/0.2/eips/struct.Eips.html#method.apply_change
[`insert`]: https://docs.rs/eips/0.2/eips/struct.Eips.html#method.insert
[`delete`]: https://docs.rs/eips/0.2/eips/struct.Eips.html#method.delete
[`Vec`]: https://doc.rust-lang.org/stable/std/vec/struct.Vec.html
[Eips]: https://docs.rs/eips/0.2/eips/struct.Eips.html
[serde]: https://docs.rs/serde/1/serde/
[`Serialize`]: https://docs.rs/serde/1/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/1/serde/trait.Deserialize.html
