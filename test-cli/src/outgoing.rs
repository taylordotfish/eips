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

use crate::bincode::{self, ErrorExt};
use crate::condvar::RwCondvar;
use crate::queue::Receiver;
use crate::{AddrMap, OutgoingMap, ShutdownOnDrop, UPDATE_BUFFER_LEN};
use crate::{Document, Message, Node, NodeAddr, ReceivedUpdate, Update};
use std::collections::btree_map::Entry;
use std::fmt::{self, Display};
use std::io;
use std::mem;
use std::net::{Shutdown, SocketAddr, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use std::thread::JoinHandle;

pub struct Outgoing {
    pub shared: Arc<OutgoingShared>,
    pub node: Option<Node>,
    pub thread: JoinHandle<()>,
}

pub struct OutgoingShared {
    pub stream: TcpStream,
    pub cond: RwCondvar,
    pub shutdown: AtomicBool,
    pub blocked: AtomicBool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SetBlockedResult {
    Changed,
    NoOp,
}

impl SetBlockedResult {
    pub fn changed(self) -> bool {
        self == Self::Changed
    }
}

impl OutgoingShared {
    pub fn set_blocked(&self, blocked: bool) -> SetBlockedResult {
        if self.blocked.load(Ordering::Relaxed) == blocked {
            return SetBlockedResult::NoOp;
        }
        self.blocked.store(blocked, Ordering::Release);
        if !blocked {
            self.cond.notify_all();
        }
        SetBlockedResult::Changed
    }
}

pub struct OutgoingThread {
    pub addr: SocketAddr,
    pub local_node: Node,
    pub document: Arc<RwLock<Document>>,
    pub updates: Receiver<ReceivedUpdate>,
    pub outgoing: Arc<RwLock<OutgoingMap>>,
    pub addrs: Arc<RwLock<AddrMap>>,
}

#[derive(Debug)]
pub enum OutgoingError {
    WriteFailed(bincode::Error, NodeAddr),
    ReadFailed(bincode::Error, NodeAddr),
    SelfConnect,
    AlreadyConnected(Node),
}

impl Display for OutgoingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WriteFailed(e, addr) => write!(
                f,
                "could not write to {addr}: {}",
                ErrorExt::display(&**e),
            ),
            Self::ReadFailed(e, addr) => write!(
                f,
                "could not read from {addr}: {}",
                ErrorExt::display(&**e),
            ),
            Self::SelfConnect => write!(f, "cannot connect to self"),
            Self::AlreadyConnected(node) => {
                write!(f, "already connected to node {node}")
            }
        }
    }
}

impl OutgoingThread {
    pub fn run(self) -> (Option<Node>, Result<(), OutgoingError>) {
        let shutdown = ShutdownOnDrop;
        let shared = self.outgoing.read().unwrap()[&self.addr].shared.clone();
        let buffer = Vec::with_capacity(UPDATE_BUFFER_LEN);
        let mut node = None;
        let mut result = RunningOutgoingThread {
            addr: self.addr,
            local_node: self.local_node,
            stream: &shared.stream,
            cond: &shared.cond,
            shutdown: &shared.shutdown,
            blocked: &shared.blocked,
            document: &self.document,
            updates: self.updates,
            outgoing: &self.outgoing,
            addrs: &self.addrs,
            buffer,
            node: &mut node,
        }
        .run();
        let disconnected = match &result {
            Err(OutgoingError::WriteFailed(e, _)) => {
                e.io_error_kind() == Some(io::ErrorKind::BrokenPipe)
            }
            _ => false,
        };

        let shutdown_result = shared.stream.shutdown(Shutdown::Both);
        if disconnected {
            result = Ok(());
        } else {
            ignore_error!(shutdown_result);
        }

        if let Some(node) = node {
            if let Entry::Occupied(entry) =
                self.addrs.write().unwrap().entry(node)
            {
                if *entry.get() == self.addr {
                    entry.remove();
                }
            }
        }
        self.outgoing.write().unwrap().remove(&self.addr);
        mem::forget(shutdown);
        (node, result)
    }
}

struct RunningOutgoingThread<'a> {
    addr: SocketAddr,
    local_node: Node,
    stream: &'a TcpStream,
    cond: &'a RwCondvar,
    shutdown: &'a AtomicBool,
    blocked: &'a AtomicBool,
    document: &'a RwLock<Document>,
    updates: Receiver<ReceivedUpdate>,
    outgoing: &'a RwLock<OutgoingMap>,
    addrs: &'a RwLock<AddrMap>,
    buffer: Vec<Update>,
    node: &'a mut Option<Node>,
}

impl RunningOutgoingThread<'_> {
    fn node_addr(&self) -> NodeAddr {
        NodeAddr::new(self.addr, *self.node)
    }

    fn send_buffer(&mut self) -> Result<(), OutgoingError> {
        let node_addr = self.node_addr();
        for update in self.buffer.drain(..) {
            let msg = Message::Update(update);
            if let Err(e) = bincode::serialize(&msg, &mut self.stream) {
                return Err(OutgoingError::WriteFailed(e, node_addr));
            }
        }
        Ok(())
    }

    fn send_initial(&mut self) -> Result<(), OutgoingError> {
        self.buffer.clear();
        let mut document = self.document.read().unwrap();
        let mut paused = document.eips.changes().pause();
        loop {
            if self.shutdown.load(Ordering::Acquire) {
                return Ok(());
            }

            let mut iter = paused.resume(&document.eips);
            self.buffer.extend(iter.by_ref().take(UPDATE_BUFFER_LEN).map(
                |(change, i)| Update {
                    change,
                    character: i.map(|i| document.text[i]),
                },
            ));

            if self.buffer.is_empty() {
                return Ok(());
            }
            paused = iter.pause();
            drop(document);
            self.send_buffer()?;
            document = self.document.read().unwrap();
        }
    }

    fn send_updates(&mut self, node: Node) -> Result<(), OutgoingError> {
        self.buffer.clear();
        let mut iter = self.updates.recv();
        loop {
            if self.shutdown.load(Ordering::Acquire) {
                return Ok(());
            }
            if self.blocked.load(Ordering::Acquire) {
                iter = iter.wait(self.cond);
                continue;
            }

            let updates = iter.by_ref().filter_map(|update| {
                (update.node != node).then_some(update.update)
            });
            self.buffer.extend(updates.take(UPDATE_BUFFER_LEN));

            if self.buffer.is_empty() {
                iter = iter.wait(self.cond);
                continue;
            }
            drop(iter);
            self.send_buffer()?;
            iter = self.updates.recv();
        }
    }

    pub fn run(mut self) -> Result<(), OutgoingError> {
        let local = self.local_node;
        if let Err(e) = bincode::serialize(&local, &mut self.stream) {
            return Err(OutgoingError::WriteFailed(e, self.node_addr()));
        }

        let node: Node = match bincode::deserialize(&mut self.stream) {
            Ok(node) => node,
            Err(e) => {
                return Err(OutgoingError::ReadFailed(e, self.node_addr()));
            }
        };

        *self.node = Some(node);
        if node == local {
            return Err(OutgoingError::SelfConnect);
        }
        match self.addrs.write().unwrap().entry(node) {
            Entry::Vacant(entry) => {
                entry.insert(self.addr);
            }
            Entry::Occupied(_) => {
                return Err(OutgoingError::AlreadyConnected(node));
            }
        }

        self.outgoing.write().unwrap().get_mut(&self.addr).unwrap().node =
            Some(node);
        self.send_initial()?;
        self.send_updates(node)
    }
}
