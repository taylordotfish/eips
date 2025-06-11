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

use crate::bincode::{self, ErrorExt};
use crate::queue::Sender;
use crate::{Document, Node, NodeAddr, ReceivedUpdate};
use crate::{IncomingMap, OutgoingMap, ShutdownOnDrop};
use std::fmt::{self, Display};
use std::mem;
use std::net::{Shutdown, SocketAddr, TcpStream};
use std::sync::{Arc, RwLock};
use std::thread::JoinHandle;

pub struct Incoming {
    pub stream: Arc<TcpStream>,
    pub node: Option<Node>,
    pub thread: JoinHandle<()>,
}

pub struct IncomingThread {
    pub addr: SocketAddr,
    pub local_node: Node,
    pub document: Arc<RwLock<Document>>,
    pub updates: Sender<ReceivedUpdate>,
    pub outgoing: Arc<RwLock<OutgoingMap>>,
    pub incoming: Arc<RwLock<IncomingMap>>,
}

#[derive(Debug)]
pub enum IncomingError {
    ReadFailed(bincode::Error, NodeAddr),
    WriteFailed(bincode::Error, NodeAddr),
}

impl Display for IncomingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ReadFailed(e, addr) => write!(
                f,
                "could not read from {addr}: {}",
                ErrorExt::display(&**e),
            ),
            Self::WriteFailed(e, addr) => write!(
                f,
                "could not write to {addr}: {}",
                ErrorExt::display(&**e),
            ),
        }
    }
}

impl IncomingThread {
    pub fn run(self) -> (Option<Node>, Result<(), IncomingError>) {
        let shutdown = ShutdownOnDrop;
        let stream = self.incoming.read().unwrap()[&self.addr].stream.clone();
        let mut node = None;
        let result = RunningIncomingThread {
            addr: self.addr,
            local_node: self.local_node,
            stream: &stream,
            document: &self.document,
            updates: self.updates,
            outgoing: &self.outgoing,
            incoming: &self.incoming,
            node: &mut node,
        }
        .run();
        ignore_error!(stream.shutdown(Shutdown::Both));
        self.incoming.write().unwrap().remove(&self.addr);
        mem::forget(shutdown);
        (node, result)
    }
}

struct RunningIncomingThread<'a> {
    addr: SocketAddr,
    local_node: Node,
    stream: &'a TcpStream,
    document: &'a RwLock<Document>,
    updates: Sender<ReceivedUpdate>,
    outgoing: &'a RwLock<OutgoingMap>,
    incoming: &'a RwLock<IncomingMap>,
    node: &'a mut Option<Node>,
}

impl RunningIncomingThread<'_> {
    fn node_addr(&self) -> NodeAddr {
        NodeAddr::new(self.addr, *self.node)
    }

    fn run(mut self) -> Result<(), IncomingError> {
        let node: Node = match bincode::deserialize(&mut self.stream) {
            Ok(node) => node,
            Err(e) => {
                return Err(IncomingError::ReadFailed(e, self.node_addr()));
            }
        };
        *self.node = Some(node);

        let local = self.local_node;
        if let Err(e) = bincode::serialize(&local, &mut self.stream) {
            return Err(IncomingError::WriteFailed(e, self.node_addr()));
        }
        self.incoming.write().unwrap().get_mut(&self.addr).unwrap().node =
            Some(node);

        rl_println!("Node {node} ({}) connected (incoming)", self.addr);
        loop {
            let msg = match bincode::deserialize(&mut self.stream) {
                Ok(msg) => msg,
                Err(e) => {
                    if e.io_error_kind()
                        == Some(std::io::ErrorKind::UnexpectedEof)
                    {
                        return Ok(());
                    }
                    return Err(IncomingError::ReadFailed(
                        e,
                        self.node_addr(),
                    ));
                }
            };
            crate::handle_message(msg, crate::HandleMessage {
                node,
                document: self.document,
                updates: &mut self.updates,
                outgoing: self.outgoing,
            });
        }
    }
}
