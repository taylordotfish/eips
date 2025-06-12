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

use diamond_types::AgentId;
use diamond_types::list::ListCRDT;
use diamond_types::list::encoding::EncodeOptions;

const ENCODE_OPTIONS: EncodeOptions<'static> = EncodeOptions {
    user_data: None,
    store_start_branch_content: false,
    store_inserted_content: true,
    store_deleted_content: false,
    compress_content: false,
    verbose: false,
};

pub struct Client {
    list: ListCRDT,
    agent: AgentId,
}

impl super::Client for Client {
    // This could be changed to `Rc<Vec<u8>>` to make cloning faster, but a
    // trial of this did not increase performance, even for large numbers of
    // clients.
    type Update = Vec<u8>;

    fn new(client_id: u64) -> Self {
        let mut list = ListCRDT::new();
        Self {
            agent: list.oplog.get_or_create_agent_id(&format!("{client_id}")),
            list,
        }
    }

    fn len(&self) -> usize {
        self.list.len()
    }

    fn text(&self) -> impl Iterator<Item = char> + '_ {
        self.list.branch.content().chars()
    }

    fn insert(&mut self, index: usize, c: char) -> Self::Update {
        let mut bytes = [0; 4];
        let version = self.list.oplog.local_version();
        self.list.insert(self.agent, index, c.encode_utf8(&mut bytes));
        self.list.oplog.encode_from(ENCODE_OPTIONS, &version)
    }

    fn remove(&mut self, index: usize) -> Self::Update {
        let version = self.list.oplog.local_version();
        self.list.delete_without_content(self.agent, index..index + 1);
        self.list.oplog.encode_from(ENCODE_OPTIONS, &version)
    }

    fn apply(&mut self, update: Self::Update) {
        self.list.merge_data_and_ff(&update).expect("bad remote update");
    }
}
