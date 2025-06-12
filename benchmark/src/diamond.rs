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
