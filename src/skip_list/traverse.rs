use super::node::{InternalNodeRef, Next, NodeRef};

pub fn get_parent<N: NodeRef>(node: N) -> Option<InternalNodeRef<N::Leaf>> {
    get_parent_with_count(node).map(|(p, _)| p)
}

pub fn get_parent_with_count<N: NodeRef>(
    mut node: N,
) -> Option<(InternalNodeRef<N::Leaf>, usize)> {
    let mut n = 0;
    loop {
        n += 1;
        node = match node.next()? {
            Next::Next(node) => node,
            Next::Parent(node) => return Some((node, n)),
        };
    }
}

pub fn get_nth_sibling<N: NodeRef>(mut node: N, n: usize) -> Option<N> {
    for _ in 0..n {
        node = node.next()?.into_next()?;
    }
    Some(node)
}

pub fn get_last_sibling<N: NodeRef>(mut node: N) -> N {
    loop {
        node = match node.next() {
            Some(Next::Next(node)) => node,
            _ => return node,
        }
    }
}

pub fn get_previous_node<N: NodeRef>(mut node: N) -> Option<Next<N>> {
    let mut count = 1;
    let parent = loop {
        node = match node.next()? {
            Next::Parent(node) => break node,
            Next::Next(node) => {
                count += 1;
                node
            }
        }
    };

    let position = parent.len.get() - count;
    if position == 0 {
        return Some(Next::Parent(parent));
    }

    let mut node: N = parent.down_as().unwrap();
    for _ in 1..position {
        node = node.next_sibling().unwrap();
    }
    Some(Next::Next(node))
}
