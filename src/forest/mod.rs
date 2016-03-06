use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use cfg::symbol::Symbol;
use RuleId;

pub struct Bocage {
    map: HashMap<SpannedNonterminal, Node>,
}

type SpannedNonterminal = (u32, u32, Symbol);

#[derive(Clone, Debug)]
pub struct Node {
    node: Rc<RefCell<NodeRepr>>,
}

#[derive(Debug)]
pub enum NodeRepr {
    Sum {
        nonterminal: Symbol,
        summands: Vec<Node>,
    },
    Product {
        rule: RuleId,
        left: Node,
        right: Node,
    },
    Leaf {
        terminal: Symbol,
    },
}

impl Node {
    pub fn new(node_repr: NodeRepr) -> Self {
        Node { node: Rc::new(RefCell::new(node_repr)) }
    }
}

impl Bocage {
    pub fn new() -> Self {
        Bocage { map: HashMap::new() }
    }

    pub fn leaf(&mut self, terminal: Symbol) -> Node {
        Node::new(NodeRepr::Leaf { terminal: terminal })
    }

    pub fn product(&mut self,
                   id: RuleId,
                   _start: usize,
                   _end: usize,
                   left: Node,
                   right: Node)
                   -> Node {
        let product = Node::new(NodeRepr::Product {
            rule: id,
            left: left,
            right: right,
        });
        product
    }

    pub fn sum(&mut self, nonterminal: Symbol, start: u32, end: u32, product: Node) -> Node {
        let sum_node = self.map.entry((start, end, nonterminal)).or_insert_with(|| {
            Node::new(NodeRepr::Sum {
                nonterminal: nonterminal,
                summands: vec![],
            })
        });
        {
            let mut sum_node_mut = sum_node.node.borrow_mut();
            match &mut *sum_node_mut {
                &mut NodeRepr::Sum { ref mut summands, .. } => {
                    summands.push(product);
                }
                _ => unreachable!(),
            }
        };
        sum_node.clone()
    }
}
