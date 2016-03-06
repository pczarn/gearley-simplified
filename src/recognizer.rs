use std::collections::HashSet;
use std::cmp::Ordering;
use std::mem;

use cfg::symbol::Symbol;

use forest::{Bocage, Node};
use grammar::InternalGrammar;

pub struct Recognizer<'g> {
    grammar: &'g InternalGrammar,
    earleme: usize,
    current_set: Set,
    chart: Vec<Set>,
    bocage: Bocage,
    unique_dots: HashSet<(Dot, SetId)>,
}

#[derive(Clone, Debug)]
pub struct Item {
    dot: Dot,
    origin: SetId,
    node: Option<Node>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Dot {
    Predicted {
        id: RuleId,
        postdot: Symbol,
    },
    Medial {
        id: RuleId,
        postdot: Symbol,
    },
    Completed {
        id: RuleId,
    },
}

impl Dot {
    fn is_completed(&self) -> bool {
        if let &Dot::Completed { .. } = self {
            true
        } else {
            false
        }
    }

    fn rule_id(&self) -> RuleId {
        match *self {
            Dot::Predicted { id, .. } | Dot::Medial { id, .. } | Dot::Completed { id, .. } => id,
        }
    }
}

pub type RuleId = u32;
type SetId = usize;

type Set = Vec<Item>;

impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        (self.dot, self.origin) == (other.dot, other.origin)
    }
}

impl Eq for Item {}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some((self.origin, self.dot).cmp(&(other.origin, other.dot)))
    }
}

impl Ord for Item {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.origin, self.dot).cmp(&(other.origin, other.dot))
    }
}

impl<'g> Recognizer<'g> {
    pub fn new(grammar: &'g InternalGrammar) -> Self {
        let mut recognizer = Recognizer {
            grammar: grammar,
            earleme: 0,
            current_set: Set::new(),
            chart: vec![],
            bocage: Bocage::new(),
            unique_dots: HashSet::new(),
        };
        for rule in grammar.rules() {
            if rule.lhs() == grammar.start_symbol() {
                recognizer.current_set.push(Item {
                    dot: Dot::Predicted {
                        id: rule.id(),
                        postdot: rule.rhs0(),
                    },
                    origin: 0,
                    node: None,
                });
            }
        }
        recognizer.prediction_pass();
        recognizer.next_set();
        recognizer
    }

    pub fn parse(&mut self, input: &[Symbol]) {
        for &token in input {
            self.scan(token);
            assert!(self.advance());
        }
        assert!(self.is_finished());
    }

    pub fn is_finished(&mut self) -> bool {
        self.grammar.has_trivial_derivation() && self.earleme == 0 ||
        self.chart.last().as_ref().unwrap().iter().any(|item| {
            if let Dot::Completed { id } = item.dot {
                item.origin == 0 && self.grammar.rule(id).lhs() == self.grammar.start_symbol()
            } else {
                false
            }
        })
    }

    pub fn scan(&mut self, terminal: Symbol) {
        let node = self.bocage.leaf(terminal);
        let earleme = self.earleme;
        self.complete(terminal, earleme, node);
    }

    pub fn advance(&mut self) -> bool {
        if self.current_set.is_empty() {
            // Exhaustion of the parse.
            return false;
        }
        // Completion pass.
        self.completion_pass();
        // Prediction pass.
        self.prediction_pass();
        // Small optimization: keep items that are incomplete, because
        // complete items in this set will never matter from now on.
        self.chart.last_mut().as_mut().unwrap().retain(|item| !item.dot.is_completed());
        self.next_set();
        self.earleme += 1;
        true
    }

    fn next_set(&mut self) {
        self.chart.push(mem::replace(&mut self.current_set, Set::new()));
        self.unique_dots.clear();
    }

    fn completion_pass(&mut self) {
        let mut idx = 0;
        while idx < self.current_set.len() {
            if let Dot::Completed { id, .. } = self.current_set[idx].dot {
                let item = self.current_set[idx].clone();
                let node = item.node.clone().unwrap();
                self.complete(self.grammar.rule(id).lhs(), item.origin, node);
            }
            idx += 1;
        }
    }

    fn complete(&mut self, symbol: Symbol, set_id: SetId, new_node: Node) {
        for item in self.chart[set_id].iter() {
            let next_dot = match item.dot {
                Dot::Predicted { postdot, id } if postdot == symbol => {
                    if let Some(rhs1) = self.grammar.rule(id).rhs1() {
                        Dot::Medial {
                            postdot: rhs1,
                            id: id,
                        }
                    } else {
                        Dot::Completed { id: id }
                    }
                }
                Dot::Medial { postdot, id } if postdot == symbol => Dot::Completed { id: id },
                _ => continue,
            };
            let node = if let Some(left_node) = item.node.clone() {
                // medial
                self.bocage.product(next_dot.rule_id(),
                                    item.origin,
                                    self.earleme,
                                    left_node,
                                    new_node.clone())
            } else {
                // predicted
                new_node.clone()
            };
            if self.unique_dots.insert((next_dot, item.origin)) {
                self.current_set.push(Item {
                    dot: next_dot,
                    origin: item.origin,
                    node: Some(node),
                });
            }
        }
    }

    fn prediction_pass(&mut self) {
        let mut idx = 0;
        while idx < self.current_set.len() {
            match self.current_set[idx].dot {
                Dot::Predicted { postdot, .. } |
                Dot::Medial { postdot, .. } => {
                    self.predict(postdot);
                }
                _ => {}
            }
            idx += 1;
        }
    }

    fn predict(&mut self, symbol: Symbol) {
        for rule in self.grammar.rules() {
            if rule.lhs() == symbol {
                if self.unique_dots.insert((rule.predicted_dot(), self.earleme)) {
                    self.current_set.push(Item {
                        dot: rule.predicted_dot(),
                        origin: self.earleme,
                        node: None,
                    })
                }
            }
        }
    }
}
