use std::cmp::Ordering;

use cfg::Symbol;
use forest::Node;

#[derive(Clone, Debug)]
pub struct Item {
    pub dot: Dot,
    pub origin: SetId,
    pub node: Option<Node>,
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

pub type DotWithOrigin = (Dot, SetId);
pub type RuleId = u32;
pub type SetId = usize;

impl Dot {
    pub fn is_completed(&self) -> bool {
        if let &Dot::Completed { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn rule_id(&self) -> RuleId {
        match *self {
            Dot::Predicted { id, .. } | Dot::Medial { id, .. } | Dot::Completed { id, .. } => id,
        }
    }
}

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
