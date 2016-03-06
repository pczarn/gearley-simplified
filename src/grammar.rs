use std::ops::{Deref, DerefMut};
use std::iter;
use std::slice;

use cfg::*;
use cfg::history::*;
use cfg::symbol::Symbol;
use cfg::rule::builder::RuleBuilder;
use cfg::sequence::builder::SequenceRuleBuilder;
use cfg::sequence::Sequence;
use cfg::usefulness::Usefulness;
use cfg::cycles::Cycles;

use recognizer::Dot;

/// Drop-in replacement for cfg::Cfg that traces relations between
/// user-provided and internal grammars.
pub struct Grammar {
    inherit: Cfg<History>,
    start: Option<Symbol>,
}

pub struct InternalGrammar {
    rules: Vec<InternalRule>,
    start_symbol: Symbol,
    trivial_derivation: bool,
}

#[derive(Debug)]
struct InternalRule {
    lhs: Symbol,
    rhs0: Symbol,
    rhs1: Option<Symbol>,
    origin: RuleOrigin,
    nulling_eliminated: NullingEliminated,
}

#[derive(Clone, Default)]
pub struct History {
    origin: RuleOrigin,
    nulling_eliminated: NullingEliminated,
}

pub type RuleOrigin = Option<u32>;
pub type NullingEliminated = Option<(Symbol, bool)>;

impl History {
    pub fn new(id: u32, _len: usize) -> Self {
        History { origin: Some(id), ..History::default() }
    }

    pub fn origin(&self) -> RuleOrigin {
        self.origin
    }
}

impl Action for History {
    fn no_op(&self) -> Self {
        History::default()
    }
}

impl Binarize for History {
    fn binarize<R>(&self, _rule: &R, depth: usize) -> Self {
        let origin = if depth == 0 {
            self.origin
        } else {
            None
        };
        History {
            origin: origin,
            nulling_eliminated: self.nulling_eliminated,
        }
    }
}

impl EliminateNulling for History {
    fn eliminate_nulling<R>(&self, rule: &R, subset: BinarizedRhsSubset) -> Self
        where R: GrammarRule
    {
        if let BinarizedRhsSubset::All = subset {
            History { origin: self.origin, ..History::default() }
        } else {
            let right = if let BinarizedRhsSubset::Right = subset {
                true
            } else {
                false
            };
            let sym = rule.rhs()[right as usize];
            History { nulling_eliminated: Some((sym, right)), ..self.clone() }
        }
    }
}

impl RewriteSequence for History {
    type Rewritten = History;

    fn top(&self, _rhs: Symbol, _sep: Option<Symbol>, _new_rhs: &[Symbol]) -> Self {
        History { origin: self.origin, ..History::default() }
    }

    fn bottom(&self, _rhs: Symbol, _sep: Option<Symbol>, _new_rhs: &[Symbol]) -> Self {
        History::default()
    }
}

/// Default history.
pub struct BuildHistory {
    num_rules: usize,
}

impl BuildHistory {
    /// Creates default history.
    pub fn new(num_rules: usize) -> Self {
        BuildHistory { num_rules: num_rules }
    }
}

impl HistorySource<History> for BuildHistory {
    fn build(&mut self, _lhs: Symbol, rhs: &[Symbol]) -> History {
        // for sequences, rhs.len() will be 1 or 2
        let ret = History::new(self.num_rules as u32, rhs.len());
        self.num_rules += 1;
        ret
    }
}

#[derive(Clone)]
pub struct BinarizedGrammar {
    inherit: BinarizedCfg<History>,
    start: Option<Symbol>,
}

impl BinarizedGrammar {
    pub fn new() -> Self {
        BinarizedGrammar {
            inherit: BinarizedCfg::new(),
            start: None,
        }
    }

    pub fn set_start(&mut self, start: Symbol) {
        self.start = Some(start);
    }

    pub fn get_start(&self) -> Symbol {
        self.start.unwrap()
    }
}

impl Deref for BinarizedGrammar {
    type Target = BinarizedCfg<History>;
    fn deref(&self) -> &Self::Target {
        &self.inherit
    }
}

impl DerefMut for BinarizedGrammar {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inherit
    }
}

impl Grammar {
    pub fn new() -> Self {
        Grammar {
            inherit: Cfg::new(),
            start: None,
        }
    }

    pub fn set_start(&mut self, start: Symbol) {
        self.start = Some(start);
    }

    pub fn get_start(&self) -> Symbol {
        self.start.unwrap()
    }

    pub fn rule(&mut self, lhs: Symbol) -> RuleBuilder<&mut Cfg<History, History>, BuildHistory> {
        let rule_count = self.inherit.rules().count() + self.sequence_rules().len();
        self.inherit.rule(lhs).default_history(BuildHistory::new(rule_count))
    }

    pub fn sequence(&mut self,
                    lhs: Symbol)
                    -> SequenceRuleBuilder<History, &mut Vec<Sequence<History>>, BuildHistory> {
        let rule_count = self.inherit.rules().count() + self.sequence_rules().len();
        self.inherit.sequence(lhs).default_history(BuildHistory::new(rule_count))
    }

    pub fn binarize(&self) -> BinarizedGrammar {
        BinarizedGrammar {
            inherit: self.inherit.binarize(),
            start: self.start,
        }
    }

    pub fn into_internal_grammar(&self) -> InternalGrammar {
        InternalGrammar::from_grammar(self)
    }
}

impl Deref for Grammar {
    type Target = Cfg<History, History>;
    fn deref(&self) -> &Self::Target {
        &self.inherit
    }
}

impl DerefMut for Grammar {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inherit
    }
}

impl BinarizedGrammar {
    fn make_proper(mut self) -> BinarizedGrammar {
        let start = self.get_start();
        {
            let mut usefulness = Usefulness::new(&mut *self).reachable([start]);
            if !usefulness.all_useful() {
                println!("warning: grammar has useless rules");
                usefulness.remove_useless_rules();
            }
        };
        {
            let mut cycles = Cycles::new(&mut *self);
            if !cycles.cycle_free() {
                println!("warning: grammar has a cycle");
            }
            cycles.remove_cycles();
        }
        self
    }

    fn eliminate_nulling(mut self) -> (BinarizedGrammar, BinarizedCfg<History>) {
        let nulling_grammar = self.eliminate_nulling_rules();
        (self, nulling_grammar)
    }

    // useless?
    pub fn process(self) -> (BinarizedGrammar, bool) {
        let (grammar, nulling) = self.make_proper().eliminate_nulling();
        let trivial_derivation = nulling.rules().any(|rule| rule.lhs() == grammar.get_start());
        (grammar, trivial_derivation)
    }
}

impl InternalGrammar {
    pub fn from_grammar(grammar: &Grammar) -> Self {
        let (grammar, trivial_derivation) = grammar.binarize().process();
        Self::from_processed_grammar(grammar, trivial_derivation)
    }

    pub fn from_processed_grammar(grammar: BinarizedGrammar, trivial_derivation: bool) -> Self {
        let mut internal_rules = vec![];
        for rule in grammar.rules() {
            internal_rules.push(InternalRule {
                lhs: rule.lhs(),
                rhs0: rule.rhs()[0],
                rhs1: rule.rhs().get(1).cloned(),
                nulling_eliminated: rule.history().nulling_eliminated,
                origin: rule.history().origin,
            });
        }
        InternalGrammar {
            rules: internal_rules,
            trivial_derivation: trivial_derivation,
            start_symbol: grammar.get_start(),
        }
    }

    pub fn rules(&self) -> Rules {
        Rules { rules: self.rules.iter().enumerate() }
    }

    pub fn rule(&self, id: u32) -> RuleRef {
        RuleRef {
            id: id,
            rule: &self.rules[id as usize],
        }
    }

    pub fn start_symbol(&self) -> Symbol {
        self.start_symbol
    }

    pub fn has_trivial_derivation(&self) -> bool {
        self.trivial_derivation
    }
}

pub struct Rules<'a> {
    rules: iter::Enumerate<slice::Iter<'a, InternalRule>>,
}

pub struct RuleRef<'a> {
    id: u32,
    rule: &'a InternalRule,
}

impl<'a> Iterator for Rules<'a> {
    type Item = RuleRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.rules.next().map(|(id, rule)| {
            RuleRef {
                id: id as u32,
                rule: rule,
            }
        })
    }
}

impl<'a> RuleRef<'a> {
    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn lhs(&self) -> Symbol {
        self.rule.lhs
    }

    pub fn rhs0(&self) -> Symbol {
        self.rule.rhs0
    }

    pub fn rhs1(&self) -> Option<Symbol> {
        self.rule.rhs1
    }

    pub fn origin(&self) -> RuleOrigin {
        self.rule.origin
    }

    pub fn predicted_dot(&self) -> Dot {
        Dot::Predicted {
            id: self.id(),
            postdot: self.rhs0(),
        }
    }
}
