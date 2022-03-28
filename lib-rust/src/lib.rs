extern crate bit_vec;
extern crate binary_heap_plus;

use std::convert::AsRef;
use std::cmp;
use std::mem;
use std::collections::{BTreeSet, BTreeMap};
use std::iter;

use bit_vec::BitVec;
use binary_heap_plus::BinaryHeap;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Symbol(u32);

pub struct Grammar {
    rules: Vec<Rule>,
    start_symbol: Option<Symbol>,
    symbol_source: SymbolSource,
}

pub struct BinarizedGrammar {
    rules: Vec<BinarizedRule>,
    start_symbol: Option<Symbol>,
    symbol_source: SymbolSource,
}

struct Rule {
    lhs: Symbol,
    rhs: Vec<Symbol>,
    id: usize,
}

#[derive(Clone)]
struct BinarizedRule {
    lhs: Symbol,
    rhs0: Symbol,
    rhs1: Option<Symbol>,
    source: Option<usize>,
}

#[derive(Clone)]
struct SymbolSource {
    next_symbol: Symbol,
    symbol_names: Vec<String>,
}

pub struct RuleBuilder<'a> {
    lhs: Symbol,
    rhs: Option<Vec<Symbol>>,
    id: Option<usize>,
    grammar: &'a mut Grammar,
}

struct Tables {
    prediction_matrix: Vec<BitVec>,
    start_symbol: Symbol,
    num_syms: usize,
    rules: Vec<BinarizedRule>,
    unary_completions: Vec<Vec<PredictionTransition>>,
    binary_completions: Vec<Vec<PredictionTransition>>,
    symbol_names: Vec<String>,
}

#[derive(Copy, Clone, Debug)]
struct PredictionTransition {
    symbol: Symbol,
    dot: usize,
}

// Forest

pub struct Forest {
    graph: Vec<Node>,
    // summands: u32,
    summands: Vec<Product>,
    eval: Vec<Option<usize>>,
}

#[derive(Copy, Clone)]
struct Product {
    action: u32,
    left_factor: NodeHandle,
    right_factor: Option<NodeHandle>,
}

#[derive(Clone)]
enum Node {
    Sum {
        summands: Vec<Product>,
    },
    Leaf {
        terminal: Symbol,
        values: u32,
    }
}

const NULL_ACTION: u32 = !0;

// Recognizer

pub struct Recognizer {
    tables: Tables,
    earley_chart: Vec<EarleySet>,
    next_set: EarleySet,
    complete: BinaryHeap<CompletedItem>,
    forest: Forest,
    finished_node: Option<NodeHandle>,
}

struct EarleySet {
    predicted: BitVec,
    medial: Vec<Item>,
}

struct Item {
    dot: usize,
    origin: usize,
    node: NodeHandle,
}

#[derive(Clone, Copy)]
struct CompletedItem {
    dot: usize,
    origin: usize,
    left_node: NodeHandle,
    right_node: Option<NodeHandle>,
}

#[derive(Eq, PartialEq, Ord, PartialOrd)]
enum MaybePostdot {
    Binary(Symbol),
    Unary,
}

#[derive(Clone, Copy, Debug)]
pub struct NodeHandle(usize);

impl PartialEq for CompletedItem {
    fn eq(&self, other: &Self) -> bool {
        (self.origin, self.dot) == (other.origin, other.dot)
    }
}

impl Eq for CompletedItem {}

impl PartialOrd for CompletedItem {
    fn partial_cmp(&self, other: &Self) -> Option<::std::cmp::Ordering> {
        Some((self.origin, self.dot).cmp(&(other.origin, other.dot)))
    }
}

impl Ord for CompletedItem {
    fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        (self.origin, self.dot).cmp(&(other.origin, other.dot))
    }
}

impl Symbol {
    fn usize(self) -> usize {
        self.0 as usize
    }
}

impl SymbolSource {
    fn new() -> Self {
        Self { next_symbol: Symbol(0), symbol_names: vec![] }
    }

    fn make_symbol(&mut self, name: &str) -> Symbol {
        let result = self.next_symbol;
        self.next_symbol.0 += 1;
        self.symbol_names.push(name.to_owned());
        result
    }

    fn make_n_symbols<F>(&mut self, count: usize, mut f: F) -> Vec<Symbol> where F: FnMut() -> String {
        (0..count).map(|_| self.make_symbol(&f()[..])).collect()
    }
}

impl EarleySet {
    fn new(num_syms: usize) -> Self {
        EarleySet {
            predicted: BitVec::from_elem(num_syms, false),
            medial: vec![],
        }
    }
}

impl Grammar {
    pub fn new() -> Self {
        Self {
            rules: vec![],
            start_symbol: None,
            symbol_source: SymbolSource::new(),
        }
    }

    pub fn make_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_source.make_symbol(name)
    }

    pub fn rule(&mut self, lhs: Symbol) -> RuleBuilder {
        RuleBuilder { grammar: self, lhs, rhs: None, id: None }
    }

    pub fn start_symbol(&mut self, symbol: Symbol) {
        self.start_symbol = Some(symbol);
    }

    pub fn binarize(&self) -> BinarizedGrammar {
        let mut gensym_n = 0;
        let mut symbol_source = self.symbol_source.clone();
        let binarized_rules = self.rules.iter().flat_map(|rule| {
            // Rewrite to a set of binarized rules.
            // From `LHS ⸬= A B C … X Y Z` to:
            // ____________________
            // | LHS ⸬= S0  Z
            // | S0  ⸬= S1  Y
            // | S1  ⸬= S2  X
            // | …
            // | Sm  ⸬= Sn  C
            // | Sn  ⸬= A   B
            let mut rules = vec![];

            match rule.rhs.len() {
                0 => unreachable!(),
                1 => {
                    rules.push(BinarizedRule {
                        lhs: rule.lhs,
                        rhs0: rule.rhs[0],
                        rhs1: None,
                        source: Some(rule.id),
                    });
                }
                rhs_count => {
                    let num_additional_symbols = rhs_count - 2;
                    let gensyms = symbol_source.make_n_symbols(num_additional_symbols, || {
                        let sym_name = format!("g{}", gensym_n);
                        gensym_n += 1;
                        sym_name
                    });
                    let lhs_iter = gensyms.iter().cloned().chain(iter::once(rule.lhs));
                    let mut rhs1_iter = rule.rhs.iter().cloned();
                    let rhs0_iter = rhs1_iter.next().into_iter().chain(gensyms.iter().cloned());

                    rules.extend(
                        lhs_iter.zip(rhs0_iter).zip(rhs1_iter).map(|((lhs, rhs0), rhs1)| {
                            BinarizedRule {
                                lhs,
                                rhs0,
                                rhs1: Some(rhs1),
                                source: if lhs == rule.lhs { Some(rule.id) } else { None },
                            }
                        })
                    );
                }
            }
            rules.into_iter()
        }).collect();
        BinarizedGrammar {
            rules: binarized_rules,
            symbol_source,
            start_symbol: self.start_symbol,
        }
    }
}

impl BinarizedGrammar {
    fn sort_rules(&mut self) {
        self.rules.sort_by(|a, b| a.lhs.cmp(&b.lhs));
    }
}

impl<'a> RuleBuilder<'a> {
    pub fn rhs<R>(mut self, rhs: R) -> Self where R: AsRef<[Symbol]> {
        assert!(rhs.as_ref().len() > 0, "empty rules are not accepted");
        self.rhs = Some(rhs.as_ref().to_vec());
        self
    }

    pub fn id(mut self, id: usize) -> Self {
        self.id = Some(id);
        self
    }

    pub fn build(self) {
        self.grammar.rules.push(
            Rule {
                lhs: self.lhs,
                rhs: self.rhs.unwrap(),
                id: self.id.unwrap_or(self.grammar.rules.len()),
            }
        );
    }
}

// Implementation for the recognizer.
//
// The recognizer has a chart of earley sets (Vec<EarleySet>) as well as the last set (next_set).
//
// A typical loop that utilizes the recognizer:
// 
// - for character in string { 
// 1.   recognizer.begin_earleme();
// 2.   recognizer.scan(token_to_symbol(character), values());
//        2a. complete
// 3.   recognizer.end_earleme();
//        3a. self.complete_all_sums_entirely();
//        3b. self.sort_medial_items();
//        3c. self.prediction_pass();
// - }
//
impl Recognizer {
    pub fn new(mut grammar: BinarizedGrammar) -> Self {
        grammar.sort_rules();
        let mut result = Self {
            tables: Tables::new(&grammar),
            earley_chart: vec![],
            next_set: EarleySet::new(grammar.symbol_source.next_symbol.usize()),
            forest: Forest::new(&grammar),
            // complete: BinaryHeap::new_by_key(Box::new(|completed_item| (completed_item.origin, completed_item.dot))),
            complete: BinaryHeap::with_capacity(64),
            finished_node: None,
        };
        result.initialize();
        result
    }

    fn initialize(&mut self) {
        // self.earley_chart.push(EarleySet {
        //     predicted: self.tables.prediction_matrix[self.tables.start_symbol.usize()].clone(),
        //     medial: vec![],
        // });
        let es = EarleySet {
            predicted: self.tables.prediction_matrix[self.tables.start_symbol.usize()].clone(),
            medial: vec![],
        };
        // self.earley_chart.push(mem::replace(&mut self.next_set, EarleySet::new(self.tables.num_syms)));
        self.earley_chart.push(es);
    }

    pub fn begin_earleme(&mut self) {
        // nothing to do
    }

    pub fn scan(&mut self, terminal: Symbol, values: u32) {
        let node = self.forest.leaf(terminal, self.earleme() + 1, values);
        self.complete(self.earleme(), terminal, node);
    }

    pub fn end_earleme(&mut self) -> bool {
        if self.is_exhausted() {
            false
        } else {
            // Completion pass, which saves successful parses.
            self.finished_node = None;
            self.complete_all_sums_entirely();
            // Do the rest.
            self.sort_medial_items();
            self.prediction_pass();
            self.earley_chart.push(mem::replace(&mut self.next_set, EarleySet::new(self.tables.num_syms)));
            true
        }
    }

    fn is_exhausted(&self) -> bool {
        self.next_set.medial.len() == 0 && self.complete.is_empty()
    }

    fn complete_all_sums_entirely(&mut self) {
        while let Some(&ei) = self.complete.peek() {
            let lhs_sym = self.tables.get_lhs(ei.dot);
            while let Some(&ei2) = self.complete.peek() {
                if ei.origin == ei2.origin && lhs_sym == self.tables.get_lhs(ei2.dot) {
                    self.forest.push_summand(ei2);
                    self.complete.pop();
                } else {
                    break;
                }
            }
            let node = self.forest.sum(lhs_sym, ei.origin);
            if ei.origin == 0 && lhs_sym == self.tables.start_symbol {
                self.finished_node = Some(node);
            }
            self.complete(ei.origin, lhs_sym, node);
        }
    }

    /// Sorts medial items with deduplication.
    fn sort_medial_items(&mut self) {
        let tables = &self.tables;
        // Build index by postdot
        // These medial positions themselves are sorted by postdot symbol.
        self.next_set.medial.sort_unstable_by(|a, b| {
            (tables.get_rhs1_cmp(a.dot), a.dot, a.origin).cmp(&(
                tables.get_rhs1_cmp(b.dot),
                b.dot,
                b.origin,
            ))
        });
    }

    fn prediction_pass(&mut self) {
        // Iterate through medial items in the current set.
        let iter = self.next_set.medial.iter();
        // For each medial item in the current set, predict its postdot symbol.
        let destination = &mut self.next_set.predicted;
        for ei in iter {
            let postdot = if let Some(rhs1) = self.tables.get_rhs1(ei.dot) {
                rhs1
            } else {
                continue;
            };
            if !destination[postdot.usize()] {
                // Prediction happens here. We would prefer to call `self.predict`, but we can't,
                // because `self.medial` is borrowed by `iter`.
                let source = &self.tables.prediction_matrix[postdot.usize()];
                destination.or(source);
            }
        }
    }

    fn complete(&mut self, earleme: usize, symbol: Symbol, node: NodeHandle) {
        if self.earley_chart[earleme].predicted[symbol.usize()] {
            self.complete_medial_items(earleme, symbol, node);
            self.complete_unary_predictions(earleme, symbol, node);
            self.complete_binary_predictions(earleme, symbol, node);
        }
    }

    fn complete_medial_items(&mut self, earleme: usize, symbol: Symbol, right_node: NodeHandle) {
        let medial = &self.earley_chart[earleme].medial;

        let inner_start = {
            // we use binary search to narrow down the range of items.
            let set_idx = medial.binary_search_by(|ei| {
                (self.tables.get_rhs1(ei.dot), cmp::Ordering::Greater).cmp(&(Some(symbol), cmp::Ordering::Less))
            });
            match set_idx {
                Ok(idx) | Err(idx) => idx,
            }
        };

        // The range contains items that have the same RHS1 symbol.
        let inner_end = medial[inner_start..]
            .iter()
            .take_while(|ei| self.tables.get_rhs1(ei.dot) == Some(symbol))
            .count();
        for item in &medial[inner_start .. inner_start + inner_end] {
            self.complete.push(CompletedItem {
                dot: item.dot,
                origin: item.origin,
                left_node: item.node,
                right_node: Some(right_node),
            });
        }
    }

    fn complete_unary_predictions(&mut self, earleme: usize, symbol: Symbol, node: NodeHandle) {
        for trans in self.tables.unary_completions(symbol) {
            if self.earley_chart[earleme].predicted.get(trans.symbol.usize()).unwrap_or(false) {
                // No checks for uniqueness, because `medial` will be deduplicated.
                // from A ::= • B
                // to   A ::=   B •
                self.complete.push(CompletedItem {
                    origin: earleme,
                    dot: trans.dot,
                    left_node: node,
                    right_node: None,
                });
            }
        }
    }

    fn complete_binary_predictions(&mut self, earleme: usize, symbol: Symbol, node: NodeHandle) {
        for trans in self.tables.binary_completions(symbol) {
            if self.earley_chart[earleme].predicted.get(trans.symbol.usize()).unwrap_or(false) {
                // No checks for uniqueness, because `medial` will be deduplicated.
                // from A ::= • B   C
                // to   A ::=   B • C
                // Where C is terminal or nonterminal.
                self.next_set.medial.push(Item {
                    origin: earleme,
                    dot: trans.dot,
                    node: node,
                });
            }
        }
    }

    fn earleme(&self) -> usize {
        self.earley_chart.len() - 1
    }

    pub fn finished_node(&self) -> Option<NodeHandle> {
        self.finished_node
    }

    pub fn log_last_earley_set(&self) {
        let dots = self.dots_for_log(self.earley_chart.last().unwrap());
        for (rule_id, dots) in dots {
            print!("{} ::= ", self.tables.symbol_names[self.tables.get_lhs(rule_id).usize()]);
            if let Some(origins) = dots.get(&0) {
                print!("{:?}", origins);
            }
            print!(" {} ", self.tables.symbol_names[self.tables.get_rhs0(rule_id).unwrap().usize()]);
            if let Some(origins) = dots.get(&1) {
                print!("{:?}", origins);
            }
            if let Some(rhs1) = self.tables.get_rhs1(rule_id) {
                print!(" {} ", self.tables.symbol_names[rhs1.usize()]);
            }
            println!();
        }
        println!();
    }

    pub fn terminal_name(&self, terminal: Symbol) -> &str {
        &self.tables.symbol_names[terminal.usize()][..]
    }

    fn log_earley_set_diff(&self) {
        let dots_last_by_id = self.dots_for_log(self.earley_chart.last().unwrap());
        let mut dots_next_by_id = self.dots_for_log(&self.next_set);
        let mut rule_ids: BTreeSet<usize> = BTreeSet::new();
        rule_ids.extend(dots_last_by_id.keys());
        rule_ids.extend(dots_next_by_id.keys());
        for item in self.complete.iter() {
            let position = if self.tables.get_rhs1(item.dot).is_some() { 2 } else { 1 };
            dots_next_by_id.entry(item.dot).or_insert(BTreeMap::new()).entry(position).or_insert(BTreeSet::new()).insert(item.origin);
        }
        let mut empty_diff = true;
        for rule_id in rule_ids {
            let dots_last = dots_last_by_id.get(&rule_id);
            let dots_next = dots_next_by_id.get(&rule_id);
            if dots_last == dots_next {
                continue;
            }
            empty_diff = false;
            print!("from {} ::= ", self.tables.symbol_names[self.tables.get_lhs(rule_id).usize()]);
            if let Some(origins) = dots_last.and_then(|d| d.get(&0)) {
                print!("{:?}", origins);
            }
            print!(" {} ", self.tables.symbol_names[self.tables.get_rhs0(rule_id).unwrap().usize()]);
            if let Some(origins) = dots_last.and_then(|d| d.get(&1)) {
                print!("{:?}", origins);
            }
            if let Some(rhs1) = self.tables.get_rhs1(rule_id) {
                print!(" {} ", self.tables.symbol_names[rhs1.usize()]);
            }
            println!();
            print!("to   {} ::= ", self.tables.symbol_names[self.tables.get_lhs(rule_id).usize()]);
            if let Some(origins) = dots_next.and_then(|d| d.get(&0)) {
                print!("{:?}", origins);
            }
            print!(" {} ", self.tables.symbol_names[self.tables.get_rhs0(rule_id).unwrap().usize()]);
            if let Some(origins) = dots_next.and_then(|d| d.get(&1)) {
                print!("{:?}", origins);
            }
            if let Some(rhs1) = self.tables.get_rhs1(rule_id) {
                print!(" {} ", self.tables.symbol_names[rhs1.usize()]);
            }
            if let Some(origins) = dots_next.and_then(|d| d.get(&2)) {
                print!("{:?}", origins);
            }
            println!();
        }
        if empty_diff {
            println!("no diff");
            println!();
        } else {
            println!();
        }
    }

    fn dots_for_log(&self, es: &EarleySet) -> BTreeMap<usize, BTreeMap<usize, BTreeSet<usize>>> {
        let mut dots = BTreeMap::new();
        for (i, rule) in self.tables.rules.iter().enumerate() {
            if es.predicted[rule.lhs.usize()] {
                dots.entry(i).or_insert(BTreeMap::new()).entry(0).or_insert(BTreeSet::new()).insert(self.earleme());
            }
        }
        for item in &es.medial {
            dots.entry(item.dot).or_insert(BTreeMap::new()).entry(1).or_insert(BTreeSet::new()).insert(item.origin);
        }
        dots
    }

    pub fn forest_mut(&mut self) -> &mut Forest {
        &mut self.forest
    }
}

impl Tables {
    fn new(grammar: &BinarizedGrammar) -> Self {
        let mut result = Self {
            prediction_matrix: vec![],
            start_symbol: grammar.start_symbol.expect("unset start symbol"),
            num_syms: grammar.symbol_source.next_symbol.usize(),
            rules: vec![],
            unary_completions: vec![],
            binary_completions: vec![],
            symbol_names: grammar.symbol_source.symbol_names.clone(),
        };
        result.populate(&grammar);
        result
    }

    fn populate(&mut self, grammar: &BinarizedGrammar) {
        self.populate_prediction_matrix(grammar);
        self.populate_rules(grammar);
        self.populate_completions(grammar);
    }

    fn populate_prediction_matrix(&mut self, grammar: &BinarizedGrammar) {
        self.prediction_matrix.resize(self.num_syms, BitVec::from_elem(self.num_syms, false));
        for rule in &grammar.rules {
            self.prediction_matrix[rule.lhs.usize()].set(rule.rhs0.usize(), true);
        }
        self.reflexive_closure();
        self.transitive_closure();
    }

    fn reflexive_closure(&mut self) {
        for i in 0 .. self.num_syms {
            self.prediction_matrix[i].set(i, true);
        }
    }

    fn transitive_closure(&mut self) {
        for pos in 0 .. self.num_syms {
            let (rows0, rows1) = self.prediction_matrix.split_at_mut(pos);
            let (rows1, rows2) = rows1.split_at_mut(1);
            for dst_row in rows0.iter_mut().chain(rows2.iter_mut()) {
                if dst_row[pos] {
                    dst_row.or(&rows1[0]);
                }
            }
        }
    }

    fn populate_rules(&mut self, grammar: &BinarizedGrammar) {
        self.rules = grammar.rules.clone();
    }

    fn populate_completions(&mut self, grammar: &BinarizedGrammar) {
        self.unary_completions.resize(self.num_syms, vec![]);
        self.binary_completions.resize(self.num_syms, vec![]);
        for (i, rule) in grammar.rules.iter().enumerate() {
            if rule.rhs1.is_some() {
                self.binary_completions[rule.rhs0.usize()].push(PredictionTransition {
                    symbol: rule.lhs,
                    dot: i,
                });
            } else {
                self.unary_completions[rule.rhs0.usize()].push(PredictionTransition {
                    symbol: rule.lhs,
                    dot: i,
                });
            }
        }
    }

    fn get_rhs0(&self, n: usize) -> Option<Symbol> {
        self.rules.get(n).map(|rule| rule.rhs0)
    }

    fn get_rhs1(&self, n: usize) -> Option<Symbol> {
        self.rules.get(n).and_then(|rule| rule.rhs1)
    }

    fn get_rhs1_cmp(&self, dot: usize) -> MaybePostdot {
        match self.rules[dot].rhs1 {
            None => MaybePostdot::Unary,
            Some(rhs1) => MaybePostdot::Binary(rhs1),
        }
    }

    fn get_lhs(&self, n: usize) -> Symbol {
        self.rules[n].lhs
    }

    fn unary_completions(&self, symbol: Symbol) -> &[PredictionTransition] {
        &self.unary_completions[symbol.usize()][..]
    }

    fn binary_completions(&self, symbol: Symbol) -> &[PredictionTransition] {
        &self.binary_completions[symbol.usize()][..]
    }
}

impl Forest {
    fn new(grammar: &BinarizedGrammar) -> Self {
        Self {
            graph: vec![],
            summands: vec![],
            eval: grammar.rules.iter().map(|rule| rule.source).collect(),
        }
    }

    fn leaf(&mut self, terminal: Symbol, _x: usize, values: u32) -> NodeHandle {
        let handle = NodeHandle(self.graph.len());
        self.graph.push(Node::Leaf {
            terminal,
            values,
        });
        handle
    }

    fn push_summand(&mut self, item: CompletedItem) {
        self.summands.push(Product {
            action: self.get_eval(item.dot).unwrap_or(NULL_ACTION),
            left_factor: item.left_node,
            right_factor: item.right_node,
        });
    }

    fn sum(&mut self, _lhs_sym: Symbol, _origin: usize) -> NodeHandle {
        let handle = NodeHandle(self.graph.len());
        self.graph.push(Node::Sum {
            summands: mem::replace(&mut self.summands, vec![]),
        });
        handle
    }

    fn get_eval(&self, dot: usize) -> Option<u32> {
        self.eval[dot].map(|id| id as u32)
    }
}

pub struct Evaluator<F, G> {
    eval_product: F,
    eval_leaf: G,
}

impl<T, F, G> Evaluator<F, G>
    where F: Fn(u32, &[T]) -> T + Copy,
          G: Fn(Symbol, u32) -> T + Copy
{
    pub fn new(eval_product: F, eval_leaf: G) -> Self {
        Self {
            eval_product,
            eval_leaf,
        }
    }

    pub fn evaluate(&mut self, forest: &mut Forest, finished_node: NodeHandle) -> T
        where F: Fn(u32, &[T]) -> T,
              G: Fn(Symbol, u32) -> T,
    {
        let res = self.evaluate_rec(forest, finished_node);
        res.into_iter().next().expect("incorrect eval count")
    }

    fn evaluate_rec(&mut self, forest: &mut Forest, handle: NodeHandle) -> Vec<T> {
        match &forest.graph[handle.0] {
            &Node::Sum { ref summands, .. } => {
                assert_eq!(summands.len(), 1);
                let product = summands[0];
                let mut result = self.evaluate_rec(forest, product.left_factor);
                if let Some(factor) = product.right_factor {
                    let v = self.evaluate_rec(forest, factor);
                    result.extend(v);
                }
                if product.action != NULL_ACTION {
                    vec![(self.eval_product)(product.action as u32, &result[..])]
                } else {
                    result
                }
            }
            &Node::Leaf { terminal, values } => {
                vec![(self.eval_leaf)(terminal, values)]
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Grammar, Recognizer, Evaluator};

    use logos::Logos;

    fn calc(expr: &str) -> f64 {
        let mut grammar = Grammar::new();
        let sum = grammar.make_symbol("sum");
        let op_sum = grammar.make_symbol("op_sum");
        let factor = grammar.make_symbol("factor");
        let op_factor = grammar.make_symbol("op_factor");
        let lparen = grammar.make_symbol("lparen");
        let rparen = grammar.make_symbol("rparen");
        let expr_sym = grammar.make_symbol("expr_sym");
        let op_minus = grammar.make_symbol("op_minus");
        let op_plus = grammar.make_symbol("op_plus");
        let number = grammar.make_symbol("number");
        let whole = grammar.make_symbol("whole");
        let digit = grammar.make_symbol("digit");
        let dot = grammar.make_symbol("dot");
        // sum ::= sum [+-] factor
        // sum ::= factor
        // factor ::= factor [*/] expr
        // factor ::= expr
        // expr ::= '(' sum ')' | '-' sum | number
        // number ::= whole | whole '.' whole
        // whole ::= whole [0-9] | [0-9]
        grammar.rule(sum).rhs([sum, op_sum, factor]).id(0).build();
        grammar.rule(sum).rhs([factor]).id(1).build();
        grammar.rule(factor).rhs([factor, op_factor, expr_sym]).id(2).build();
        grammar.rule(factor).rhs([expr_sym]).id(3).build();
        grammar.rule(expr_sym).rhs([lparen, sum, rparen]).id(4).build();
        grammar.rule(expr_sym).rhs([op_minus, sum]).id(5).build();
        grammar.rule(expr_sym).rhs([number]).id(6).build();
        grammar.rule(number).rhs([whole]).id(7).build();
        grammar.rule(number).rhs([whole, dot, whole]).id(8).build();
        grammar.rule(whole).rhs([whole, digit]).id(9).build();
        grammar.rule(whole).rhs([digit]).id(10).build();
        grammar.rule(op_sum).rhs([op_minus]).id(11).build();
        grammar.rule(op_sum).rhs([op_plus]).id(12).build();
        grammar.start_symbol(sum);
        let binarized_grammar = grammar.binarize();
        let mut recognizer = Recognizer::new(binarized_grammar);

        #[derive(Logos)]
        enum Token {
            #[token("-")]
            OpMinus,
            #[token(".")]
            Dot,
            #[regex("[0-9]")]
            Digit,
            #[token("(")]
            Lparen,
            #[token(")")]
            Rparen,
            #[token("*")]
            #[token("/")]
            OpFactor,
            #[token("+")]
            OpPlus,
            #[token(" ", logos::skip)]
            #[error]
            Error,
        }
        let mut lex = Token::lexer(expr);
        let mut spans = vec![];
        while let Some(token) = lex.next() {
            spans.push(lex.span());
            recognizer.begin_earleme();
            let terminal = match token {
                Token::OpMinus => op_minus,
                Token::Dot => dot,
                Token::Digit => digit,
                Token::Lparen => lparen,
                Token::Rparen => rparen,
                Token::OpFactor => op_factor,
                Token::OpPlus => op_plus,
                Token::Error => {
                    let span = lex.span();
                    panic!("lexing error at {:?}", span);
                },
            };
            recognizer.scan(terminal, spans.len() as u32 - 1);
            assert!(recognizer.end_earleme(), "parse failed at string {} at input range {:?}", lex.slice(), lex.span());
        }
        let finished_node = recognizer.finished_node().expect("parse failed");
        let mut evaluator = Evaluator::new(
            |rule_id, args: &[(f64, u32)]| {
                match rule_id {
                    0 => {
                        let (left, op, right) = (args[0].0, args[1].1, args[2].0);
                        let op = op as u8 as char;
                        if op == '-' {
                            (left - right, 0)
                        } else if op == '+' {
                            (left + right, 0)
                        } else {
                            panic!()
                        }
                    }
                    1 => {
                        (args[0].0, 0)
                    }
                    2 => {
                        let (left, op, right) = (args[0].0, args[1].1, args[2].0);
                        if op as u8 as char == '*' {
                            (left * right, 0)
                        } else if op as u8 as char == '/' {
                            (left / right, 0)
                        } else {
                            panic!()
                        }
                    }
                    3 => {
                        (args[0].0, 0)
                    }
                    4 => {
                        (args[1].0, 0)
                    }
                    5 => {
                        (-args[1].0, 0)
                    }
                    6 => {
                        (args[0].0, 0)
                    }
                    7 => {
                        (args[0].0, 0)
                    }
                    8 => {
                        let (left, right, decimals) = (args[0].0, args[2].0, args[2].1 as i32);
                        (left + right * 10f64.powi(-decimals), 0)
                    }
                    9 => {
                        (args[0].0 * 10f64 + args[1].0, args[0].1 + 1)
                    }
                    10 => {
                        (args[0].0, 1)
                    }
                    11 => {
                        (0f64, '-' as u32)
                    }
                    12 => {
                        (0f64, '+' as u32)
                    }
                    other => panic!("unknown rule id {}", other)
                }
            },
            |terminal, values| {
                let span = spans[values as usize].clone();
                let slice = &expr[span];
                if terminal == digit {
                    ((slice.chars().nth(0).unwrap() as u32 - ('0' as u32)) as f64, 1)
                } else if terminal == op_factor {
                    (0f64, slice.chars().nth(0).unwrap() as u32)
                } else {
                    (0f64, 0)
                }
            }
        );
        evaluator.evaluate(&mut recognizer.forest, finished_node).0
    }
    
    fn test(expr: &str, expected: f64) {
        let result = calc(expr);
        assert_eq!(result, expected);
    }

    #[test]
    fn it_works() {
        test("((2.33 / (2.9+3.5)*4) - -6)", 7.45625);
    }
}
