extern crate bit_vec;
extern crate binary_heap_plus;

use std::convert::AsRef;
use std::cmp;
use std::mem;
use std::collections::{BTreeSet, BTreeMap};

use bit_vec::BitVec;
use binary_heap_plus::BinaryHeap;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Symbol(u32);

struct Grammar {
    rules: Vec<Rule>,
    start_symbol: Option<Symbol>,
    symbol_source: SymbolSource,
}

struct BinarizedGrammar {
    rules: Vec<BinarizedRule>,
    start_symbol: Option<Symbol>,
    symbol_source: SymbolSource,
}

struct Rule {
    lhs: Symbol,
    rhs: Vec<Symbol>,
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

struct RuleBuilder<'a> {
    lhs: Symbol,
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

#[derive(Copy, Clone)]
struct Product {
    action: u32,
    left_factor: NodeHandle,
    right_factor: Option<NodeHandle>,
}

enum Node {
    Sum {
        nonterminal: Symbol,
        summands: Vec<Product>,
    },
    Leaf {
        terminal: Symbol,
        values: u32,
    }
}

struct Forest {
    graph: Vec<Node>,
    // summands: u32,
    summands: Vec<Product>,
    eval: Vec<Option<usize>>,
}

struct Recognizer {
    tables: Tables,
    earley_chart: Vec<EarleySet>,
    next_set: EarleySet,
    complete: BinaryHeap<CompletedItem>,
    forest: Forest,
    // grammar: Grammar,
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

#[derive(Clone, Copy)]
struct NodeHandle(usize);

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
    fn new() -> Self {
        Self {
            rules: vec![],
            start_symbol: None,
            symbol_source: SymbolSource::new(),
        }
    }

    fn make_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_source.make_symbol(name)
    }

    fn rule(&mut self, lhs: Symbol) -> RuleBuilder {
        RuleBuilder { grammar: self, lhs }
    }

    fn start_symbol(&mut self, symbol: Symbol) {
        self.start_symbol = Some(symbol);
    }

    fn binarize(&self) -> BinarizedGrammar {
        let mut gensym_n = 0;
        let mut symbol_source = self.symbol_source.clone();
        let binarized_rules = self.rules.iter().enumerate().flat_map(|(rule_id, rule)| {
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
                        source: Some(rule_id),
                    });
                }
                2 => {
                    rules.push(BinarizedRule {
                        lhs: rule.lhs,
                        rhs0: rule.rhs[0],
                        rhs1: Some(rule.rhs[1]),
                        source: Some(rule_id),
                    });
                }
                rhs_count => {
                    let num_additional_rules = rhs_count.saturating_sub(3);
                    let mut next_sym = symbol_source.make_symbol(&format!("g{}", gensym_n));
                    gensym_n += 1;
                    rules.push(BinarizedRule {
                        lhs: next_sym,
                        rhs0: rule.rhs[0],
                        rhs1: Some(rule.rhs[1]),
                        source: None,
                    });
                    for i in 0..num_additional_rules {
                        let next_sym_2 = symbol_source.make_symbol(&format!("g{}", gensym_n));
                        gensym_n += 1;
                        rules.push(BinarizedRule {
                            lhs: next_sym_2,
                            rhs0: next_sym,
                            rhs1: Some(rule.rhs[2 + i]),
                            source: None,
                        });
                        next_sym = next_sym_2;
                    }
                    rules.push(BinarizedRule {
                        lhs: rule.lhs,
                        rhs0: next_sym,
                        rhs1: Some(rule.rhs.last().cloned().unwrap()),
                        source: Some(rule_id),
                    });
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
    fn rhs<R>(self, rhs: R) where R: AsRef<[Symbol]> {
        assert!(rhs.as_ref().len() > 0, "empty rules are not accepted");
        self.grammar.rules.push(
            Rule {
                lhs: self.lhs,
                rhs: rhs.as_ref().to_vec(),
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
// 3.   recognizer.end_earleme();
//        3a. self.complete_all_sums_entirely();
//        3b. self.sort_medial_items();
//        3c. self.prediction_pass();
// - }
//
// 1. nothing is done
// 2. completion of last-set that 
// 3. 
//    3a. 
impl Recognizer {
    fn new(mut grammar: BinarizedGrammar) -> Self {
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
    }

    fn begin_earleme(&mut self) {
        // nothing to do
    }

    fn scan(&mut self, terminal: Symbol, values: u32) {
        let node = self.forest.leaf(terminal, self.earleme() + 1, values);
        self.complete(self.earleme(), terminal, node);
    }

    fn end_earleme(&mut self) -> bool {
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
        self.earley_chart.last().unwrap().medial.len() == 0 && self.complete.is_empty()
    }

    fn complete_all_sums_entirely(&mut self) {
        while let Some(&ei) = self.complete.peek() {
            let lhs_sym = self.tables.get_lhs(ei.dot);
            while let Some(&ei2) = self.complete.peek() {
                if ei.origin == ei2.origin && lhs_sym == self.tables.get_lhs(ei2.dot) {
                    self.forest.push_summand(ei2);
                    self.complete.pop();
                } else {
                    let node = self.forest.sum(lhs_sym, ei.origin);
                    if ei.origin == 0 && lhs_sym == self.tables.start_symbol {
                        self.finished_node = Some(node);
                    }
                    self.complete(ei.origin, lhs_sym, node);
                    break;
                }
            }
        }
    }

    /// Sorts medial items with deduplication.
    fn sort_medial_items(&mut self) {
        let tables = &self.tables;
        // Build index by postdot
        // These medial positions themselves are sorted by postdot symbol.
        let current_earley_set = self.earley_chart.last_mut().unwrap();
        current_earley_set.medial.sort_unstable_by(|a, b| {
            (tables.get_rhs1_cmp(a.dot), a.dot, a.origin).cmp(&(
                tables.get_rhs1_cmp(b.dot),
                b.dot,
                b.origin,
            ))
        });
    }

    fn prediction_pass(&mut self) {
        let last = self.earley_chart.len() - 1;
        let (before, after) = self.earley_chart.split_at_mut(last);
        // Iterate through medial items in the current set.
        let iter = before.last().unwrap().medial.iter();
        // For each medial item in the current set, predict its postdot symbol.
        let destination = &mut after.last_mut().unwrap().predicted;
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
                self.earley_chart.last_mut().unwrap().medial.push(Item {
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

    fn finished_node(&self) -> Option<NodeHandle> {
        self.finished_node
    }

    fn log_last_earley_set(&self) {
        let mut dots: BTreeMap<usize, BTreeMap<usize, BTreeSet<usize>>> = BTreeMap::new();
        let es = self.earley_chart.last().unwrap();
        for (i, rule) in self.tables.rules.iter().enumerate() {
            if es.predicted[rule.lhs.usize()] {
                dots.entry(i).or_insert(BTreeMap::new()).entry(0).or_insert(BTreeSet::new()).insert(self.earleme());
            }
        }
        for item in &es.medial {
            dots.entry(item.dot).or_insert(BTreeMap::new()).entry(1).or_insert(BTreeSet::new()).insert(item.origin);
        }
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

const NULL_ACTION: u32 = !0;

impl Forest {
    fn new(grammar: &BinarizedGrammar) -> Self {
        Self {
            graph: vec![],
            summands: vec![],
            eval: grammar.rules.iter().map(|rule| rule.source).collect(),
        }
    }

    fn evaluate<T, F, G>(&mut self, finished_node: NodeHandle, eval_product: F, eval_leaf: G) -> T
        where F: Fn(u32, &[T]) -> T + Copy,
              G: Fn(Symbol, u32) -> T + Copy,
              T: Copy
    {
        self.evaluate_rec(finished_node, eval_product, eval_leaf)[0]
    }

    fn evaluate_rec<T, F, G>(&mut self, handle: NodeHandle, eval_product: F, eval_leaf: G) -> Vec<T>
        where F: Fn(u32, &[T]) -> T + Copy,
              G: Fn(Symbol, u32) -> T + Copy,
              T: Copy
    {
        match &self.graph[handle.0] {
            &Node::Sum { ref summands, .. } => {
                assert_eq!(summands.len(), 1);
                let product = summands[0];
                let mut result = self.evaluate_rec(product.left_factor, eval_product, eval_leaf);
                if let Some(factor) = product.right_factor {
                    result.extend(self.evaluate_rec(factor, eval_product, eval_leaf));
                }
                if let Some(rule_id) = self.eval[product.action as usize] {
                    vec![eval_product(rule_id as u32, &result[..])]
                } else {
                    result
                }
            }
            &Node::Leaf { terminal, values } => {
                vec![eval_leaf(terminal, values)]
            }
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

    fn sum(&mut self, lhs_sym: Symbol, _origin: usize) -> NodeHandle {
        let handle = NodeHandle(self.graph.len());
        self.graph.push(Node::Sum {
            nonterminal: lhs_sym,
            summands: mem::replace(&mut self.summands, vec![]),
        });
        handle
    }

    fn get_eval(&self, dot: usize) -> Option<u32> {
        self.eval[dot].map(|id| id as u32)
    }
}

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
    grammar.rule(sum).rhs([sum, op_sum, factor]);
    grammar.rule(sum).rhs([factor]);
    grammar.rule(factor).rhs([factor, op_factor, expr_sym]);
    grammar.rule(factor).rhs([expr_sym]);
    grammar.rule(expr_sym).rhs([lparen, sum, rparen]);
    grammar.rule(expr_sym).rhs([op_minus, sum]);
    grammar.rule(expr_sym).rhs([number]);
    grammar.rule(number).rhs([whole]);
    grammar.rule(number).rhs([whole, dot, whole]);
    grammar.rule(whole).rhs([whole, digit]);
    grammar.rule(whole).rhs([digit]);
    grammar.rule(op_sum).rhs([op_minus]);
    grammar.rule(op_sum).rhs([op_plus]);
    grammar.start_symbol(sum);
    let binarized_grammar = grammar.binarize();
    let mut recognizer = Recognizer::new(binarized_grammar);
    for (i, ch) in expr.chars().enumerate() {
        let terminal = match ch {
            '-' => op_minus,
            '.' => dot,
            '0' ..= '9' => digit,
            '(' => lparen,
            ')' => rparen,
            '*' | '/' => op_factor,
            '+' => op_plus,
            ' ' => continue,
            other => panic!("invalid character {}", other)
        };
        println!("begin {} earleme", i);
        recognizer.log_last_earley_set();
        recognizer.begin_earleme();
        println!("scan '{}'", ch);
        recognizer.scan(terminal, ch as u32);
        println!("end {} earleme", i);
        recognizer.log_last_earley_set();
        assert!(recognizer.end_earleme(), "parse failed at character {}", i);
    }
    let finished_node = recognizer.finished_node().expect("parse failed");
    recognizer.forest.evaluate(
        finished_node,
        |rule_id, args| {
            match rule_id {
                0 => {
                    let (left, right) = (args[0], args[2]);
                    left + right
                }
                1 => {
                    args[0]
                }
                other => panic!("unknown rule id {}", other)
            }
        },
        |terminal, values| {
            if terminal == digit {
                (values - ('0' as u32)) as f64
            } else {
                0f64
            }
        }
    )
}

fn main() {
    let expr = "((2.33 / (2.9+3.5)*4) - -6)";
    let expected = 7.45625;
    let result = calc(expr);
    assert_eq!(result, expected);
}
