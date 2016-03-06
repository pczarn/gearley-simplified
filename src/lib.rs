extern crate cfg;

pub mod forest;
pub mod grammar;
pub mod recognizer;

pub use recognizer::RuleId;

use grammar::Grammar;
use recognizer::Recognizer;

use cfg::rule::container::RuleContainer;

#[test]
fn test_simple() {
    let mut cfg = Grammar::new();
    let (start, foo, bar) = cfg.sym();
    cfg.rule(start).rhs([foo, bar]);
    cfg.set_start(start);
    let grammar = cfg.into_internal_grammar();
    let mut recognizer = Recognizer::new(&grammar);
    recognizer.parse(&[foo, bar]);
    assert!(recognizer.is_finished());
}
