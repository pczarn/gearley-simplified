extern crate cfg;

pub mod forest;
pub mod grammar;
pub mod item;
pub mod recognizer;

use grammar::Grammar;
use recognizer::Recognizer;

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

#[test]
fn test_parenthesized() {
    let mut cfg = Grammar::new();
    let (start, expr, parenthesized, l_paren, r_paren) = cfg.sym();
    cfg.rule(start).rhs([expr])
       .rule(parenthesized).rhs([l_paren, r_paren])
                        .rhs([l_paren, expr, r_paren]);
    cfg.sequence(expr).inclusive(1, None).rhs(parenthesized);
    cfg.set_start(start);
    let grammar = cfg.into_internal_grammar();
    let mut recognizer = Recognizer::new(&grammar);
    recognizer.parse(&[l_paren, r_paren, l_paren, l_paren, r_paren, r_paren]);
    assert!(recognizer.is_finished());
}
