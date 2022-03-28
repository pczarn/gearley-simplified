extern crate gearley_simplified;
extern crate logos;

use gearley_simplified::{Evaluator, Forest, Grammar, Recognizer};

use logos::Logos;

#[derive(Clone)]
enum Decl {
    LiteralTokenDecl { string: String, ident: String },
    RegexpTokenDecl { regexp: String, ident: String },
}
#[derive(Clone)]
pub struct RsTyGrammar {}
#[derive(Clone)]
pub struct RsTyRule {}
#[derive(Clone)]
pub struct RsTyNonterminalRule {}
#[derive(Clone)]
pub struct RsTyTerminalRule {}
type RsTyOpBnf = ();
type RsTyOpColon = ();
type RsTyLparen = ();
type RsTyRparen = ();
type RsTyOpPlus = ();
type RsTyOpStar = ();
type RsTyQuote = ();
type RsTyDoublequote = ();
type RsTySlash = ();
type RsTyArrow = ();
type RsTyAlpha = char;
type RsTyAlnum = char;
type RsTyAny = char;

type RsTyLiteralTokenRule = Decl;
type RsTyRegexpTokenRule = Decl;
type RsTyString = String;
type RsTyIdent = String;
type RsTyAlnumStr = String;

#[allow(unused_braces)]
pub fn parse(expr: &str) -> RsTyGrammar {
    let mut grammar = Grammar::new();
    let sym_alnum = grammar.make_symbol("alnum");
    let sym_alnum_str = grammar.make_symbol("alnum_str");
    let sym_alpha = grammar.make_symbol("alpha");
    let sym_any = grammar.make_symbol("any");
    let sym_arrow = grammar.make_symbol("arrow");
    let sym_doublequote = grammar.make_symbol("doublequote");
    let sym_grammar = grammar.make_symbol("grammar");
    let sym_ident = grammar.make_symbol("ident");
    let sym_literal_token_rule = grammar.make_symbol("literal_token_rule");
    let sym_lparen = grammar.make_symbol("lparen");
    let sym_nonterminal_rule = grammar.make_symbol("nonterminal_rule");
    let sym_op_bnf = grammar.make_symbol("op_bnf");
    let sym_op_colon = grammar.make_symbol("op_colon");
    let sym_op_plus = grammar.make_symbol("op_plus");
    let sym_op_star = grammar.make_symbol("op_star");
    let sym_quote = grammar.make_symbol("quote");
    let sym_regexp_token_rule = grammar.make_symbol("regexp_token_rule");
    let sym_rparen = grammar.make_symbol("rparen");
    let sym_rule = grammar.make_symbol("rule");
    let sym_rule_rhs = grammar.make_symbol("rule_rhs");
    let sym_slash = grammar.make_symbol("slash");
    let sym_string = grammar.make_symbol("string");
    let sym_terminal_rule = grammar.make_symbol("terminal_rule");
    grammar
        .rule(sym_grammar)
        .rhs([sym_grammar, sym_rule])
        .id(0)
        .build();
    grammar.rule(sym_grammar).rhs([sym_rule]).id(1).build();
    grammar
        .rule(sym_rule)
        .rhs([sym_nonterminal_rule])
        .id(2)
        .build();
    grammar
        .rule(sym_rule)
        .rhs([sym_terminal_rule])
        .id(3)
        .build();
    grammar
        .rule(sym_nonterminal_rule)
        .rhs([sym_ident, sym_op_bnf, sym_rule_rhs])
        .id(4)
        .build();
    grammar
        .rule(sym_terminal_rule)
        .rhs([sym_literal_token_rule])
        .id(5)
        .build();
    grammar
        .rule(sym_terminal_rule)
        .rhs([sym_regexp_token_rule])
        .id(6)
        .build();
    grammar
        .rule(sym_literal_token_rule)
        .rhs([sym_quote, sym_string, sym_quote, sym_arrow, sym_ident])
        .id(7)
        .build();
    grammar
        .rule(sym_regexp_token_rule)
        .rhs([sym_slash, sym_string, sym_slash, sym_arrow, sym_ident])
        .id(8)
        .build();
    grammar
        .rule(sym_string)
        .rhs([sym_string, sym_any])
        .id(9)
        .build();
    grammar.rule(sym_string).rhs([sym_any]).id(10).build();
    grammar
        .rule(sym_ident)
        .rhs([sym_alpha, sym_alnum_str])
        .id(11)
        .build();
    grammar
        .rule(sym_alnum_str)
        .rhs([sym_alnum_str, sym_alnum])
        .id(12)
        .build();
    grammar.rule(sym_alnum_str).rhs([sym_alnum]).id(13).build();
    grammar
        .rule(sym_grammar)
        .rhs([sym_grammar, sym_rule])
        .id(14)
        .build();
    grammar.rule(sym_grammar).rhs([sym_rule]).id(15).build();
    grammar.start_symbol(sym_grammar);
    let binarized_grammar = grammar.binarize();
    let mut recognizer = Recognizer::new(binarized_grammar);
    #[derive(Logos)]
    enum Token {
        #[token("::=", |_| 0)]
        OpBnf(usize),
        #[token(":", |_| 0)]
        OpColon(usize),
        #[token("(", |_| 0)]
        Lparen(usize),
        #[token(")", |_| 0)]
        Rparen(usize),
        #[token("+", |_| 0)]
        OpPlus(usize),
        #[token("*", |_| 0)]
        OpStar(usize),
        #[token("\'", |_| 0)]
        Quote(usize),
        #[token("\"", |_| 0)]
        Doublequote(usize),
        #[token("/", |_| 0)]
        Slash(usize),
        #[token("->", |_| 0)]
        Arrow(usize),
        #[regex("[a-z][A-Z]", |_| 0)]
        Alpha(usize),
        #[regex("[0-9][a-z][A-Z]_", |_| 0)]
        Alnum(usize),
        #[regex(".", |_| 0)]
        Any(usize),
        #[token(" ", logos::skip)]
        #[error]
        Error,
    }
    #[derive(Clone)]
    #[allow(non_camel_case_types)]
    enum Value {
        None,
        grammar(RsTyGrammar),
        rule(RsTyRule),
        nonterminal_rule(RsTyNonterminalRule),
        terminal_rule(RsTyTerminalRule),
        literal_token_rule(RsTyLiteralTokenRule),
        regexp_token_rule(RsTyRegexpTokenRule),
        string(RsTyString),
        ident(RsTyIdent),
        alnum_str(RsTyAlnumStr),
        op_bnf(RsTyOpBnf),
        op_colon(RsTyOpColon),
        lparen(RsTyLparen),
        rparen(RsTyRparen),
        op_plus(RsTyOpPlus),
        op_star(RsTyOpStar),
        quote(RsTyQuote),
        doublequote(RsTyDoublequote),
        slash(RsTySlash),
        arrow(RsTyArrow),
        alpha(RsTyAlpha),
        alnum(RsTyAlnum),
        any(RsTyAny),
    }
    let mut lex = Token::lexer(expr);
    let mut spans = vec![];
    let mut ordinals = vec![];
    while let Some(token) = lex.next() {
        spans.push(lex.span());
        recognizer.begin_earleme();
        let (terminal, ordinal) = match token {
            Token::OpBnf(ordinal) => (sym_op_bnf, ordinal),
            Token::OpColon(ordinal) => (sym_op_colon, ordinal),
            Token::Lparen(ordinal) => (sym_lparen, ordinal),
            Token::Rparen(ordinal) => (sym_rparen, ordinal),
            Token::OpPlus(ordinal) => (sym_op_plus, ordinal),
            Token::OpStar(ordinal) => (sym_op_star, ordinal),
            Token::Quote(ordinal) => (sym_quote, ordinal),
            Token::Doublequote(ordinal) => (sym_doublequote, ordinal),
            Token::Slash(ordinal) => (sym_slash, ordinal),
            Token::Arrow(ordinal) => (sym_arrow, ordinal),
            Token::Alpha(ordinal) => (sym_alpha, ordinal),
            Token::Alnum(ordinal) => (sym_alnum, ordinal),
            Token::Any(ordinal) => (sym_any, ordinal),
            Token::Error => {
                let span = lex.span();
                panic!("lexing error at {:?}", span);
            }
        };
        ordinals.push(ordinal);
        recognizer.scan(terminal, spans.len() as u32 - 1);
        if !recognizer.end_earleme() {
            eprintln!("actual: {}", recognizer.terminal_name(terminal));
            recognizer.log_last_earley_set();
            panic!(
                "parse failed at string {} at input range {:?}",
                lex.slice(),
                lex.span()
            );
        }
    }
    let finished_node = recognizer.finished_node().expect("parse failed");
    let mut evaluator = Evaluator::new(
        |rule_id, args: &[Value]| match rule_id {
            0 => Value::grammar({ RsTyGrammar {} }),
            1 => Value::grammar({ RsTyGrammar {} }),
            2 => Value::rule({ RsTyRule {} }),
            3 => Value::rule({ RsTyRule {} }),
            4 => Value::nonterminal_rule({ RsTyNonterminalRule {} }),
            5 => Value::terminal_rule({ RsTyTerminalRule {} }),
            6 => Value::terminal_rule({ RsTyTerminalRule {} }),
            7 => {
                let mut string = match args[1].clone() {
                    Value::string(val) => val,
                    _ => panic!("wrong sym"),
                };
                let mut ident = match args[4].clone() {
                    Value::ident(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::literal_token_rule({ Decl::LiteralTokenDecl { string, ident } })
            }
            8 => {
                let mut string = match args[1].clone() {
                    Value::string(val) => val,
                    _ => panic!("wrong sym"),
                };
                let mut ident = match args[4].clone() {
                    Value::ident(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::regexp_token_rule({
                    Decl::RegexpTokenDecl {
                        regexp: string,
                        ident,
                    }
                })
            }
            9 => {
                let mut s = match args[0].clone() {
                    Value::string(val) => val,
                    _ => panic!("wrong sym"),
                };
                let mut ch = match args[1].clone() {
                    Value::any(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::string({
                    s.push(ch);
                    s
                })
            }
            10 => {
                let mut ch = match args[0].clone() {
                    Value::any(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::string({ ch.to_string() })
            }
            11 => {
                let mut ch = match args[0].clone() {
                    Value::alpha(val) => val,
                    _ => panic!("wrong sym"),
                };
                let mut s = match args[1].clone() {
                    Value::alnum_str(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::ident({
                    s.push(ch);
                    s
                })
            }
            12 => {
                let mut s = match args[0].clone() {
                    Value::alnum_str(val) => val,
                    _ => panic!("wrong sym"),
                };
                let mut ch = match args[1].clone() {
                    Value::alnum(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::alnum_str({
                    s.push(ch);
                    s
                })
            }
            13 => {
                let mut ch = match args[0].clone() {
                    Value::alnum(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::alnum_str({ ch.to_string() })
            }
            14 => Value::grammar({ RsTyGrammar {} }),
            15 => Value::grammar({ RsTyGrammar {} }),
            other => panic!("unknown rule id {}", other),
        },
        |terminal, values| {
            let span = spans[values as usize].clone();
            let ordinal = ordinals[values as usize];
            let slice = expr[span].to_string();
            if terminal == sym_op_bnf {
                Value::op_bnf(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_op_colon {
                Value::op_colon(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_lparen {
                Value::lparen(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_rparen {
                Value::rparen(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_op_plus {
                Value::op_plus(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_op_star {
                Value::op_star(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_quote {
                Value::quote(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_doublequote {
                Value::doublequote(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_slash {
                Value::slash(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_arrow {
                Value::arrow(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_alpha {
                Value::alpha(if ordinal == 0 {
                    slice.chars().next().unwrap()
                } else {
                    unreachable!()
                })
            } else if terminal == sym_alnum {
                Value::alnum(if ordinal == 0 {
                    slice.chars().next().unwrap()
                } else {
                    unreachable!()
                })
            } else if terminal == sym_any {
                Value::any(if ordinal == 0 {
                    slice.chars().next().unwrap()
                } else {
                    unreachable!()
                })
            } else {
                Value::None
            }
        },
    );
    let result = evaluator.evaluate(recognizer.forest_mut(), finished_node);
    match result {
        Value::grammar(val) => val,
        _ => panic!("incorrect result of eval"),
    }
}

#[test]
fn test_example_simple() {
    parse(r###"grammar ::= grammar rule | rule"###);
}
