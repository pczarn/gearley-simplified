extern crate gearley_simplified;
extern crate logos;

use gearley_simplified::{Evaluator, Forest, Grammar, Recognizer};

use logos::Logos;

#[derive(Clone)]
pub struct RsTySum {}
#[derive(Clone)]
pub struct RsTyFactor {}
#[derive(Clone)]
pub struct RsTyExpr {}
#[derive(Clone)]
pub struct RsTyNumber {}
#[derive(Clone)]
pub struct RsTyWhole {}
type RsTyLparen = ();
type RsTyRparen = ();
type RsTyOpMinus = ();
type RsTyOpPlus = ();
type RsTyOpMul = ();
type RsTyOpDiv = ();
type RsTyDigit = ();
type RsTyDot = ();

#[allow(unused_braces)]
pub fn parse(expr: &str) -> RsTySum {
    let mut grammar = Grammar::new();
    let sym_digit = grammar.make_symbol("digit");
    let sym_dot = grammar.make_symbol("dot");
    let sym_expr = grammar.make_symbol("expr");
    let sym_factor = grammar.make_symbol("factor");
    let sym_lparen = grammar.make_symbol("lparen");
    let sym_number = grammar.make_symbol("number");
    let sym_op_div = grammar.make_symbol("op_div");
    let sym_op_minus = grammar.make_symbol("op_minus");
    let sym_op_mul = grammar.make_symbol("op_mul");
    let sym_op_plus = grammar.make_symbol("op_plus");
    let sym_rparen = grammar.make_symbol("rparen");
    let sym_sum = grammar.make_symbol("sum");
    let sym_whole = grammar.make_symbol("whole");
    grammar
        .rule(sym_sum)
        .rhs([sym_sum, sym_op_plus, sym_factor])
        .id(0)
        .build();
    grammar
        .rule(sym_sum)
        .rhs([sym_sum, sym_op_minus, sym_factor])
        .id(1)
        .build();
    grammar.rule(sym_sum).rhs([sym_factor]).id(2).build();
    grammar
        .rule(sym_factor)
        .rhs([sym_factor, sym_op_mul, sym_expr])
        .id(3)
        .build();
    grammar
        .rule(sym_factor)
        .rhs([sym_factor, sym_op_div, sym_expr])
        .id(4)
        .build();
    grammar.rule(sym_factor).rhs([sym_expr]).id(5).build();
    grammar
        .rule(sym_expr)
        .rhs([sym_lparen, sym_sum, sym_rparen])
        .id(6)
        .build();
    grammar
        .rule(sym_expr)
        .rhs([sym_op_minus, sym_sum])
        .id(7)
        .build();
    grammar.rule(sym_expr).rhs([sym_number]).id(8).build();
    grammar.rule(sym_number).rhs([sym_whole]).id(9).build();
    grammar
        .rule(sym_number)
        .rhs([sym_whole, sym_dot, sym_whole])
        .id(10)
        .build();
    grammar
        .rule(sym_whole)
        .rhs([sym_whole, sym_digit])
        .id(11)
        .build();
    grammar.rule(sym_whole).rhs([sym_digit]).id(12).build();
    grammar.start_symbol(sym_sum);
    let binarized_grammar = grammar.binarize();
    let mut recognizer = Recognizer::new(binarized_grammar);
    #[derive(Logos)]
    enum Token {
        #[token("(", |_| 0)]
        Lparen(usize),
        #[token(")", |_| 0)]
        Rparen(usize),
        #[token("-", |_| 0)]
        OpMinus(usize),
        #[token("+", |_| 0)]
        OpPlus(usize),
        #[token("*", |_| 0)]
        OpMul(usize),
        #[token("/", |_| 0)]
        OpDiv(usize),
        #[token("[0-9]", |_| 0)]
        Digit(usize),
        #[token(".", |_| 0)]
        Dot(usize),

        #[error]
        Error,
    }
    #[derive(Clone)]
    #[allow(non_camel_case_types)]
    enum Value {
        None,
        sum(RsTySum),
        factor(RsTyFactor),
        expr(RsTyExpr),
        number(RsTyNumber),
        whole(RsTyWhole),
        lparen(RsTyLparen),
        rparen(RsTyRparen),
        op_minus(RsTyOpMinus),
        op_plus(RsTyOpPlus),
        op_mul(RsTyOpMul),
        op_div(RsTyOpDiv),
        digit(RsTyDigit),
        dot(RsTyDot),
    }
    let mut lex = Token::lexer(expr);
    let mut spans = vec![];
    let mut ordinals = vec![];
    while let Some(token) = lex.next() {
        spans.push(lex.span());
        recognizer.begin_earleme();
        let (terminal, ordinal) = match token {
            Token::Lparen(ordinal) => (sym_lparen, ordinal),
            Token::Rparen(ordinal) => (sym_rparen, ordinal),
            Token::OpMinus(ordinal) => (sym_op_minus, ordinal),
            Token::OpPlus(ordinal) => (sym_op_plus, ordinal),
            Token::OpMul(ordinal) => (sym_op_mul, ordinal),
            Token::OpDiv(ordinal) => (sym_op_div, ordinal),
            Token::Digit(ordinal) => (sym_digit, ordinal),
            Token::Dot(ordinal) => (sym_dot, ordinal),
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
            0 => {
                let mut left = match args[0].clone() {
                    Value::sum(val) => val,
                    _ => panic!("wrong sym"),
                };
                let mut right = match args[2].clone() {
                    Value::factor(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::sum({ left + right })
            }
            1 => {
                let mut left = match args[0].clone() {
                    Value::sum(val) => val,
                    _ => panic!("wrong sym"),
                };
                let mut right = match args[2].clone() {
                    Value::factor(val) => val,
                    _ => panic!("wrong sym"),
                };
                Value::sum({ left - right })
            }
            2 => Value::sum({ RsTySum {} }),
            3 => Value::factor({ RsTyFactor {} }),
            4 => Value::factor({ RsTyFactor {} }),
            5 => Value::factor({ RsTyFactor {} }),
            6 => Value::expr({ RsTyExpr {} }),
            7 => Value::expr({ RsTyExpr {} }),
            8 => Value::expr({ RsTyExpr {} }),
            9 => Value::number({ RsTyNumber {} }),
            10 => Value::number({ RsTyNumber {} }),
            11 => Value::whole({ RsTyWhole {} }),
            12 => Value::whole({ RsTyWhole {} }),
            other => panic!("unknown rule id {}", other),
        },
        |terminal, values| {
            let span = spans[values as usize].clone();
            let ordinal = ordinals[values as usize];
            let slice = expr[span].to_string();
            if terminal == sym_lparen {
                Value::lparen(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_rparen {
                Value::rparen(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_op_minus {
                Value::op_minus(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_op_plus {
                Value::op_plus(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_op_mul {
                Value::op_mul(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_op_div {
                Value::op_div(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_digit {
                Value::digit(if ordinal == 0 {
                } else {
                    unreachable!()
                })
            } else if terminal == sym_dot {
                Value::dot(if ordinal == 0 {
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
        Value::sum(val) => val,
        _ => panic!("incorrect result of eval"),
    }
}
