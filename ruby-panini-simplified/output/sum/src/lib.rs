extern crate gearley_simplified;

use std::collections::BTreeMap;
use std::ops::Range;

use gearley_simplified::{BinarizedGrammar, Evaluator, Forest, Grammar, Recognizer};

type RsTyLparen = ();
type RsTyRparen = ();
type RsTyOpMinus = ();
type RsTyOpPlus = ();
type RsTyOpMul = ();
type RsTyOpDiv = ();
type RsTyDigit = ();
type RsTyDot = ();
type RsTySum = ();
type RsTyFactor = ();
type RsTyExpr = ();
type RsTyNumber = ();
type RsTyWhole = ();
fn make_parser_grammar() -> BinarizedGrammar {
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
    grammar.binarize()
}
fn make_lexer_grammar() -> BinarizedGrammar {
    let mut grammar = Grammar::new();
    let sym__contentdigit = grammar.make_symbol("_contentdigit");
    let sym__contentdot = grammar.make_symbol("_contentdot");
    let sym__contentlparen = grammar.make_symbol("_contentlparen");
    let sym__contentop_div = grammar.make_symbol("_contentop_div");
    let sym__contentop_minus = grammar.make_symbol("_contentop_minus");
    let sym__contentop_mul = grammar.make_symbol("_contentop_mul");
    let sym__contentop_plus = grammar.make_symbol("_contentop_plus");
    let sym__contentrparen = grammar.make_symbol("_contentrparen");
    let sym__guard_digit = grammar.make_symbol("_guard_digit");
    let sym__guard_dot = grammar.make_symbol("_guard_dot");
    let sym__guard_lparen = grammar.make_symbol("_guard_lparen");
    let sym__guard_op_div = grammar.make_symbol("_guard_op_div");
    let sym__guard_op_minus = grammar.make_symbol("_guard_op_minus");
    let sym__guard_op_mul = grammar.make_symbol("_guard_op_mul");
    let sym__guard_op_plus = grammar.make_symbol("_guard_op_plus");
    let sym__guard_rparen = grammar.make_symbol("_guard_rparen");
    let sym__lexer_start = grammar.make_symbol("_lexer_start");
    let sym_match_class0 = grammar.make_symbol("match_class0");
    let sym_match_class1 = grammar.make_symbol("match_class1");
    let sym_match_class10 = grammar.make_symbol("match_class10");
    let sym_match_class2 = grammar.make_symbol("match_class2");
    let sym_match_class3 = grammar.make_symbol("match_class3");
    let sym_match_class4 = grammar.make_symbol("match_class4");
    let sym_match_class5 = grammar.make_symbol("match_class5");
    let sym_match_class6 = grammar.make_symbol("match_class6");
    let sym_match_class7 = grammar.make_symbol("match_class7");
    let sym_match_class8 = grammar.make_symbol("match_class8");
    let sym_match_class9 = grammar.make_symbol("match_class9");
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_lparen, sym__contentlparen])
        .id(0)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_rparen, sym__contentrparen])
        .id(1)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_op_minus, sym__contentop_minus])
        .id(2)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_op_plus, sym__contentop_plus])
        .id(3)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_op_mul, sym__contentop_mul])
        .id(4)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_op_div, sym__contentop_div])
        .id(5)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_digit, sym__contentdigit])
        .id(6)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_dot, sym__contentdot])
        .id(7)
        .build();
    grammar
        .rule(sym__contentlparen)
        .rhs([sym_match_class0])
        .id(8)
        .build();
    grammar
        .rule(sym__contentrparen)
        .rhs([sym_match_class1])
        .id(9)
        .build();
    grammar
        .rule(sym__contentop_minus)
        .rhs([sym_match_class2])
        .id(10)
        .build();
    grammar
        .rule(sym__contentop_plus)
        .rhs([sym_match_class3])
        .id(11)
        .build();
    grammar
        .rule(sym__contentop_mul)
        .rhs([sym_match_class4])
        .id(12)
        .build();
    grammar
        .rule(sym__contentop_div)
        .rhs([sym_match_class5])
        .id(13)
        .build();
    grammar
        .rule(sym__contentdigit)
        .rhs([
            sym_match_class6,
            sym_match_class7,
            sym_match_class2,
            sym_match_class8,
            sym_match_class9,
        ])
        .id(14)
        .build();
    grammar
        .rule(sym__contentdot)
        .rhs([sym_match_class10])
        .id(15)
        .build();
    grammar.start_symbol(sym__lexer_start);
    grammar.binarize()
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

type Span = (usize, usize);

// struct ParseInfo {
//     next_key: usize,
//     earlemes: BTreeMap<usize, EarlemeInfo>,
// }

// struct EarlemeInfo {
//     expr: String,
//     scan: Vec<usize>,
// }

// impl EarlemeInfo {
//     fn new() -> Self {
//         EarlemeInfo {
//             expr: String::new(),
//             scan: vec![],
//         }
//     }
// }

// impl ParseInfo {
//     fn new() -> Self {
//         ParseInfo {
//             next_key: 0,
//             earlemes: BTreeMap::new(),
//         }
//     }

//     fn push(&mut self, earleme_info: EarlemeInfo) {
//         let scan_len = earleme_info.scan.len();
//         self.earlemes.insert(self.next_key, earleme_info);
//         self.next_key += scan_len;
//     }

//     fn get(&self, index: usize) -> Option<usize> {
//         use std::ops::Bound::Included;
//         self.earlemes.range(Included(0), Included(index)).next_back().map(|(&key, value)| value.scan[index - key])
//     }
// }

struct ParseInfo {
    input: String,
    earlemes: Vec<EarlemeInfo>,
}

#[derive(Clone)]
struct EarlemeInfo {
    input_range: Range<usize>,
    terminal: usize,
}

struct LexResult {
    input_range: Range<usize>,
    terminals: Range<usize>,
}

impl EarlemeInfo {
    fn new() -> Self {
        EarlemeInfo {
            input_range: String::new(),
            scan: vec![],
        }
    }
}

impl ParseInfo {
    fn new() -> Self {
        ParseInfo {
            input: String::new(),
            earlemes: vec![],
        }
    }

    fn push(&mut self, terminal: usize) {
        let scan_len = earleme_info.scan.len();
        self.earlemes.insert(self.next_key, earleme_info);
        self.next_key += scan_len;
    }
}

struct Lexer {
    grammar: BinarizedGrammar,
    recognizer: Recognizer,
}

const TERMINAL_TO_SYM_NAME: &'static [&'static str] = [
    "lparen", "rparen", "op_minus", "op_plus", "op_mul", "op_div", "digit", "dot",
];

impl Lexer {
    fn new() -> Self {
        let grammar = make_lexer_grammar();
        let recognizer = Recognizer::new(&grammar);
        Lexer {
            grammar,
            recognizer,
        }
    }

    fn lex(&mut self, expr: &str, parser: &mut Parser) -> ParseInfo {
        let mut parse_info = ParseInfo::new();
        let mut input = expr.chars().peekable();
        while input.peek().is_some() {
            if let Some(lex_result) = self.lex_one(&mut input, parser, &mut parse_info) {
                parser.recognizer.begin_earleme();
                for &info in &parse_info.earlemes[lex_result.terminals] {
                    parser.recognizer.scan(
                        parser.grammar.sym(TERMINAL_TO_SYM_NAME[info.terminal]),
                        info.terminal as u32,
                    );
                }
                if !parser.recognizer.end_earleme() {
                    eprintln!("actual: {}", parser.recognizer.terminal_name(terminal));
                    parser.recognizer.log_last_earley_set();
                    panic!(
                        "lexing failed at string {:?} at input range {:?}",
                        &expr[lex_result.input_range],
                        (lex_result.input_range.start, lex_result.input_range.end),
                    );
                }
                parse_info.push(earleme_info);
            } else {
                let pos = parse_info.input.len();
                panic!("lexing error at position {:?}", pos);
            }
        }
        parse_info
    }

    fn lex_one(
        &mut self,
        input: &mut (impl Iterator<Item = char> + Clone),
        parser: &Parser,
        parse_info: &mut ParseInfo,
    ) -> Option<LexResult> {
        let mut cur = input.clone().enumerate();
        let mut lexer_finished_node = None;
        let mut num_chars_consumed = 0;
        let mut earleme = EarlemeInfo::new();
        self.recognizer.reset();
        self.recognizer.begin_earleme();
        for expected in parser.recognizer.predicted() {
            let sym_name = format!("_guard_{}", parser.grammar.sym_name(expected).unwrap());
            self.recognizer
                .scan(self.grammar.sym(&sym_name[..]).unwrap(), 0);
        }
        self.recognizer.end_earleme();
        while let Some((i, input_char)) = cur.next() {
            earleme.expr.push(input_char);
            self.recognizer.begin_earleme();
            let scan = |sym_name| {
                self.recognizer.scan(self.grammar.sym(sym_name).unwrap(), 0);
            };
            match input_char {
                '(' => {
                    scan("sym_match_class0");
                }
                _ => {}
            }
            match input_char {
                ')' => {
                    scan("sym_match_class1");
                }
                _ => {}
            }
            match input_char {
                '-' => {
                    scan("sym_match_class2");
                }
                _ => {}
            }
            match input_char {
                '+' => {
                    scan("sym_match_class3");
                }
                _ => {}
            }
            match input_char {
                '*' => {
                    scan("sym_match_class4");
                }
                _ => {}
            }
            match input_char {
                '/' => {
                    scan("sym_match_class5");
                }
                _ => {}
            }
            match input_char {
                '[' => {
                    scan("sym_match_class6");
                }
                _ => {}
            }
            match input_char {
                '0' => {
                    scan("sym_match_class7");
                }
                _ => {}
            }
            match input_char {
                '9' => {
                    scan("sym_match_class8");
                }
                _ => {}
            }
            match input_char {
                ']' => {
                    scan("sym_match_class9");
                }
                _ => {}
            }
            match input_char {
                '.' => {
                    scan("sym_match_class10");
                }
                _ => {}
            }
            // earleme.ordinals.push(ordinal);
            if self.recognizer.end_earleme() {
                let finished_node = self.recognizer.finished_node();
                if finished_node.is_some() {
                    lexer_finished_node = finished_node;
                    num_chars_consumed = i;
                }
            } else {
                break;
            }
        }
        if let Some(finished_node) = lexer_finished_node {
            let input_range_start = parse_info.input.len();
            for _ in 0..num_chars_consumed {
                parse_info.input.push(input.next().unwrap());
            }
            let input_range_end = parse_info.input.len();
            earleme.input_range = input_range_start..input_range_end;
            let rule_eval = |rule_id, args: &[&Option<usize>]| match rule_id {
                0 => Some(args[0].unwrap()),
                1 => Some(args[0].unwrap()),
                2 => Some(args[0].unwrap()),
                3 => Some(args[0].unwrap()),
                4 => Some(args[0].unwrap()),
                5 => Some(args[0].unwrap()),
                6 => Some(args[0].unwrap()),
                7 => Some(args[0].unwrap()),
                8 => Some(0),
                9 => Some(1),
                10 => Some(2),
                11 => Some(3),
                12 => Some(4),
                13 => Some(5),
                14 => Some(6),
                15 => Some(7),
                other => panic!("unknown rule id {}", other),
            };
            let terminal_eval = |terminal, values| {
                // let info = &earlemes[values as usize];
                // let ordinal = ordinals[values as usize];
                // let slice = expr[span].to_string();
                // vec![# {lexer_terminal_actions}
                // else {
                //     Value::None
                // }]
                vec![None]
            };

            let mut evaluator = Evaluator::new(rule_eval, terminal_eval);
            let eval_result = evaluator.evaluate(self.recognizer.forest_mut(), finished_node);
            let terminals_start = parse_info.earlemes.len();
            for terminal in eval_result {
                earleme.terminal =
                    terminal.expect("incorrect evaluation: non-toplevel rule in result");
                parse_info.earlemes.push(earleme);
            }
            let terminals_end = parse_info.earlemes.len();
            Some(LexResult {
                input_range: earleme.input_range.clone(),
                terminals: terminals_start..terminals_end,
            })
        } else {
            None
        }
    }
}

struct Parser {
    grammar: BinarizedGrammar,
    recognizer: Recognizer,
}

impl Parser {
    fn new() -> Self {
        let grammar = make_parser_grammar();
        let recognizer = Recognizer::new(&grammar);
        Parser {
            grammar,
            recognizer,
        }
    }
}

#[allow(unused_braces)]
pub fn parse(expr: &str) -> () {
    let mut parser = Parser::new();
    let mut lexer = Lexer::new();
    let earlemes = lexer.lex(expr, &mut parser);
    let finished_node = parser.recognizer.finished_node().expect("parse failed");
    let rule_eval = |rule_id, args: &[&Value]| match rule_id {
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
        2 => Value::sum({ () }),
        3 => Value::factor({ () }),
        4 => Value::factor({ () }),
        5 => Value::factor({ () }),
        6 => Value::expr({ () }),
        7 => Value::expr({ () }),
        8 => Value::expr({ () }),
        9 => Value::number({ () }),
        10 => Value::number({ () }),
        11 => Value::whole({ () }),
        12 => Value::whole({ () }),
        other => panic!("unknown rule id {}", other),
    };
    let terminal_eval = |terminal, values| {
        // let info = &earlemes[values as usize];
        // let slice = &info.expr[..];
        if values == 0 {
            Value::lparen({})
        } else if values == 1 {
            Value::rparen({})
        } else if values == 2 {
            Value::op_minus({})
        } else if values == 3 {
            Value::op_plus({})
        } else if values == 4 {
            Value::op_mul({})
        } else if values == 5 {
            Value::op_div({})
        } else if values == 6 {
            Value::digit({})
        } else if values == 7 {
            Value::dot({})
        } else {
            Value::None
        }
    };
    let mut evaluator = Evaluator::new(rule_eval, terminal_eval);
    let result = evaluator
        .evaluate(parser.recognizer.forest_mut(), finished_node)
        .into_iter()
        .next()
        .expect("evaluation failed");
    match result {
        Value::sum(val) => val,
        _ => panic!("incorrect result of eval"),
    }
}
