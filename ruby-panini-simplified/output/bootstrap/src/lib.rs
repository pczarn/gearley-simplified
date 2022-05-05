extern crate gearley_simplified;

use std::collections::BTreeMap;
use std::ops::Range;

use gearley_simplified::{BinarizedGrammar, Evaluator, Forest, Grammar, Recognizer};

#[derive(Clone)]
enum Decl {
    LiteralTokenDecl { string: String, ident: String },
    RegexpTokenDecl { regexp: String, ident: String },
}
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
type RsTyGrammar = ();
type RsTyRule = ();
type RsTyNonterminalRule = ();
type RsTyTerminalRule = ();
type RsTyLiteralTokenRule = Decl;
type RsTyRegexpTokenRule = Decl;
type RsTyString = String;
type RsTyIdent = String;
type RsTyAlnumStr = String;
fn make_parser_grammar() -> BinarizedGrammar {
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
    grammar.binarize()
}
fn make_lexer_grammar() -> BinarizedGrammar {
    let mut grammar = Grammar::new();
    let sym__contentalnum = grammar.make_symbol("_contentalnum");
    let sym__contentalpha = grammar.make_symbol("_contentalpha");
    let sym__contentany = grammar.make_symbol("_contentany");
    let sym__contentarrow = grammar.make_symbol("_contentarrow");
    let sym__contentdoublequote = grammar.make_symbol("_contentdoublequote");
    let sym__contentlparen = grammar.make_symbol("_contentlparen");
    let sym__contentop_bnf = grammar.make_symbol("_contentop_bnf");
    let sym__contentop_colon = grammar.make_symbol("_contentop_colon");
    let sym__contentop_plus = grammar.make_symbol("_contentop_plus");
    let sym__contentop_star = grammar.make_symbol("_contentop_star");
    let sym__contentquote = grammar.make_symbol("_contentquote");
    let sym__contentrparen = grammar.make_symbol("_contentrparen");
    let sym__contentslash = grammar.make_symbol("_contentslash");
    let sym__guard_alnum = grammar.make_symbol("_guard_alnum");
    let sym__guard_alpha = grammar.make_symbol("_guard_alpha");
    let sym__guard_any = grammar.make_symbol("_guard_any");
    let sym__guard_arrow = grammar.make_symbol("_guard_arrow");
    let sym__guard_doublequote = grammar.make_symbol("_guard_doublequote");
    let sym__guard_lparen = grammar.make_symbol("_guard_lparen");
    let sym__guard_op_bnf = grammar.make_symbol("_guard_op_bnf");
    let sym__guard_op_colon = grammar.make_symbol("_guard_op_colon");
    let sym__guard_op_plus = grammar.make_symbol("_guard_op_plus");
    let sym__guard_op_star = grammar.make_symbol("_guard_op_star");
    let sym__guard_quote = grammar.make_symbol("_guard_quote");
    let sym__guard_rparen = grammar.make_symbol("_guard_rparen");
    let sym__guard_slash = grammar.make_symbol("_guard_slash");
    let sym__lexer_start = grammar.make_symbol("_lexer_start");
    let sym_match_class0 = grammar.make_symbol("match_class0");
    let sym_match_class1 = grammar.make_symbol("match_class1");
    let sym_match_class10 = grammar.make_symbol("match_class10");
    let sym_match_class11 = grammar.make_symbol("match_class11");
    let sym_match_class12 = grammar.make_symbol("match_class12");
    let sym_match_class13 = grammar.make_symbol("match_class13");
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
        .rhs([sym__guard_op_bnf, sym__contentop_bnf])
        .id(0)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_op_colon, sym__contentop_colon])
        .id(1)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_lparen, sym__contentlparen])
        .id(2)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_rparen, sym__contentrparen])
        .id(3)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_op_plus, sym__contentop_plus])
        .id(4)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_op_star, sym__contentop_star])
        .id(5)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_quote, sym__contentquote])
        .id(6)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_doublequote, sym__contentdoublequote])
        .id(7)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_slash, sym__contentslash])
        .id(8)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_arrow, sym__contentarrow])
        .id(9)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_alpha, sym__contentalpha])
        .id(10)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_alnum, sym__contentalnum])
        .id(11)
        .build();
    grammar
        .rule(sym__lexer_start)
        .rhs([sym__guard_any, sym__contentany])
        .id(12)
        .build();
    grammar
        .rule(sym__contentop_bnf)
        .rhs([sym_match_class0, sym_match_class0, sym_match_class1])
        .id(13)
        .build();
    grammar
        .rule(sym__contentop_colon)
        .rhs([sym_match_class0])
        .id(14)
        .build();
    grammar
        .rule(sym__contentlparen)
        .rhs([sym_match_class2])
        .id(15)
        .build();
    grammar
        .rule(sym__contentrparen)
        .rhs([sym_match_class3])
        .id(16)
        .build();
    grammar
        .rule(sym__contentop_plus)
        .rhs([sym_match_class4])
        .id(17)
        .build();
    grammar
        .rule(sym__contentop_star)
        .rhs([sym_match_class5])
        .id(18)
        .build();
    grammar
        .rule(sym__contentquote)
        .rhs([sym_match_class6])
        .id(19)
        .build();
    grammar
        .rule(sym__contentdoublequote)
        .rhs([sym_match_class7])
        .id(20)
        .build();
    grammar
        .rule(sym__contentslash)
        .rhs([sym_match_class8])
        .id(21)
        .build();
    grammar
        .rule(sym__contentarrow)
        .rhs([sym_match_class9, sym_match_class10])
        .id(22)
        .build();
    grammar
        .rule(sym__contentalpha)
        .rhs([sym_match_class11])
        .id(23)
        .build();
    grammar
        .rule(sym__contentalnum)
        .rhs([sym_match_class12])
        .id(24)
        .build();
    grammar
        .rule(sym__contentany)
        .rhs([sym_match_class13])
        .id(25)
        .build();
    grammar.start_symbol(sym__lexer_start);
    grammar.binarize()
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
    "op_bnf",
    "op_colon",
    "lparen",
    "rparen",
    "op_plus",
    "op_star",
    "quote",
    "doublequote",
    "slash",
    "arrow",
    "alpha",
    "alnum",
    "any",
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
                ':' => {
                    scan("sym_match_class0");
                }
                _ => {}
            }
            match input_char {
                '=' => {
                    scan("sym_match_class1");
                }
                _ => {}
            }
            match input_char {
                '(' => {
                    scan("sym_match_class2");
                }
                _ => {}
            }
            match input_char {
                ')' => {
                    scan("sym_match_class3");
                }
                _ => {}
            }
            match input_char {
                '+' => {
                    scan("sym_match_class4");
                }
                _ => {}
            }
            match input_char {
                '*' => {
                    scan("sym_match_class5");
                }
                _ => {}
            }
            match input_char {
                '\'' => {
                    scan("sym_match_class6");
                }
                _ => {}
            }
            match input_char {
                '\"' => {
                    scan("sym_match_class7");
                }
                _ => {}
            }
            match input_char {
                '/' => {
                    scan("sym_match_class8");
                }
                _ => {}
            }
            match input_char {
                '-' => {
                    scan("sym_match_class9");
                }
                _ => {}
            }
            match input_char {
                '>' => {
                    scan("sym_match_class10");
                }
                _ => {}
            }
            match input_char {
                'A'..='Z' | 'a'..='z' => {
                    scan("sym_match_class11");
                }
                _ => {}
            }
            match input_char {
                '0'..='9' | 'A'..='Z' | '_' | 'a'..='z' => {
                    scan("sym_match_class12");
                }
                _ => {}
            }
            scan("sym_match_class13");
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
                8 => Some(args[0].unwrap()),
                9 => Some(args[0].unwrap()),
                10 => Some(args[0].unwrap()),
                11 => Some(args[0].unwrap()),
                12 => Some(args[0].unwrap()),
                13 => Some(0),
                14 => Some(1),
                15 => Some(2),
                16 => Some(3),
                17 => Some(4),
                18 => Some(5),
                19 => Some(6),
                20 => Some(7),
                21 => Some(8),
                22 => Some(9),
                23 => Some(10),
                24 => Some(11),
                25 => Some(12),
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
        0 => Value::grammar({ () }),
        1 => Value::grammar({ () }),
        2 => Value::rule({ () }),
        3 => Value::rule({ () }),
        4 => Value::nonterminal_rule({ () }),
        5 => Value::terminal_rule({ () }),
        6 => Value::terminal_rule({ () }),
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
        14 => Value::grammar({ () }),
        15 => Value::grammar({ () }),
        other => panic!("unknown rule id {}", other),
    };
    let terminal_eval = |terminal, values| {
        // let info = &earlemes[values as usize];
        // let slice = &info.expr[..];
        if values == 0 {
            Value::op_bnf({})
        } else if values == 1 {
            Value::op_colon({})
        } else if values == 2 {
            Value::lparen({})
        } else if values == 3 {
            Value::rparen({})
        } else if values == 4 {
            Value::op_plus({})
        } else if values == 5 {
            Value::op_star({})
        } else if values == 6 {
            Value::quote({})
        } else if values == 7 {
            Value::doublequote({})
        } else if values == 8 {
            Value::slash({})
        } else if values == 9 {
            Value::arrow({})
        } else if values == 10 {
            Value::alpha({ slice.chars().next().unwrap() })
        } else if values == 11 {
            Value::alnum({ slice.chars().next().unwrap() })
        } else if values == 12 {
            Value::any({ slice.chars().next().unwrap() })
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
        Value::grammar(val) => val,
        _ => panic!("incorrect result of eval"),
    }
}

#[test]
fn test_example_simple() {
    parse(r###"grammar ::= grammar rule | rule"###);
}
