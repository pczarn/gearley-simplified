extern crate gearley_simplified;

use std::collections::BTreeMap;
use std::ops::Range;

use gearley_simplified::{Grammar, BinarizedGrammar, Evaluator, Forest, Recognizer};

<%= custom_decls %>
% auto_decls.each do |auto_decl|
    #[derive(Clone)]
    pub struct RsTy<%= auto_decl.lhs_name %> {
        % auto_decl.bound_syms.each do |sym|
            <%= sym.bind %>: RsTy<%= sym.name.camel_case %>,
        % end
    }
% end
<%= type_decls %>

% grammars.each do |grammar|
    fn make_<%= grammar.name %>_grammar() -> BinarizedGrammar {
        let mut grammar = Grammar::new();
        % sym_names_dedup(grammar).each do |name|
            let sym_<%= name %> = grammar.make_symbol("<%= name %>");
        % end
        % grammar.rules.each_with_index do |rule, i|
            grammar.rule(sym_<%= rule.lhs.name %>).rhs(
                [
                    % rule.rhs.each do |sym|
                        sym_<%= sym.name %>,
                    % end
                ]
            ).id(<%= i %>).build();
        % end
        grammar.start_symbol(sym_<%= start_sym_name(grammar) %>);
        grammar.binarize()
    }
% end

#[derive(Clone)]
#[allow(non_camel_case_types)]
enum Value {
    None,
    % rules.map(&:lhs).uniq.each do |lhs|
        <%= lhs.name %>(RsTy<%= lhs.name.camel_case %>),
    % end
    <%= terminal_value_variants %>
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
    % terminal_sym_names.each do |name|
        "<%= name %>",
    % end
];

impl Lexer {
    fn new() -> Self {
        let grammar = make_lexer_grammar();
        let recognizer = Recognizer::new(&grammar);
        Lexer { grammar, recognizer }
    }

    fn lex(&mut self, expr: &str, parser: &mut Parser) -> ParseInfo {
        let mut parse_info = ParseInfo::new();
        let mut input = expr.chars().peekable();
        while input.peek().is_some() {
            if let Some(lex_result) = self.lex_one(&mut input, parser, &mut parse_info) {
                parser.recognizer.begin_earleme();
                for &info in &parse_info.earlemes[lex_result.terminals] {
                    parser.recognizer.scan(parser.grammar.sym(TERMINAL_TO_SYM_NAME[info.terminal]), info.terminal as u32);
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

    fn lex_one(&mut self, input: &mut (impl Iterator<Item=char> + Clone), parser: &Parser, parse_info: &mut ParseInfo) -> Option<LexResult> {
        let mut cur = input.clone().enumerate();
        let mut lexer_finished_node = None;
        let mut num_chars_consumed = 0;
        let mut earleme = EarlemeInfo::new();
        self.recognizer.reset();
        self.recognizer.begin_earleme();
        for expected in parser.recognizer.predicted() {
            let sym_name = format!("_guard_{}", parser.grammar.sym_name(expected).unwrap());
            self.recognizer.scan(self.grammar.sym(&sym_name[..]).unwrap(), 0);
        }
        self.recognizer.end_earleme();
        while let Some((i, input_char)) = cur.next() {
            earleme.expr.push(input_char);
            self.recognizer.begin_earleme();
            let scan = |sym_name| {
                self.recognizer.scan(self.grammar.sym(sym_name).unwrap(), 0);
            };
            % lexer_input_cases.each do |matcher|
                % if matcher.is_a? AnyInputMatcher
                    scan("sym_<%= matcher.sym_name %>");
                % else
                    match input_char {
                        <%= matcher.ranges.join(" | ") %> => {
                            % if !matcher.negate
                                scan("sym_<%= matcher.sym_name %>");
                            % end
                        }
                        _ => {
                            % if matcher.negate
                                scan("sym_<%= matcher.sym_name %>");
                            % end
                        }
                    }
                % end
            % end
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
            for _ in 0 .. num_chars_consumed {
                parse_info.input.push(input.next().unwrap());
            }
            let input_range_end = parse_info.input.len();
            earleme.input_range = input_range_start .. input_range_end;
            let rule_eval = |rule_id, args: &[&Option<usize>]| {
                match rule_id {
                    % lexer_rule_actions.each do |action|
                        <%= action.index %> => {
                            % if action.code
                                Some(<%= action.code %>)
                            % else
                                None
                            % end
                        }
                    % end
                    other => panic!("unknown rule id {}", other)
                }
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
                earleme.terminal = terminal.expect("incorrect evaluation: non-toplevel rule in result");
                parse_info.earlemes.push(earleme);
            }
            let terminals_end = parse_info.earlemes.len();
            Some(LexResult {
                input_range: earleme.input_range.clone(),
                terminals: terminals_start .. terminals_end,
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
        Parser { grammar, recognizer }
    }
}

#[allow(unused_braces)]
pub fn parse(expr: &str) -> <%= result_type %> {
    let mut parser = Parser::new();
    let mut lexer = Lexer::new();
    let earlemes = lexer.lex(expr, &mut parser);
    let finished_node = parser.recognizer.finished_node().expect("parse failed");
    let rule_eval = |rule_id, args: &[&Value]| {
        match rule_id {
            % rule_actions.each do |action|
                <%= action.index %> => {
                    % action.args.each do |arg|
                        let mut <%= arg.bind %> = match args[<%= arg.index %>].clone() { Value::<%= arg.variant %>(val) => val, _ => panic!("wrong sym") };
                    % end
                    Value::<%= action.result_variant %>({ <%= action.code %> })
                }
            % end
            other => panic!("unknown rule id {}", other)
        }
    };
    let terminal_eval = |terminal, values| {
        // let info = &earlemes[values as usize];
        // let slice = &info.expr[..];
        % terminal_actions.each do |action|
            if values == <%= action.index %> {
                Value::<%= action.variant %>({ <%= action.code %> })
            } else
        % end
        {
            Value::None
        }
    };
    let mut evaluator = Evaluator::new(rule_eval, terminal_eval);
    let result = evaluator.evaluate(parser.recognizer.forest_mut(), finished_node).into_iter().next().expect("evaluation failed");
    match result {
        Value::<%= result_variant %>(val) => val,
        _ => panic!("incorrect result of eval")
    }
}

% test_fns.each do |fn|
    #[test]
    fn <%= "test_example_" + fn.name >() {
        parse(r###"<%= fn.string >"###);
    }
%
