extern crate gearley_simplified;

use gearley_simplified::{Grammar, BinarizedGrammar, Evaluator, Forest, Recognizer};

<%= custom_decls >
% auto_decls.each do |auto_decl|
    #[derive(Clone)]
    pub struct RsTy<%= auto_decl.lhs_name > {
        % auto_decl.bound_syms.each do |sym|
            <%= sym.bind >: RsTy<%= sym.name.camel_case >,
        % end
    }
<% end >
<%= type_decls >

% grammars.each do |grammar|
    fn make_<%= grammar.name >_grammar() -> BinarizedGrammar {
        let mut grammar = Grammar::new();
        % sym_names_dedup(grammar).each do |name|
            let sym_<%= name > = grammar.make_symbol("<%= name >");
        % end
        % grammar.rules.each_with_index do |rule, i|
            grammar.rule(sym_<%= rule.lhs.name>).rhs(
                [
                    % rule.rhs.each do |sym|
                        sym_%<= sym.name >
                    % end
                ]
            ).id(%<= i >).build();
        % end
        grammar.start_symbol(sym_<%= start_sym_name(grammar) >);
        grammar.binarize()
    }
% end

#[derive(Clone)]
#[allow(non_camel_case_types)]
enum Value {
    None,
    % rules.map(&:lhs).uniq.each do |lhs|
        <%= lhs.name >(RsTy<%= lhs.name.camel_case >),
    % end
    <%= terminal_value_variants >
}

type Span = (usize, usize);

struct EarlemeInfo {
    expr: String,
    ordinals: Vec<usize>,
    scan: Vec<&'static str>,
}

impl EarlemeInfo {
    fn new() -> Self {
        EarlemeInfo {
            expr: String::new(),
            ordinals: vec![],
            scan: vec![],
        }
    }
}

struct Lexer {
    grammar: BinarizedGrammar,
    recognizer: Recognizer,
}

impl Lexer {
    fn new() -> Self {
        let grammar = make_lexer_grammar();
        let recognizer = Recognizer::new(&grammar);
        Lexer { grammar, recognizer }
    }

    fn lex(&mut self, expr: &str, parser: &mut Parser) -> Vec<EarlemeInfo> {
        let mut earlemes = vec![];
        let mut input = expr.chars().peekable();
        while input.peek().is_some() {
            if let Some(earleme_info) = self.lex_one(&mut input, parser) {
                parser.recognizer.begin_earleme();
                for &terminal in &earleme_info.scan {
                    parser.recognizer.scan(parser.grammar.sym(terminal), earlemes.len() as u32);
                }
                if !parser.recognizer.end_earleme() {
                    eprintln!("actual: {}", parser.recognizer.terminal_name(terminal));
                    parser.recognizer.log_last_earley_set();
                    let start = earlemes.map(|e| e.expr.len()).sum::<usize>();
                    let end = start + earleme_info.expr.len();
                    panic!(
                        "lexing failed at string {:?} at input range {:?}",
                        earleme_info.expr,
                        (start, end),
                    );
                }
                earlemes.push(earleme_info);
            } else {
                let pos = earlemes.map(|e| e.expr.len()).sum::<usize>();
                panic!("lexing error at {:?}", pos);
            }
        }
    }

    fn lex_one(&mut self, expr: &mut impl Iterator<Item=char>, parser: &Parser) -> EarlemeInfo {
        let mut cur = input.clone().enumerate();
        let mut lexer_finished_node = None;
        let mut num_chars_consumed = 0;
        let mut earleme = EarlemeInfo::new();
        self.recognizer.reset();
        self.recognizer.begin_earleme();
        for expected in parser.recognizer.predicted() {
            let sym_name = format!("_guard_{}", parser.grammar.sym_name(expected).unwrap());
            self.recognizer.scan(self.grammar.sym(&sym_name[..]));
        }
        self.recognizer.end_earleme();
        while let Some((i, input_char)) = cur.next() {
            earleme.expr.push(input_char);
            self.recognizer.begin_earleme();
            let terminals = match input_char {
                <%= map_input_char_to_terminals}
            };
            // earleme.ordinals.push(ordinal);
            for &sym_name in terminals {
                self.recognizer.scan(self.grammar.sym(sym_name), lex_earlemes.len() as u32 - 1);
            }
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
            for _ in 0 .. num_chars_consumed {
                earleme.expr.push(input.next().unwrap());
            }
            let rule_eval = |rule_id, args: &[&Option<&'static str>]| {
                match rule_id {
                    <%= lexer_rule_actions}
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
            let eval_result = evaluator.evaluate(recognizer.forest_mut(), finished_node);
            earleme.scan = eval_result.into_iter().map(|r| r.expect("incorrect evaluation: non-toplevel rule in result")).collect();
            Some(earleme)
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
pub fn parse(expr: &str) -> <%= result_type} {
    let mut parser = Parser::new();
    let mut lexer = Lexer::new();
    let earlemes = lexer.lex(expr, &mut parser);
    let finished_node = recognizer.finished_node().expect("parse failed");
    let rule_eval = |rule_id, args: &[&Value]| {
        match rule_id {
            <%= rule_actions}
            other => panic!("unknown rule id {}", other)
        }
    };
    let terminal_eval = |terminal, values| {
        let info = &earlemes[values as usize];

        let ordinal = ordinals[values as usize];
        let slice = expr[span].to_string();
        <%= terminal_actions}
        else {
            Value::None
        }
    };
    let mut evaluator = Evaluator::new(rule_eval, terminal_eval);
    let result = evaluator.
    (recognizer.forest_mut(), finished_node);
    match result {
        Value::<%= result_variant}(val) => val,
        _ => panic!("incorrect result of eval")
    }
}

<%= test_fns >
