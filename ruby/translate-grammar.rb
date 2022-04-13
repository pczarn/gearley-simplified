require 'pry'
# require 'memoist'

class Rule < Struct.new(:lhs, :rhs, :type, :action)
end

class Grammar < Struct.new(:rules, :terminals_by_ident)
end

class Lexer < Struct.new(:terminals, :ignore, :error)
end

class Token < Struct.new(:kind, :ident, :string, :type, :code)
end

class Sym < Struct.new(:name, :bind)
    def <=>(other)
        name <=> other.name
    end
end

# terminals is {[digit, /0-9/], []]
#ignore is [regex, str]

TOKEN_REGEX = /
    '(.+)'  \s*
    ->      \s*
    (\w+)   \s*
    (?:
        :       \s*
        (\w+)   \s*
        {(.*)}
    )?
/x
TOKEN_REGEX_REGEX = /
    \/(.+)\/    \s*
    ->          \s*
    (\w+)       \s*
    (?:
        :       \s*
        (\w+)   \s*
        {(.*)}
    )?
/x

RULE_REGEXP = /
    (?<lhs>\w+)     \s* # lhs
    (?:
        ->
        (?<type>.*)
    )?
    ::=             \s*
    (?<rhs>.+) # rhs
    \n
/x

class String
    def camel_case
        return self if self !~ /_/ && self =~ /[A-Z]+.*/
        split('_').map{|e| e.capitalize}.join
    end
end

def escape_double_quotes str
    str.gsub("\"", "\\\"")
end

class RsGenerator
    def initialize grammar_str
        @grammar_str = grammar_str
    end

    def rules

    end

    def literal_tokens_array
        @grammar_str.scan(TOKEN_REGEX).map {|s, ident, type, code| Token.new(:literal, ident, s, type, code) }
    end

    def regexp_tokens_array
        @grammar_str.scan(TOKEN_REGEX_REGEX).map {|regexp, ident, type, code| Token.new(:regexp, ident, regexp, type, code) }
    end

    def tokens_array
        literal_tokens_array + regexp_tokens_array
    end

    def terminals_by_ident
        tokens_array.group_by(&:ident)
    end

    def lexer
        ignore = terminals_by_ident["ignore"]
    
        error = terminals_by_ident["error"]
    
        terminals = terminals_by_ident.select {|key, value| key != "ignore" && key != "error" }
    
        Lexer.new(terminals, ignore, error)
    end

    def tokens_by_kind_and_string
        Hash[tokens_array.map {|t| [[t.kind, t.string], t.ident] }]
    end

    def literal_by_string shorthand
        tokens_by_kind_and_string[[:literal, shorthand]]
    end

    def regexp_by_string shorthand
        tokens_by_kind_and_string[[:regexp, shorthand]]
    end

    def rules
        @grammar_str.scan(RULE_REGEXP).flat_map do |lhs, type, rhs|
            lhs = Sym.new(lhs)
            rhs.split('|').map(&:strip).map do |alt|
                rhs_str, code = alt.split(' => ')
                rhs_syms = rhs_str.split(/\s+/)
                rhs_syms.map! do |sym|
                    bind = nil
                    if match = sym.match(/\A(?<bind>[_a-zA-Z]\w*):(?<val>.*)\Z/)
                        sym = match[:val]
                        bind = match[:bind]
                    end
                    sym_name = if shorthand = sym[/\A'(.+)'\Z/, 1]
                        raise "unknown shorthand '#{shorthand}'" if literal_by_string(shorthand).nil?
                        literal_by_string(shorthand)
                    elsif shorthand = sym[/\A\/(.+)\/\Z/, 1]
                        raise "unknown regexp shorthand '#{shorthand}'" if regexp_by_string(shorthand).nil?
                        regexp_by_string(shorthand)
                    elsif sym[/\A[_a-zA-Z]\w*\Z/]
                        sym
                    else
                        raise "wrong symbol #{sym}"
                    end
                    Sym.new(sym_name, bind)
                end
                if code
                    code = code.strip[/\A\{(.+)\}\Z/, 1]
                    raise "wrong code in #{alt}" if code.nil?
                end
                Rule.new(lhs, rhs_syms, type, code)
            end
        end
    end

    def parser_grammar
        Grammar.new(rules, lexer.terminals)
    end

    def lexer_grammar
        Grammar.new(lexer_rules, terminals)
    end

    def lexer_rule_actions
        lexer_rules.each_with_index.map do |i, rule|
            if rule.action
                "#{i} => { Some(#{rule.action}) }"
            else
                "#{i} => { None }"
            end
        end.join("\n")
    end

    def lexer_rules
        lexer_toplevel_rules + lexer_terminal_rules
    end

    def lexer_toplevel_rules
        lexer.terminals.map do |ident, tokens|
            Rule.new(lexer_start_sym, [Sym.new("_guard_" + ident, nil), Sym.new("_content" + ident)], "&'static str", "\"#{ident}\"")
        end
    end

    def lexer_terminal_rules
        lexer.terminals.flat_map do |ident, tokens|
            tokens.map do |token|
                Rule.new(Sym.new("_content" + ident), lexer_rhs(token), nil, nil)
            end
        end
    end

    def map_input_char_to_terminals
        lexer_input_cases.map do |char, terminals|
            terminals = terminals.join(", ")
            "'#{char}' => &[#{terminals}],"
        end.join("\n")
    end

    TOKEN_REGEXP_CHAR_REGEXP_KLASS = /
        (?<negative> \A ^ ) |
        (?<range> [^ \\ \. \| \[ ] ) - (?<range_end> [^ \\ \. \| \[ ] ) |
        (?<literal> [^ \\ \. \| \[ ] ) (?!-) |
        \\ (?<escaped> [ S s \. \\ ] )
    /x

    TOKEN_REGEXP_CHAR_REGEXP = /
        (?<dot> \. ) |
        \[
            (?<klass>
                ^ ?
                (?:
                    [^ \\ \. \| \[ ] - [^ \\ \. \| \[ ] |
                    [^ \\ \. \| \[ ] (?!-) |
                    \\ [ S s \. \\ ]
                )+
            )
        \] |
        (?<literal> [^ \\ \. \| \[ ] ) |
        \\ (?<escaped> [ S s \. \\ ] )
    /x

    class InputClass < Struct.new(:ranges, :negate)
    end

    def lexer_rhs token
        # TODO lexer rhs
        if token.kind == :literal
            token.string.split('').map do |char|
                char_terminal_sym char
            end
        elsif token.kind == :regexp
            klasses = token.string.scan(TOKEN_REGEXP_CHAR_REGEXP).map do |dot, klass, literal, escaped|
                string = literal || escaped
                if dot
                    next InputKlass.new(["\u00" .. "\u10FFFF"], false)
                elsif klass
                    negate = false
                    ranges = klass.scan(TOKEN_REGEXP_CHAR_REGEXP_MATCH).map do |negative, range, range_end, literal, escaped|
                        negate = true if negative
                        string = literal || escaped
                        if range
                            raise "incorrect range" if range > range_end
                            range .. range_end
                        elsif string
                            string .. string
                        end
                    end
                    next InputKlass.new(ranges, negate)
                elsif string
                    next InputKlass.new([string .. string], false)
                end
            end

        end
    end

    def char_terminal_sym klass
        @input_table_char ||= {}
        @input_table_char[klass] ||= Sym.new("match_class#{@input_table_char.size}")
        @input_table_char[klass]
    end

    def lexer_input_cases
        # TODO input char to terminal
        lexer.terminals.
        # for example, 
        []
    end

    def lexer_start_sym
        Sym.new("_lexer_start")
    end

    def syms grammar
        rule_content = grammar.rules.flat_map do |rule|
            [rule.lhs, *rule.rhs]
        end
        token_defs = grammar.terminals_by_ident.map {|(ident, syms)| Sym.new(ident, nil) }
        rule_content + token_defs
    end

    def start_sym_name grammar
        syms(grammar).first.name
    end

    def sym_names_dedup grammar
        syms(grammar).map(&:name).sort.uniq
    end

    def symbol_stmts grammar
        sym_names_dedup(grammar).map do |sym_name|
            %{let sym_#{sym_name} = grammar.make_symbol("#{sym_name}");}
        end.join("\n")
    end

    def rule_stmts grammar
        grammar.rules.each_with_index.map do |rule, i|
            lhs = "sym_" + rule.lhs.name
            rhs = rule.rhs.map {|sym| "sym_#{sym.name}" }.join(", ")
            "grammar.rule(#{lhs}).rhs([#{rhs}]).id(#{i}).build();"
        end.join("\n")
    end

    def generate_grammar name, for_grammar
        <<-RUST
            fn make_#{name}_grammar() -> BinarizedGrammar {
                let mut grammar = Grammar::new();
                #{symbol_stmts(for_grammar)}
                #{rule_stmts(for_grammar)}
                grammar.start_symbol(sym_#{start_sym_name(for_grammar)});
                grammar.binarize()
            }
        RUST
    end

    class Code < Struct.new(:language, :code)
    end
    
    CODE_REGEXP = /
        code \s* \( \s* (?<language>\S*) \s* \) \s* begin \n
            (?<code>[\s\S]+?)
        \n end \b
    /x

    def custom_toplevel_code language
        @grammar_str.scan(CODE_REGEXP).map do |decl|
            Code.new(decl[0], decl[1])
        end.filter do |decl|
            decl.language == language
        end.map do |decl|
            decl.code
        end
    end

    def custom_decls
        custom_toplevel_code("rust").uniq.join("\n")
    end

    def grammar_types
        result = {}

        lexer.terminals.each do |ident, tokens|
            types = tokens.map(&:type).uniq
            raise "conflicting types for #{ident}" if types.size > 1
            type = types.first
            if type.nil?
                type = "()"
            end
            result[ident] = type
        end
    
        rules.group_by(&:lhs).each do |lhs, rules|
            types = rules.map(&:type).compact.uniq
            raise "conflicting types #{types}" if types.size > 1
            type = types.first || "()"
            result[lhs.name] = type
        end

        result
    end

    def type_decls
        grammar_types.map do |ident, type|
            name = ident.camel_case
            if type == "RsTy#{name}"
                ""
            else
                "type RsTy#{name} = #{type};"
            end
        end.join("\n")
    end

    def result_type
        grammar_types[start_sym_name(parser_grammar)]
    end

    def rs_lexer_attrs tokens, type = :default
        maybe_skip = type == :ignore ? ", logos::skip" : ""
        (tokens || []).each_with_index.map do |token, ordinal|
            maybe_ordinal = type == :default ? ", |_| #{ordinal}" : ""
            case token.kind
            when :literal
                "#[token(\"#{escape_double_quotes(token.string)}\"#{maybe_skip}#{maybe_ordinal})]"
            when :regexp
                "#[regex(\"#{escape_double_quotes(token.string)}\"#{maybe_skip}#{maybe_ordinal})]"
            end
        end.join("\n")
    end

    def rule_actions
        rules.each_with_index.map do |rule, i|
            # ACTION_REGEXP = /
            #     \|(?<args>[\w\s,]+)\|
            #     (?<code>.*)
            # /x
            # block = rule.action.match(ACTION_REGEXP)
            # match_args = ""
            # if block
            #     match_args = block[:args].split(',').map(&:strip).each_with_index.map do |i, arg|
            #         "let #{arg} = args[#{i}];"
            #     end.join("\n")
            #     action_code = block[:code]
            # end
            match_args = rule.rhs.each_with_index.filter do |sym, i|
                sym.bind
            end.map do |sym, i|
                "let mut #{sym.bi
                nd} = match args[#{i}].clone() { Value::#{sym.name}(val) => val, _ => panic!(\"wrong sym\") };"
            end.join("\n")
    
            action_code = rule.action || rs_auto_code(rule)
    
            result_variant = rule.lhs.name
    
            "#{i} => {
                #{match_args}
                Value::#{result_variant}({ #{action_code} })
            }"
        end.join("\n")
    end

    def bound_syms rule
        rule.rhs.filter {|sym| !sym.bind.nil? }
    end

    def rs_auto_code rule
        if grammar_types[rule.lhs.name] == "()"
            "()"
        else
            action_fields = bound_syms(rule).map do |sym|
                "#{sym.bind},"
            end.join(" ")
        
            "#{rule_type(rule)} { #{action_fields} }"
        end
    end
    
    def rule_type rule
        "RsTy#{rule.lhs.name.camel_case}"
    end

    def auto_rule_fields rule
        bound_syms(rule).map do |sym|
            "#{sym.bind}: RsTy#{sym.name.camel_case},"
        end.join(" ")
    end

    def auto_decls
        rules.each.filter do |rule|
            rule.action.nil? && !bound_syms(rule).empty?
        end.map do |rule|
            "#[derive(Clone)] pub struct #{rule_type(rule)} { #{auto_rule_fields(rule)} }"
        end.uniq.join("\n")
    end

    def error_attrs
        rs_lexer_attrs(lexer.error, :error)
    end

    def ignore_attrs
        rs_lexer_attrs(lexer.ignore, :ignore)
    end
    
    def lexer_variants
        lexer.terminals.map do |ident, tokens|
            rs_lexer_attrs(tokens) + "\n" +
            "#{ident.camel_case}(usize),"
        end.join("\n")
    end

    def rule_value_variants
        rules.group_by(&:lhs).map do |lhs, rules|
            "#{lhs.name}(RsTy#{lhs.name.camel_case}),"
        end.join("\n")
    end

    def terminal_value_variants
        lexer.terminals.map do |ident, tokens|
            "#{ident}(RsTy#{ident.camel_case}),"
        end.join("\n")
    end
    
    def terminal_actions
        # TODO ordinal
        lexer.terminals.map do |ident, tokens|
            positive_cases = tokens.each_with_index.map do |token, ordinal|
                "if ordinal == #{ordinal} { #{token.code} }"
            end
            negative_case = "{ unreachable!() }"
            cases = positive_cases + [negative_case]
            code = cases.join(" else ")
            "if terminal == grammar.sym(\"#{ident}\").unwrap() { Value::#{ident}(#{code}) }"
        end.join(" else ")
    end

    def map_token_to_terminal
        lexer.terminals.map do |ident, tokens|
            "Token::#{ident.camel_case}(ordinal) => (grammar.sym(\"#{ident}\").unwrap(), ordinal),"
        end.join("\n")
    end

    def result_variant
        start_sym_name(parser_grammar)
    end

    class Example < Struct.new(:name, :string)
    end
    
    EXAMPLE_REGEXP = /
        example \s* \( \s* name \s* : \s* (?<name>.*) \s* \) \s* begin \n
            (?<string>[\s\S]+)
        \n end \b
    /x
    
    def examples
        @grammar_str.scan(EXAMPLE_REGEXP).map do |example|
            Example.new(example[0], example[1])
        end
    end

    def test_fns
        examples.map do |example|
            <<-RUST
            #[test]
            fn test_example_#{example.name}() {
                parse(r###"#{example.string}"###);
            }
            RUST
        end.join("\n")
    end

    def generate
        #{generate_grammar("lexer", lexer_grammar)}
        <<-RUST
            extern crate gearley_simplified;

            use gearley_simplified::{Grammar, BinarizedGrammar, Evaluator, Forest, Recognizer};

            #{custom_decls}
            #{auto_decls}
            #{type_decls}

            #{generate_grammar("parser", parser_grammar)}
            #{generate_grammar("lexer", lexer_grammar)}

            enum Token {
                #{lexer_variants}
                #{error_attrs}
                Error,
            }
            #[derive(Clone)]
            #[allow(non_camel_case_types)]
            enum Value {
                None,
                #{rule_value_variants}
                #{terminal_value_variants}
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
                            #{map_input_char_to_terminals}
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
                                #{lexer_rule_actions}
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
            pub fn parse(expr: &str) -> #{result_type} {
                let mut parser = Parser::new();
                let mut lexer = Lexer::new();
                let earlemes = lexer.lex(expr, &mut parser);
                let finished_node = recognizer.finished_node().expect("parse failed");
                let rule_eval = |rule_id, args: &[&Value]| {
                    match rule_id {
                        #{rule_actions}
                        other => panic!("unknown rule id {}", other)
                    }
                };
                let terminal_eval = |terminal, values| {
                    let info = &earlemes[values as usize];

                    let ordinal = ordinals[values as usize];
                    let slice = expr[span].to_string();
                    #{terminal_actions}
                    else {
                        Value::None
                    }
                };
                let mut evaluator = Evaluator::new(rule_eval, terminal_eval);
                let result = evaluator.
                (recognizer.forest_mut(), finished_node);
                match result {
                    Value::#{result_variant}(val) => val,
                    _ => panic!("incorrect result of eval")
                }
            }

            #{test_fns}
        RUST
    end
end

class RbGenerator < RsGenerator
    def rule_stmts
        rules.each_with_index.map do |rule, i|
            "grammar.rule lhs: #{rule.lhs}, rhs: [#{rule.rhs.join(", ")}], id: #{i}"
        end.join("\n")
    end

    def generate_grammar
        sym_name_assign = sym_names_dedup(parser_grammar).join(",\n    ")
        sym_name_arg = sym_names_dedup(parser_grammar).join("\n    ")

        <<-RUBY
            def make_grammar
                grammar = Earley::Grammar.new
                #{sym_name_assign} = grammar.make_n_symbols %w{
                    #{sym_name_arg}
                }
                #{rule_stmts}
                grammar.start_symbol = #{start_sym_name(parser_grammar)}
                grammar
            end
        RUBY
    end

    def generate
        <<-RUBY
            require_relative "../earley"
            #{generate_grammar}
            grammar = make_grammar
            p grammar
        RUBY
    end
end

def cargo_toml filebase
    <<-TOML
[package]
name = "#{filebase}"
version = "0.1.0"
edition = "2021"

[lib]
name = "#{filebase}"

[dependencies]
logos = "0.12"
gearley-simplified = { path = "../../../lib-rust", version = "0.1" }
    TOML
end

output_dir = File.join(__dir__, "output")
grammars_dir = File.join(__dir__, "grammars")
p __dir__, output_dir
`mkdir #{output_dir}`

Dir["#{grammars_dir}/*.bootstrap.txt"].each do |path|
    grammar = File.read path
    filebase = path[/#{Regexp.quote(grammars_dir)}\/(.*)\.bootstrap\.txt\Z/, 1]
    grammar_output_dir = File.join(output_dir, filebase)
    puts "cd #{output_dir}; cargo init #{filebase} --lib"
    `cd #{output_dir}; cargo init #{filebase} --lib`
    File.write "#{grammar_output_dir}.rb", RbGenerator.new(grammar).generate
    File.write "#{grammar_output_dir}/src/lib.rs", RsGenerator.new(grammar).generate
    File.write "#{grammar_output_dir}/Cargo.toml", cargo_toml(filebase)
    File.write "#{grammar_output_dir}/.gitignore", "/target/\nCargo.lock\n"
    `cd #{grammar_output_dir}; cargo fmt`
end
