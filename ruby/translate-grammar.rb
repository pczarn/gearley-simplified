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
        Grammar.new # TODO
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
                "let mut #{sym.bind} = match args[#{i}].clone() { Value::#{sym.name}(val) => val, _ => panic!(\"wrong sym\") };"
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
        action_fields = bound_syms(rule).map do |sym|
            "#{sym.bind},"
        end.join(" ")
    
        "#{rule_type(rule)} { #{action_fields} }"
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
            rule.action.nil?
        end.map do |rule|
            "#[derive(Clone)] pub struct #{rule_type(rule)} { #{auto_rule_fields(rule)} }"
        end
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
            extern crate logos;
            extern crate gearley_simplified;

            use gearley_simplified::{Grammar, BinarizedGrammar, Evaluator, Forest, Recognizer};

            use logos::Logos;

            #{custom_decls}
            #{auto_decls}
            #{type_decls}

            #{generate_grammar("parser", parser_grammar)}

            #[allow(unused_braces)]
            pub fn parse(expr: &str) -> #{result_type} {
                let grammar = make_grammar();
                let mut recognizer = Recognizer::new(&grammar);
                #[derive(Logos)]
                enum Token {
                    #{lexer_variants}
                    #{ignore_attrs}
                    #{error_attrs}
                    #[error]
                    Error,
                }
                #[derive(Clone)]
                #[allow(non_camel_case_types)]
                enum Value {
                    None,
                    #{rule_value_variants}
                    #{terminal_value_variants}
                }
                let mut lex = Token::lexer(expr);
                let mut spans = vec![];
                let mut ordinals = vec![];
                while let Some(token) = lex.next() {
                    spans.push(lex.span());
                    recognizer.begin_earleme();
                    let (terminal, ordinal) = match token {
                        #{map_token_to_terminal}
                        Token::Error => {
                            let span = lex.span();
                            panic!("lexing error at {:?}", span);
                        },
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
                let rule_eval = |rule_id, args: &[Value]| {
                    match rule_id {
                        #{rule_actions}
                        other => panic!("unknown rule id {}", other)
                    }
                };
                let terminal_eval = |terminal, values| {
                    let span = spans[values as usize].clone();
                    let ordinal = ordinals[values as usize];
                    let slice = expr[span].to_string();
                    #{terminal_actions}
                    else {
                        Value::None
                    }
                };
                let mut evaluator = Evaluator::new(rule_eval, terminal_eval);
                let result = evaluator.evaluate(recognizer.forest_mut(), finished_node);
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

def rs_grammar grammar_str
    grammar = get_rules(grammar_str)
    all_syms = get_syms(grammar)
    start_sym_name = all_syms.first.name
    all_syms_dedup = all_syms.map(&:name).sort.uniq

    # sym_name_assign = all_syms_dedup.join(",\n    ")
    # sym_name_arg = all_syms_dedup.join("\n    ")

    symbol_stmts = all_syms_dedup.map do |sym_name|
        %{let sym_#{sym_name} = grammar.make_symbol("#{sym_name}");}
    end.join("\n")

    rule_stmts = grammar.rules.each_with_index.map do |rule, i|
        rhs = rule.rhs.map {|sym| "sym_#{sym.name}" }.join(", ")
        "grammar.rule(sym_#{rule.lhs.name}).rhs([#{rhs}]).id(#{i}).build();"
    end.join("\n")

    grammar_content = GrammarContent.new(symbol_stmts, rule_stmts, start_sym_name)

    grammar_types = {}

    rust_code = get_code(grammar_str, "rust")

    rule_actions = grammar.rules.each_with_index.map do |rule, i|
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
        match_args = rule.rhs.each_with_index.map do |sym, i|
            if sym_bind = sym.bind
                ["let mut #{sym_bind} = match args[#{i}].clone() { Value::#{sym.name}(val) => val, _ => panic!(\"wrong sym\") };"]
            else
                []
            end
        end.flatten(1).join("\n")

        action_code = rule.action || rs_auto_code(rule, rust_code)

        result_variant = rule.lhs.name

        "#{i} => {
            #{match_args}
            Value::#{result_variant}({ #{action_code} })
        }"
    end.join("\n")

    rust_code = rust_code.uniq.join("\n")

    error_attrs = rs_lexer_attrs(grammar.lexer.error, :error)
    ignore_attrs = rs_lexer_attrs(grammar.lexer.ignore, :ignore)
    lexer_variants = grammar.lexer.terminals.map do |ident, tokens|
        rs_lexer_attrs(tokens) + "\n" +
        "#{ident.camel_case}(usize),"
    end.join("\n")

    grammar.lexer.terminals.each do |ident, tokens|
        types = tokens.map(&:type).uniq
        raise "conflicting types for #{ident}" if types.size > 1
        type = types.first
        if type.nil?
            type = "()"
        end
        grammar_types[ident] = type
    end

    grammar.rules.group_by(&:lhs).each do |lhs, rules|
        types = rules.map(&:type).compact.uniq
        raise "conflicting types #{types}" if types.size > 1
        type = types.first || "()"
        grammar_types[lhs.name] = type
    end

    rule_value_variants = grammar.rules.group_by(&:lhs).map do |lhs, rules|
        "#{lhs.name}(RsTy#{lhs.name.camel_case}),"
    end.join("\n")

    rs_types = grammar_types.map do |ident, type|
        name = ident.camel_case
        if type == "RsTy#{name}"
            ""
        else
            "type RsTy#{name} = #{type};"
        end
    end.join("\n")

    terminal_value_variants = grammar.lexer.terminals.map do |ident, tokens|
        "#{ident}(RsTy#{ident.camel_case}),"
    end.join("\n")

    terminal_actions = grammar.lexer.terminals.map do |ident, tokens|
        positive_cases = tokens.each_with_index.map do |token, ordinal|
            "if ordinal == #{ordinal} { #{token.code} }"
        end
        negative_case = "{ unreachable!() }"
        cases = positive_cases + [negative_case]
        code = cases.join(" else ")
        "if terminal == grammar.sym(\"#{ident}\").unwrap() { Value::#{ident}(#{code}) }"
    end.join(" else ")

    # Token::OpMinus => op_minus,
    # Token::Dot => dot,
    # Token::Digit => digit,
    # Token::Lparen => lparen,
    # Token::Rparen => rparen,
    # Token::OpFactor => op_factor,
    # Token::OpPlus => op_plus,

    map_token_to_terminal = grammar.lexer.terminals.map do |ident, tokens|
        "Token::#{ident.camel_case}(ordinal) => (grammar.sym(\"#{ident}\").unwrap(), ordinal),"
    end.join("\n")

    result_type = grammar_types[grammar.rules.first.lhs.name]
    result_variant = grammar.rules.first.lhs.name

    examples = get_examples(grammar_str)

    test_fns = examples.map do |example|
        <<-RUST
        #[test]
        fn test_example_#{example.name}() {
            parse(r###"#{example.string}"###);
        }
        RUST
    end.join("\n")

    # lexer_str_stmts = grammar.terminals.map do |str, ident|
    #     "lexer.add(\"#{str}\", op_minus);"
    # end.join("\n")

    # lexer_regexp_stmts = grammar.regexp_terminals.map do |regexp_str, ident|
    #     "lexer.add_regexp(Regexp::new(\"#{regexp_str}\"), #{ident});"
    # end.join("\n")

    # lexer_ignore_stmt = grammar.ignore_regexp.nil ? "" : "lexer.ignore(Regexp::new(\"#{grammar.ignore_regexp}\"));"

    <<-RUST
        extern crate logos;
        extern crate gearley_simplified;

        use gearley_simplified::{Grammar, BinarizedGrammar, Evaluator, Forest, Recognizer};

        use logos::Logos;

        #{rust_code}
        #{rs_types}

        fn make_grammar() -> BinarizedGrammar {
            let mut grammar = Grammar::new();
            #{symbol_stmts}
            #{rule_stmts}
            grammar.start_symbol(sym_#{start_sym_name});
            grammar.binarize()
        }

        #[allow(unused_braces)]
        pub fn parse(expr: &str) -> #{result_type} {
            let grammar = make_grammar();
            let mut recognizer = Recognizer::new(&grammar);
            #[derive(Logos)]
            enum Token {
                #{lexer_variants}
                #{ignore_attrs}
                #{error_attrs}
                #[error]
                Error,
            }
            #[derive(Clone)]
            #[allow(non_camel_case_types)]
            enum Value {
                None,
                #{rule_value_variants}
                #{terminal_value_variants}
            }
            let mut lex = Token::lexer(expr);
            let mut spans = vec![];
            let mut ordinals = vec![];
            while let Some(token) = lex.next() {
                spans.push(lex.span());
                recognizer.begin_earleme();
                let (terminal, ordinal) = match token {
                    #{map_token_to_terminal}
                    Token::Error => {
                        let span = lex.span();
                        panic!("lexing error at {:?}", span);
                    },
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
            let rule_eval = |rule_id, args: &[Value]| {
                match rule_id {
                    #{rule_actions}
                    other => panic!("unknown rule id {}", other)
                }
            };
            let terminal_eval = |terminal, values| {
                let span = spans[values as usize].clone();
                let ordinal = ordinals[values as usize];
                let slice = expr[span].to_string();
                #{terminal_actions}
                else {
                    Value::None
                }
            };
            let mut evaluator = Evaluator::new(rule_eval, terminal_eval);
            let result = evaluator.evaluate(recognizer.forest_mut(), finished_node);
            match result {
                Value::#{result_variant}(val) => val,
                _ => panic!("incorrect result of eval")
            }
        }

        #{test_fns}
    RUST
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
