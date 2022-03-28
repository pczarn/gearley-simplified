require 'pry'

class Rule < Struct.new(:lhs, :rhs, :type, :action)
end

class Grammar < Struct.new(:rules, :lexer)
end

class Lexer < Struct.new(:terminals, :ignore, :error)
end

class TokenRegexp < Struct.new(:ident, :regexp, :type, :code)
end

class TokenString < Struct.new(:ident, :string, :type, :code)
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

def get_rules grammar
    rules = []

    ops_array = grammar.scan(TOKEN_REGEX)
    regexp_ops_array = grammar.scan(TOKEN_REGEX_REGEX)

    terminals = (
        ops_array.map {|s, ident, type, code| TokenString.new(ident, s, type, code) } +
        regexp_ops_array.map {|regexp, ident, type, code| TokenRegexp.new(ident, regexp, type, code) }
    ).group_by(&:ident)

    ignore = terminals["ignore"]

    error = terminals["error"]

    terminals.select! {|key, value| key != "ignore" && key != "error" }

    ops_hash = Hash[ops_array.map {|s, ident, _, _| [s, ident] }]
    regexp_ops_hash = Hash[regexp_ops_array.map {|r, ident, _, _| [r, ident] }]
    lexer = Lexer.new(terminals, ignore, error)

    grammar.scan(RULE_REGEXP).each do |lhs, type, rhs|
        lhs = Sym.new(lhs)
        rhs.split('|').map(&:strip).each do |alt|
            rhs_str, code = alt.split(' => ')
            rhs_syms = rhs_str.split(/\s+/)
            rhs_syms.map! do |sym|
                bind = nil
                if match = sym.match(/\A(?<bind>[_a-zA-Z]\w*):(?<val>.*)\Z/)
                    sym = match[:val]
                    bind = match[:bind]
                end
                sym_name = if shorthand = sym[/\A'(.+)'\Z/, 1]
                    raise "unknown shorthand '#{shorthand}'" if ops_hash[shorthand].nil?
                    ops_hash[shorthand]
                elsif shorthand = sym[/\A\/(.+)\/\Z/, 1]
                    raise "unknown shorthand '#{shorthand}'" if regexp_ops_hash[shorthand].nil?
                    regexp_ops_hash[shorthand]
                # elsif sym[/\A[_a-zA-Z]\w*:[_a-zA-Z]\w*\Z/]
                #     bind, sym = sym.split(':')
                #     Sym.new(sym, bind)
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
            rules << Rule.new(lhs, rhs_syms, type, code)
        end
    end

    Grammar.new(rules, lexer)
end

def get_syms grammar
    grammar.rules.flat_map do |rule|
        [rule.lhs, rule.rhs].flatten
    end + grammar.lexer.terminals.map {|(ident, syms)| Sym.new(ident, nil) }
end

def rb_grammar grammar_str
    grammar = get_rules(grammar_str)
    all_syms = get_syms(grammar)
    start_sym = all_syms.first

    all_syms_dedup = all_syms.map(&:name).sort.uniq

    sym_name_assign = all_syms_dedup.join(",\n    ")
    sym_name_arg = all_syms_dedup.join("\n    ")

    rule_stmts = grammar.rules.each_with_index.map do |rule, i|
        "grammar.rule lhs: #{rule.lhs}, rhs: [#{rule.rhs.join(", ")}], id: #{i}"
    end.join("\n")

    <<-RUBY
require_relative "../earley"

grammar = Earley::Grammar.new
#{sym_name_assign} = grammar.make_n_symbols %w{
    #{sym_name_arg}
}
#{rule_stmts}
grammar.start_symbol = #{start_sym}
p grammar
    RUBY
end

class String
    def camel_case
        return self if self !~ /_/ && self =~ /[A-Z]+.*/
        split('_').map{|e| e.capitalize}.join
    end
end

def escape_double_quotes str
    str.gsub("\"", "\\\"")
end

def rs_lexer_attrs tokens, type = :default
    maybe_skip = type == :ignore ? ", logos::skip" : ""
    (tokens || []).each_with_index.map do |token, ordinal|
        maybe_ordinal = type == :default ? ", |_| #{ordinal}" : ""
        case token
        when TokenString
            "#[token(\"#{escape_double_quotes(token.string)}\"#{maybe_skip}#{maybe_ordinal})]"
        when TokenRegexp
            "#[regex(\"#{escape_double_quotes(token.regexp)}\"#{maybe_skip}#{maybe_ordinal})]"
        end
    end.join("\n")
end

class Example < Struct.new(:name, :string)
end

EXAMPLE_REGEXP = /
    example \s* \( \s* name \s* : \s* (?<name>.*) \s* \) \s* begin \n
        (?<string>[\s\S]+)
    \n end \b
/x

def get_examples(grammar_str)
    grammar_str.scan(EXAMPLE_REGEXP).map do |example|
        Example.new(example[0], example[1])
    end
end

class Code < Struct.new(:language, :code)
end

CODE_REGEXP = /
    code \s* \( \s* (?<language>\S*) \s* \) \s* begin \n
        (?<code>[\s\S]+?)
    \n end \b
/x

def get_code(grammar_str, language)
    grammar_str.scan(CODE_REGEXP).map do |decl|
        Code.new(decl[0], decl[1])
    end.filter do |decl|
        decl.language == language
    end.map do |decl|
        decl.code
    end
end

def rs_auto_code(rule, codes)
    type = rule.lhs.name.camel_case

    syms = rule.rhs.filter do |sym|
        !sym.bind.nil?
    end
    
    fields = syms.map do |sym|
        "#{sym.bind}: RsTy#{sym.name.camel_case},"
    end.join(" ")
    
    action_fields = syms.map do |sym|
        "#{sym.bind},"
    end.join(" ")

    codes << "#[derive(Clone)] pub struct RsTy#{type} { #{fields} }"

    if rule.type.nil?
        rule.type = "RsTy#{type}"
    end

    "RsTy#{type} { #{action_fields} }"
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
        "if terminal == sym_#{ident} { Value::#{ident}(#{code}) }"
    end.join(" else ")

    # Token::OpMinus => op_minus,
    # Token::Dot => dot,
    # Token::Digit => digit,
    # Token::Lparen => lparen,
    # Token::Rparen => rparen,
    # Token::OpFactor => op_factor,
    # Token::OpPlus => op_plus,

    map_token_to_terminal = grammar.lexer.terminals.map do |ident, tokens|
        "Token::#{ident.camel_case}(ordinal) => (sym_#{ident}, ordinal),"
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

        use gearley_simplified::{Grammar, Evaluator, Forest, Recognizer};

        use logos::Logos;

        #{rust_code}
        #{rs_types}

        #[allow(unused_braces)]
        pub fn parse(expr: &str) -> #{result_type} {
            let mut grammar = Grammar::new();
            #{symbol_stmts}
            #{rule_stmts}
            grammar.start_symbol(sym_#{start_sym_name});
            let binarized_grammar = grammar.binarize();
            let mut recognizer = Recognizer::new(binarized_grammar);
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
            let mut evaluator = Evaluator::new(
                |rule_id, args: &[Value]| {
                    match rule_id {
                        #{rule_actions}
                        other => panic!("unknown rule id {}", other)
                    }
                },
                |terminal, values| {
                    let span = spans[values as usize].clone();
                    let ordinal = ordinals[values as usize];
                    let slice = expr[span].to_string();
                    #{terminal_actions}
                    else {
                        Value::None
                    }
                }
            );
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

`mkdir output`

Dir['grammars/*.bootstrap.txt'].each do |path|
    grammar = File.read path
    filebase = path[/\/(.*)\.bootstrap\.txt\Z/, 1]
    `cargo init output/#{filebase} --lib`
    File.write "output/#{filebase}.rb", rb_grammar(grammar)
    File.write "output/#{filebase}/src/lib.rs", rs_grammar(grammar)
    File.write "output/#{filebase}/Cargo.toml", cargo_toml(filebase)
    File.write "output/#{filebase}/.gitignore", "/target/\nCargo.lock\n"
    `cd output/#{filebase}; cargo fmt`
end
