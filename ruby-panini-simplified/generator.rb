require 'erb'
require 'forwardable'

class Rule < Struct.new(:lhs, :rhs, :type, :action)
end

class Grammar < Struct.new(:name, :rules, :terminals)
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


class String
    def camel_case
        return self if self !~ /_/ && self =~ /[A-Z]+.*/
        split('_').map{|e| e.capitalize}.join
    end
end

def escape_double_quotes str
    str.gsub("\"", "\\\"")
end

module Rs
    module LexerReader
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

        def literal_tokens
            grammar_str.scan(TOKEN_REGEX).map do |s, ident, type, code|
                Token.new(:literal, ident, s, type, code)
            end
        end
    
        def regexp_tokens
            grammar_str.scan(TOKEN_REGEX_REGEX).map do |regexp, ident, type, code|
                Token.new(:regexp, ident, regexp, type, code)
            end
        end
    
        def tokens
            literal_tokens_array + regexp_tokens_array
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
    end

    class LexerGenerator
        include LexerReader

        def tokens_by_kind_and_string
            Hash[tokens.map {|t| [[t.kind, t.string], t.ident] }]
        end
    
        def literal_by_string shorthand
            tokens_by_kind_and_string[[:literal, shorthand]]
        end
    
        def regexp_by_string shorthand
            tokens_by_kind_and_string[[:regexp, shorthand]]
        end
    end

    module ParserReader
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

        RHS_SYM_REGEXP = /
            \A
            (?:
                (?<bind> [_a-zA-Z]\w* )
                :
            )?
            (?<sym>
                (?<literal> '(.+)' ) |
                (?<regexp> \/.+\/ ) |
                (?<name> [_a-zA-Z]\w* )
            )
            \Z
        /x

        RHS_SHORTHAND_REGEXP = /
            \A
            (?<literal> '(.+)' ) |
            (?<regexp> \/.+\/ )
            \Z
        /x

        class RuleAst < Struct.new(:lhs, :rhs, :type, :action)
        end

        def rule_asts
            grammar_str.scan(RULE_REGEXP).flat_map do |lhs, type, rhs|
                rhs.split('|').map(&:strip).map do |alt|
                    rhs_str, code = alt.split(' => ')
                    rhs_syms = rhs_str.split(/\s+/).map do |sym|
                        match = sym.match(BOUND_SYM_REGEXP)
                        raise "wrong symbol #{sym}" if match.nil?
                        Sym.new(name[:sym], match[:bind])
                    end
                    if code
                        code = code.strip[/\A\{(.+)\}\Z/, 1]
                        raise "wrong code in #{alt}" if code.nil?
                    end
                    RuleAst.new(lhs, rhs_syms, type, code)
                end
            end
        end
    end

    class ParserGenerator < LexerGenerator
        include ParserReader

        class Rule < Struct.new(:lhs, :rhs, :type, :action)
        end

        def terminal_name shorthand

        end
    
        def rules
            rule_asts.map do |rule|
                rhs = rule.rhs.map do |sym|
                    new_sym = sym
                    if match = sym.name.match(RHS_SHORTHAND_REGEXP)
                        if match[:literal]
                            new_sym = Sym.new(terminal_name(match[:literal]), sym.bind) 
                        else
                            new_sym = Sym.new(terminal_name(match[:regexp]), sym.bind) 
                        end
                    end
                    raise "unknown symbol #{sym.inspect}" if sym.name.nil?
                    new_sym
                end
                Rule.new(rule.lhs, rhs, type, code)
            end
        end
    end

    class Generator
        def_delegators :@lexer, :
        def_delegators :@parser, :

        attr_reader :grammar_str

        def initialize grammar_str
            @lexer = LexerGenerator.new grammar_str
            @parser = ParserGenerator.new grammar_str
        end

        def generate
            template = File.read("rs-template.erb.rs")
            template.gsub!(/^\s+% /, '% ')
            rhtml = ERB.new(template, trim_mode: '%')
            rhtml.result(binding)
        end
    end
end

class RsGenerator
    attr_reader :grammar_str

    def initialize grammar_str
        @grammar_str = grammar_str
    end

    def literal_tokens_array
        grammar_str.scan(TOKEN_REGEX).map {|s, ident, type, code| Token.new(:literal, ident, s, type, code) }
    end

    def regexp_tokens_array
        grammar_str.scan(TOKEN_REGEX_REGEX).map {|regexp, ident, type, code| Token.new(:regexp, ident, regexp, type, code) }
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
        Grammar.new("parser", rules, lexer.terminals.keys)
    end

    def lexer_grammar
        Grammar.new("lexer", lexer_rules, terminals)
    end

    class LexerRuleAction < Struct.new(:index, :code)
    end

    def lexer_rule_actions
        lexer_rules.each_with_index.map do |rule, i|
            LexerRuleAction.new(i, rule.action)
        end
    end

    def lexer_rules
        lexer_toplevel_rules + lexer_terminal_rules
    end

    def lexer_toplevel_rules
        lexer.terminals.map do |ident, tokens|
            Rule.new(lexer_start_sym, [Sym.new("_guard_" + ident, nil), Sym.new("_content" + ident, "arg")], "usize", "args[0].unwrap()")
        end
    end

    def all_tokens
        lexer.terminals.flat_map do |ident, tokens|
            tokens.map do |token|
                [ident, token]
            end
        end
    end

    def lexer_terminal_rules
        all_tokens.each_with_index.map do |(ident, token), i|
            rhs = lexer_klasses_for_token(token).map do |klass|
                lexer_input_table[klass]
            end
            Rule.new(Sym.new("_content" + ident), rhs, "usize", i.to_s)
        end
    end

    def map_input_char_to_terminals
        lexer_input_cases.map do |char, terminals|
            terminals = terminals.join(", ")
            "'#{char}' => &[#{terminals}],"
        end.join("\n")
    end

    TOKEN_REGEXP_CHAR_REGEXP_KLASS = /
        (?<negative> \A \^ ) |
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

    class InputKlass < Struct.new(:ranges, :negate)
        def <=>(other)
            [ranges, negate] <=> [other.ranges, other.negate]
        end
    end

    def parse_regexp string
        string.scan(TOKEN_REGEXP_CHAR_REGEXP).map do |dot, klass, literal, escaped|
            string = literal || escaped
            if dot
                next InputKlass.new(["\x00" .. "\u{10FFFF}"], false)
            elsif klass
                negate = false
                ranges = klass.scan(TOKEN_REGEXP_CHAR_REGEXP_KLASS).map do |negative, range, range_end, literal, escaped|
                    negate = true if negative
                    string = literal || escaped
                    if range
                        raise "incorrect range" if range > range_end
                        range .. range_end
                    elsif string
                        string .. string
                    end
                end.compact.sort_by {|range| [range.first, range.last] }.uniq
                next InputKlass.new(ranges, negate)
            elsif string
                next InputKlass.new([string .. string], false)
            else
                raise "unreachable"
            end
        end
    end

    def lexer_klasses
        lexer.terminals.flat_map do |ident, tokens|
            tokens.flat_map do |token|
                lexer_klasses_for_token(token)
            end
        end
    end

    def lexer_input_table
        Hash[lexer_klasses.uniq.each_with_index.map {|klass, i| [klass, Sym.new("match_class#{i}")] }]
    end

    def terminals
        lexer_input_table.values.map {|sym| sym.name }
    end

    def lexer_klasses_for_token token
        # TODO lexer rhs
        if token.kind == :literal
            token.string.gsub("\\\\", "\\").gsub("\\'", "'").split('').map do |char|
                InputKlass.new([char .. char], false)
            end
        elsif token.kind == :regexp
            parse_regexp(token.string)
        end
    end

    # def char_terminal_sym klass
    #     klass.ranges.sort_by! {|range| [range.first, range.last] }
    #     klass.ranges.uniq!
    #     @input_table_char ||= {}
    #     @input_table_char[klass] ||= Sym.new("match_class#{@input_table_char.size}")
    #     @input_table_char[klass]
    # end

    class AnyInputMatcher < Struct.new(:sym_name)
    end

    class InputMatcher < Struct.new(:sym_name, :ranges, :negate)
    end

    def rs_char_match char
        result = char.inspect.gsub(/\A"(.+)"\Z/, "'\\1'")
        if result == "'''"
            result = "'\\''"
        end
        result
    end

    def lexer_input_cases
        lexer_input_table.flat_map do |klass, sym|
            if klass.ranges == ["\u0000" .. "\u{10FFFF}"]
                if klass.negate
                    []
                else
                    [AnyInputMatcher.new(sym.name)]
                end
            else
                [InputMatcher.new(
                    sym.name,
                    klass.ranges.map do |range|
                        if range.first == range.last
                            rs_char_match(range.first)
                        else
                            "#{rs_char_match(range.first)} ..= #{rs_char_match(range.last)}"
                        end
                    end,
                    klass.negate,
                )]
            end
        end
    end

    def lexer_start_sym
        Sym.new("_lexer_start")
    end

    def syms grammar
        rule_content = grammar.rules.flat_map do |rule|
            [rule.lhs, *rule.rhs]
        end
        token_defs = grammar.terminals.map {|name| Sym.new(name, nil) }
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

    def grammars
        [parser_grammar, lexer_grammar]
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

    def custom_toplevel_code langua<ge
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

    class RuleActionArg < Struct.new(:bind, :index, :variant)
    end

    class RuleAction < Struct.new(:index, :args, :result_variant, :code)
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
                RuleActionArg.new(sym.bind, i, sym.name) 
            end
    
            action_code = rule.action || rs_auto_code(rule)
    
            result_variant = rule.lhs.name
    
            RuleAction.new(i, match_args, result_variant, action_code)
        end
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

    class AutoDecl < Struct.new(:lhs_name, :bound_syms)
    end

    def auto_decls
        rules.each.filter do |rule|
            rule.action.nil? && !bound_syms(rule).empty?
        end.map do |rule|
            AutoDecl.new(rule.lhs, bound_syms(rule))
        end.uniq
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
        rules.map(&:lhs).uniq.map do |lhs|
            "#{lhs.name}(RsTy#{lhs.name.camel_case}),"
        end.join("\n")
    end

    def terminal_value_variants
        lexer.terminals.map do |ident, tokens|
            "#{ident}(RsTy#{ident.camel_case}),"
        end.join("\n")
    end
    
    class TerminalAction < Struct.new(:index, :variant, :code)
    end

    def terminal_actions
        all_tokens.each_with_index.map do |(ident, token), i|
            TerminalAction.new(i, ident, token.code)
        end
        # TODO ordinal
        # lexer.terminals.map do |ident, tokens|
        #     positive_cases = tokens.each_with_index.map do |token, ordinal|
        #         "if ordinal == #{ordinal} { #{token.code} }"
        #     end
        #     negative_case = "{ unreachable!() }"
        #     cases = positive_cases + [negative_case]
        #     code = cases.join(" else ")
        #     "if terminal == parser.recognizer.sym(\"#{ident}\").unwrap() { Value::#{ident}(#{code}) }"
        # end
    end

    def terminal_sym_names
        all_tokens.map do |ident, token|
            ident
        end
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
        template = File.read("rs-template.erb.rs")
        template.gsub!(/^\s+% /, '% ')
        rhtml = ERB.new(template, trim_mode: '%')
        rhtml.result(binding)
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