            require_relative "../earley"
                        def make_grammar
                grammar = Earley::Grammar.new
                alnum,
    alnum_str,
    alpha,
    any,
    arrow,
    doublequote,
    grammar,
    ident,
    literal_token_rule,
    lparen,
    nonterminal_rule,
    op_bnf,
    op_colon,
    op_plus,
    op_star,
    quote,
    regexp_token_rule,
    rparen,
    rule,
    rule_rhs,
    slash,
    string,
    terminal_rule = grammar.make_n_symbols %w{
                    alnum
    alnum_str
    alpha
    any
    arrow
    doublequote
    grammar
    ident
    literal_token_rule
    lparen
    nonterminal_rule
    op_bnf
    op_colon
    op_plus
    op_star
    quote
    regexp_token_rule
    rparen
    rule
    rule_rhs
    slash
    string
    terminal_rule
                }
                grammar.rule lhs: #<struct Sym name="grammar", bind=nil>, rhs: [#<struct Sym name="grammar", bind=nil>, #<struct Sym name="rule", bind=nil>], id: 0
grammar.rule lhs: #<struct Sym name="grammar", bind=nil>, rhs: [#<struct Sym name="rule", bind=nil>], id: 1
grammar.rule lhs: #<struct Sym name="rule", bind=nil>, rhs: [#<struct Sym name="nonterminal_rule", bind=nil>], id: 2
grammar.rule lhs: #<struct Sym name="rule", bind=nil>, rhs: [#<struct Sym name="terminal_rule", bind=nil>], id: 3
grammar.rule lhs: #<struct Sym name="nonterminal_rule", bind=nil>, rhs: [#<struct Sym name="ident", bind=nil>, #<struct Sym name="op_bnf", bind=nil>, #<struct Sym name="rule_rhs", bind=nil>], id: 4
grammar.rule lhs: #<struct Sym name="terminal_rule", bind=nil>, rhs: [#<struct Sym name="literal_token_rule", bind=nil>], id: 5
grammar.rule lhs: #<struct Sym name="terminal_rule", bind=nil>, rhs: [#<struct Sym name="regexp_token_rule", bind=nil>], id: 6
grammar.rule lhs: #<struct Sym name="literal_token_rule", bind=nil>, rhs: [#<struct Sym name="quote", bind=nil>, #<struct Sym name="string", bind="string">, #<struct Sym name="quote", bind=nil>, #<struct Sym name="arrow", bind=nil>, #<struct Sym name="ident", bind="ident">], id: 7
grammar.rule lhs: #<struct Sym name="regexp_token_rule", bind=nil>, rhs: [#<struct Sym name="slash", bind=nil>, #<struct Sym name="string", bind="string">, #<struct Sym name="slash", bind=nil>, #<struct Sym name="arrow", bind=nil>, #<struct Sym name="ident", bind="ident">], id: 8
grammar.rule lhs: #<struct Sym name="string", bind=nil>, rhs: [#<struct Sym name="string", bind="s">, #<struct Sym name="any", bind="ch">], id: 9
grammar.rule lhs: #<struct Sym name="string", bind=nil>, rhs: [#<struct Sym name="any", bind="ch">], id: 10
grammar.rule lhs: #<struct Sym name="ident", bind=nil>, rhs: [#<struct Sym name="alpha", bind="ch">, #<struct Sym name="alnum_str", bind="s">], id: 11
grammar.rule lhs: #<struct Sym name="alnum_str", bind=nil>, rhs: [#<struct Sym name="alnum_str", bind="s">, #<struct Sym name="alnum", bind="ch">], id: 12
grammar.rule lhs: #<struct Sym name="alnum_str", bind=nil>, rhs: [#<struct Sym name="alnum", bind="ch">], id: 13
grammar.rule lhs: #<struct Sym name="grammar", bind=nil>, rhs: [#<struct Sym name="grammar", bind=nil>, #<struct Sym name="rule", bind=nil>], id: 14
grammar.rule lhs: #<struct Sym name="grammar", bind=nil>, rhs: [#<struct Sym name="rule", bind=nil>], id: 15
                grammar.start_symbol = grammar
                grammar
            end

            grammar = make_grammar
            p grammar
