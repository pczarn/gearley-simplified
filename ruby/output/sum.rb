require_relative "../earley"

grammar = Earley::Grammar.new
digit,
    dot,
    expr,
    factor,
    lparen,
    number,
    op_div,
    op_minus,
    op_mul,
    op_plus,
    rparen,
    sum,
    whole = grammar.make_n_symbols %w{
    digit
    dot
    expr
    factor
    lparen
    number
    op_div
    op_minus
    op_mul
    op_plus
    rparen
    sum
    whole
}
grammar.rule lhs: #<struct Sym name="sum", bind=nil>, rhs: [#<struct Sym name="sum", bind="left">, #<struct Sym name="op_plus", bind=nil>, #<struct Sym name="factor", bind="right">], id: 0
grammar.rule lhs: #<struct Sym name="sum", bind=nil>, rhs: [#<struct Sym name="sum", bind="left">, #<struct Sym name="op_minus", bind=nil>, #<struct Sym name="factor", bind="right">], id: 1
grammar.rule lhs: #<struct Sym name="sum", bind=nil>, rhs: [#<struct Sym name="factor", bind=nil>], id: 2
grammar.rule lhs: #<struct Sym name="factor", bind=nil>, rhs: [#<struct Sym name="factor", bind=nil>, #<struct Sym name="op_mul", bind=nil>, #<struct Sym name="expr", bind=nil>], id: 3
grammar.rule lhs: #<struct Sym name="factor", bind=nil>, rhs: [#<struct Sym name="factor", bind=nil>, #<struct Sym name="op_div", bind=nil>, #<struct Sym name="expr", bind=nil>], id: 4
grammar.rule lhs: #<struct Sym name="factor", bind=nil>, rhs: [#<struct Sym name="expr", bind=nil>], id: 5
grammar.rule lhs: #<struct Sym name="expr", bind=nil>, rhs: [#<struct Sym name="lparen", bind=nil>, #<struct Sym name="sum", bind=nil>, #<struct Sym name="rparen", bind=nil>], id: 6
grammar.rule lhs: #<struct Sym name="expr", bind=nil>, rhs: [#<struct Sym name="op_minus", bind=nil>, #<struct Sym name="sum", bind=nil>], id: 7
grammar.rule lhs: #<struct Sym name="expr", bind=nil>, rhs: [#<struct Sym name="number", bind=nil>], id: 8
grammar.rule lhs: #<struct Sym name="number", bind=nil>, rhs: [#<struct Sym name="whole", bind=nil>], id: 9
grammar.rule lhs: #<struct Sym name="number", bind=nil>, rhs: [#<struct Sym name="whole", bind=nil>, #<struct Sym name="dot", bind=nil>, #<struct Sym name="whole", bind=nil>], id: 10
grammar.rule lhs: #<struct Sym name="whole", bind=nil>, rhs: [#<struct Sym name="whole", bind=nil>, #<struct Sym name="digit", bind=nil>], id: 11
grammar.rule lhs: #<struct Sym name="whole", bind=nil>, rhs: [#<struct Sym name="digit", bind=nil>], id: 12
grammar.start_symbol = #<struct Sym name="sum", bind=nil>
p grammar
