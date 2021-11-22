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
grammar.rule lhs: sum, rhs: [sum, op_plus, factor], id: 0
grammar.rule lhs: sum, rhs: [sum, op_minus, factor], id: 1
grammar.rule lhs: sum, rhs: [factor], id: 2
grammar.rule lhs: factor, rhs: [factor, op_mul, expr], id: 3
grammar.rule lhs: factor, rhs: [factor, op_div, expr], id: 4
grammar.rule lhs: factor, rhs: [expr], id: 5
grammar.rule lhs: expr, rhs: [lparen, sum, rparen], id: 6
grammar.rule lhs: expr, rhs: [op_minus, sum], id: 7
grammar.rule lhs: expr, rhs: [number], id: 8
grammar.rule lhs: number, rhs: [whole], id: 9
grammar.rule lhs: number, rhs: [whole, dot, whole], id: 10
grammar.rule lhs: whole, rhs: [whole, digit], id: 11
grammar.rule lhs: whole, rhs: [digit], id: 12
grammar.start_symbol = sum
p grammar
