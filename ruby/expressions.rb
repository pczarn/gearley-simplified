require_relative "earley"

def calc expression
    grammar = Earley::Grammar.new
    digit,
        dot,
        expr_sym,
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
    grammar.rule lhs: factor, rhs: [factor, op_mul, expr_sym], id: 3
    grammar.rule lhs: factor, rhs: [factor, op_div, expr_sym], id: 4
    grammar.rule lhs: factor, rhs: [expr_sym], id: 5
    grammar.rule lhs: expr_sym, rhs: [lparen, sum, rparen], id: 6
    grammar.rule lhs: expr_sym, rhs: [op_minus, expr_sym], id: 7
    grammar.rule lhs: expr_sym, rhs: [number], id: 8
    grammar.rule lhs: number, rhs: [whole], id: 9
    grammar.rule lhs: number, rhs: [whole, dot, whole], id: 10
    grammar.rule lhs: whole, rhs: [whole, digit], id: 11
    grammar.rule lhs: whole, rhs: [digit], id: 12
    grammar.start_symbol = sum
    binarized_grammar = grammar.binarize
    recognizer = Earley::Recognizer.new binarized_grammar
    expression.each_char.each_with_index do |ch, i|
        terminal = case ch
        when '-' then op_minus
        when '.' then dot
        when '0'..'9' then digit
        when '(' then lparen
        when ')' then rparen
        when '*' then op_mul
        when '/' then op_div
        when '+' then op_plus
        when ' ' then next
        else raise "invalid character #{ch}"
        end
        recognizer.begin_earleme!
        recognizer.scan! terminal, ch
        result = recognizer.end_earleme!
        raise "parse failed at #{i}" if !result
    end
    finished_node = recognizer.finished_node
    raise "parse failed" if finished_node.nil?
    evaluator = Earley::Evaluator.new(
        eval_product: lambda do |rule_id, args|
            case rule_id
            when 0
                left, _, right = args
                left + right
            when 1
                left, _, right = args
                left - right
            when 2
                args.first
            when 3
                left, _, right = args
                left * right
            when 4
                left, _, right = args
                left / right
            when 5
                args.first
            when 6
                args[1]
            when 7
                -args[1]
            when 8
                args.first.to_f
            when 9
                args.first
            when 10
                args[0] + '.' + args[2]
            when 11
                args[0] + args[1]
            when 12
                args.first
            else
                raise "unknown rule #{rule_id}"
            end
        end,
        eval_leaf: lambda do |terminal, values|
            values
        end
    )
    result = evaluator.evaluate recognizer.forest, finished_node
    if result.is_a?(Array) && result.first.is_a?(Float)
        result.first
    else
        raise "evaluation failed"
    end
end

p calc "((2.33 / (2.9+3.5)*4) - -6)"
p calc '12* 123/-(-5 + 2)'