require "rubygems"
require "algorithms"
require "bitarray"

# Resources:
#
# • Context free language parsing with Earley Algorithm
#   https://dev.to/jennieji/context-free-language-parsing-with-earley-algorithm-2kp3
#
# • Earley Parsing Explained
#   https://loup-vaillant.fr/tutorials/earley-parsing/

module Earley
    class Symbol < Struct.new(:id)
        include Comparable

        def inspect
            "Symbol(#{id})"
        end

        def <=> other
            id <=> other.id
        end
    end

    class Rule < Struct.new(:lhs, :rhs, :id)
    end

    class BinarizedRule < Struct.new(:lhs, :rhs0, :rhs1, :id)
    end

    class Leaf < Struct.new(:terminal, :values, :alive)
        def initialize(terminal, values, alive = false); super; end
    end

    class Sum < Struct.new(:summands, :alive)
        def initialize(summands, alive = false); super; end
    end

    class Product < Struct.new(:action, :left_node, :right_node)
    end

    class Evaluated < Struct.new(:values)
    end

    class NodeHandle < Struct.new(:index)
    end

    class Item < Struct.new(:origin, :dot, :node)
    end

    class CompletedItem < Struct.new(:origin, :dot, :left_node, :right_node)
        include Comparable

        def <=> other
            [origin, dot] <=> [other.origin, other.dot]
        end
    end

    class BitVec < BitArray
        def or other
            other.each_with_index do |bit, i|
                self[i] = true if bit
            end
        end
    end

    class BitMatrix < Array
        def initialize num_cols, num_rows
            super(num_rows) { BitVec.new num_cols }
        end

        def transitive_closure!
            n = [size, first.size].min
            n.times do |pos|
                rows = self[0 ... pos] + self[pos + 1 .. -1]
                src_row = self[pos]
                rows.each do |dst_row|
                    dst_row.or src_row if dst_row[pos]
                end
            end
        end

        def reflexive_closure!
            [size, first.size].min.times do |i|
                self[i][i] = true
            end
        end
    end

    class MaybePostdot < Struct.new(:rhs1)
        include Comparable

        def initialize(rhs1 = nil); super; end

        def <=> other
            [rhs1 ? 0 : 1, rhs1] <=> [other.rhs1 ? 0 : 1, other.rhs1]
        end
    end

    class SymbolSource
        def initialize
            @next_symbol = 0
            @symbol_names = []
        end

        def make_symbol word
            id = @next_symbol
            @next_symbol += 1
            @symbol_names << word
            Symbol.new id
        end

        def make_n_symbols words
            words.map {|word| make_symbol word }
        end

        def num_syms
            @next_symbol
        end
    end

    class Grammar
        attr_accessor :start_symbol

        def initialize
            @symbol_source = SymbolSource.new
            @rules = []
            # start_symbol is nil
        end

        def make_symbol word
            @symbol_source.make_symbol word
        end

        def make_n_symbols words
            @symbol_source.make_n_symbols words
        end

        def rule lhs:, rhs:, id:
            @rules << Rule.new(lhs, rhs, id)
        end

        def binarize
            binarized_rules = @rules.flat_map do |rule|
                case rule.rhs.size
                when 0
                    raise "rules of length 0 are forbidden"
                when 1
                    [BinarizedRule.new(rule.lhs, rule.rhs.first, nil, rule.id)]
                else
                    num_additional_symbols = rule.rhs.size - 2
                    gensym_names = num_additional_symbols.times.map {|n| "g#{n}" }
                    gensyms = @symbol_source.make_n_symbols gensym_names
                    lhs_ary = gensyms + [rule.lhs]
                    rhs0_ary = [rule.rhs[0]] + gensyms
                    rhs1_ary = rule.rhs[1..-1]
                    lhs_ary.zip(rhs0_ary, rhs1_ary).each_with_index.map do |(lhs, rhs0, rhs1)|
                        BinarizedRule.new(lhs, rhs0, rhs1, lhs == rule.lhs ? rule.id : nil)
                    end
                end
            end
            result = BinarizedGrammar.new(rules: binarized_rules, symbol_source: @symbol_source)
            result.start_symbol = @start_symbol
            result
        end
    end

    class BinarizedGrammar
        attr_reader :rules
        attr_accessor :start_symbol

        def initialize rules:, symbol_source:
            @rules = rules
            @symbol_source = symbol_source
        end

        def make_symbol word
            @symbol_source.make_symbol word
        end

        def make_n_symbols words
            @symbol_source.make_n_symbols words
        end

        def num_syms
            @symbol_source.num_syms
        end

        def sort_rules
            @rules.sort_by(&:lhs)
        end

        def eval
            @rules.map(&:id)
        end
    end

    class EarleySet
        attr_accessor :predicted, :medial

        def initialize grammar
            @predicted = BitVec.new grammar.num_syms
            @medial = []
        end
    end

    class Recognizer
        attr_reader :finished_node, :forest

        def initialize grammar
            grammar.sort_rules
            @grammar = grammar
            @tables = Tables.new(grammar)
            @forest = Forest.new(grammar)
            @earley_chart = [EarleySet.new(grammar)]
            @next_set = EarleySet.new(grammar)
            @complete = Containers::MaxHeap.new
            # finished_node is nil 
            @earley_chart.first.predicted = @tables.prediction_matrix[grammar.start_symbol.id]
        end

        def begin_earleme!
            # nothing to do
        end

        def scan! terminal, values
            node = @forest.leaf terminal, values
            complete! earleme, terminal, node
        end

        def end_earleme!
            if is_exhausted?
                false
            else
                @finished_node = nil
                complete_all_sums_entirely!
                sort_medial_items!
                prediction_pass!
                @earley_chart << @next_set
                @next_set = EarleySet.new(@grammar)
                true
            end
        end

        def complete_all_sums_entirely!
            while ei = @complete.max
                lhs_sym = @tables.get_lhs ei.dot
                while ei2 = @complete.max
                    if ei.origin == ei2.origin && lhs_sym == @tables.get_lhs(ei2.dot)
                        @forest.push_summand ei2
                        @complete.pop
                    else
                        break
                    end
                end
                node = @forest.sum lhs_sym, ei.origin
                if ei.origin == 0 && lhs_sym == @grammar.start_symbol
                    @finished_node = node
                end
                complete! ei.origin, lhs_sym, node
            end
        end

        def sort_medial_items!
            @next_set.medial.sort_by! do |item|
                [@tables.get_rhs1_cmp(item.dot), item.dot, item.origin]
            end
        end

        def prediction_pass!
            @next_set.medial.each do |ei|
                postdot = if rhs1 = @tables.get_rhs1(ei.dot)
                    rhs1
                else
                    next
                end
                if !@next_set.predicted[postdot.id]
                    @next_set.predicted.or @tables.prediction_matrix[postdot.id]
                end
            end
        end

        def complete! earleme, symbol, node
            if @earley_chart[earleme].predicted[symbol.id]
                complete_medial_items! earleme, symbol, node
                complete_unary_predictions! earleme, symbol, node
                complete_binary_predictions! earleme, symbol, node
            end
        end

        def complete_medial_items! earleme, symbol, right_node
            medial = @earley_chart[earleme].medial
            min_item = medial.bsearch {|ei| @tables.get_rhs1(ei.dot) >= symbol }
            return if min_item.nil?
            start = medial.index min_item
            medial[start..].take_while {|ei| @tables.get_rhs1(ei.dot) == symbol }.each do |item|
                @complete.push CompletedItem.new(item.origin, item.dot, item.node, right_node)
            end
        end

        def complete_unary_predictions! earleme, symbol, node
            @tables.unary_completions(symbol).each do |trans|
                if @earley_chart[earleme].predicted[trans.symbol.id]
                    # No checks for uniqueness, because `medial` will be deduplicated.
                    # from A ::= • B
                    # to   A ::=   B •
                    @complete.push CompletedItem.new(earleme, trans.dot, node, nil)
                end
            end
        end

        def complete_binary_predictions! earleme, symbol, node
            @tables.binary_completions(symbol).each do |trans|
                if @earley_chart[earleme].predicted[trans.symbol.id]
                    # No checks for uniqueness, because `medial` will be deduplicated.
                    # from A ::= • B   C
                    # to   A ::=   B • C
                    # Where C is terminal or nonterminal.
                    @next_set.medial << Item.new(earleme, trans.dot, node)
                end
            end
        end

        def is_exhausted?
            @next_set.medial.empty? && @complete.empty?
        end

        def earleme
            @earley_chart.size - 1
        end
    end

    class PredictionTransition < Struct.new(:symbol, :dot)
    end

    class Tables
        attr_reader :prediction_matrix 
    
        def initialize grammar
            @prediction_matrix = BitMatrix.new grammar.num_syms, grammar.num_syms
            @start_symbol = grammar.start_symbol
            @num_syms = grammar.num_syms
            @rules = grammar.rules
            populate_completions! grammar
            populate_prediction_matrix! grammar
        end

        def populate_completions! grammar
            @binary_completions = Array.new(grammar.num_syms) { [] }
            @unary_completions = Array.new(grammar.num_syms) { [] }
            grammar.rules.each_with_index do |rule, i|
                transition = PredictionTransition.new(rule.lhs, i)
                if rule.rhs1
                    @binary_completions[rule.rhs0.id] ||= []
                    @binary_completions[rule.rhs0.id] << transition
                else
                    @unary_completions[rule.rhs0.id] ||= []
                    @unary_completions[rule.rhs0.id] << transition
                end
            end
        end

        def populate_prediction_matrix! grammar
            grammar.rules.each do |rule|
                @prediction_matrix[rule.lhs.id][rule.rhs0.id] = true
            end
            @prediction_matrix.reflexive_closure!
            @prediction_matrix.transitive_closure!
        end

        def get_rhs0 dot
            @rules[dot] && @rules[dot].rhs0
        end

        def get_rhs1 dot
            @rules[dot] && @rules[dot].rhs1
        end

        def get_rhs1_cmp dot
            MaybePostdot.new get_rhs1 dot
        end

        def get_lhs dot
            @rules[dot].lhs
        end

        def unary_completions symbol
            @unary_completions[symbol.id]
        end

        def binary_completions symbol
            @binary_completions[symbol.id]
        end
    end

    class Forest
        attr_reader :graph

        def initialize binarized_grammar
            @graph = []
            @summands = []
            @eval = binarized_grammar.eval
        end

        def leaf terminal, values
            @graph << Leaf.new(terminal, values)
            NodeHandle.new(@graph.size - 1)
        end

        def push_summand item
            @summands << Product.new(@eval[item.dot], item.left_node, item.right_node)
        end

        def sum sym, origin
            @graph << Sum.new(@summands)
            @summands = []
            NodeHandle.new(@graph.size - 1)
        end
    end

    class Evaluator
        def initialize eval_product:, eval_leaf:
            @eval_product = eval_product
            @eval_leaf = eval_leaf
            @factor_traversal = []
            @factors = []
        end

        def evaluate forest, finished_node
            stack = [finished_node]
            while node_handle = stack.pop
                node = forest.graph[node_handle.index]
                node.alive = true
                case node
                when Sum then
                    node.summands.each do |product|
                        stack << product.left_node
                        stack << product.right_node if product.right_node
                    end
                when Leaf then
                end
            end

            forest.graph.each_with_index do |node, i|
                if node.alive
                    case node
                    when Leaf
                        result = @eval_leaf.call(node.terminal, node.values)
                        forest.graph[i] = Evaluated.new([result])
                    when Sum
                        raise "found #{node.summands.size} summands, expected 1" if node.summands.size != 1

                        values = node.summands.flat_map do |product|
                            if product.action
                                unfold_factors forest.graph, product
                                cartesian_product = @factors[0].product(*@factors[1..-1])
                                cartesian_product.map do |value_ary|
                                    @eval_product.call(product.action, value_ary)
                                end
                            else
                                []
                            end
                        end
                        next if values.empty?
                        forest.graph[i] = Evaluated.new(values)
                    end
                end
            end

            forest.graph[finished_node.index].values
        end

        def unfold_factors graph, product
            @factor_traversal.clear
            @factors.clear
            enqueue_for_unfold graph, product.left_node, product.right_node
            while factor = @factor_traversal.pop
                case factor
                when Sum
                    raise "found #{factor.summands.size} summands, expected 1" if factor.summands.size != 1
                    product = factor.summands.first
                    enqueue_for_unfold graph, product.left_node, product.right_node
                when Evaluated
                    @factors << factor.values
                end
            end
        end

        def enqueue_for_unfold graph, left, right
            @factor_traversal << graph[right.index] if right
            @factor_traversal << graph[left.index]
        end
    end
end
