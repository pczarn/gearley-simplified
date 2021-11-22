# Resources:
#
# • Context free language parsing with Earley Algorithm
#   https://dev.to/jennieji/context-free-language-parsing-with-earley-algorithm-2kp3
#
# • Earley Parsing Explained
#   https://loup-vaillant.fr/tutorials/earley-parsing/

module Earley
    module Containers
        class Heap
            include Enumerable
            
            # call-seq:
            #     size -> int
            #
            # Return the number of elements in the heap.
            def size
              @size
            end
            alias_method :length, :size
            
            # call-seq:
            #     Heap.new(optional_array) { |x, y| optional_comparison_fn } -> new_heap
            #
            # If an optional array is passed, the entries in the array are inserted into the heap with
            # equal key and value fields. Also, an optional block can be passed to define the function
            # that maintains heap property. For example, a min-heap can be created with:
            #
            #     minheap = Heap.new { |x, y| (x <=> y) == -1 }
            #     minheap.push(6)
            #     minheap.push(10)
            #     minheap.pop #=> 6
            #
            # Thus, smaller elements will be parent nodes. The heap defaults to a min-heap if no block
            # is given.
            def initialize(ary=[], &block)
              @compare_fn = block || lambda { |x, y| (x <=> y) == -1 }
              @next = nil
              @size = 0
              @stored = {}
              
              ary.each { |n| push(n) } unless ary.empty?
            end
            
            # call-seq:
            #     push(key, value) -> value
            #     push(value) -> value
            # 
            # Inserts an item with a given key into the heap. If only one parameter is given,
            # the key is set to the value.
            #
            # Complexity: O(1)
            #
            #     heap = MinHeap.new
            #     heap.push(1, "Cat")
            #     heap.push(2)
            #     heap.pop #=> "Cat"
            #     heap.pop #=> 2
            def push(key, value=key)
              raise ArgumentError, "Heap keys must not be nil." unless key
              node = Node.new(key, value)
              # Add new node to the left of the @next node
              if @next
                node.right = @next
                node.left = @next.left
                node.left.right = node
                @next.left = node
                if @compare_fn[key, @next.key]
                  @next = node
                end
              else
                @next = node
              end
              @size += 1
              
              arr = []
              w = @next.right
              until w == @next do
                arr << w.value
                w = w.right
              end
              arr << @next.value
              @stored[key] ||= []
              @stored[key] << node
              value
            end
            alias_method :<<, :push
            
            # call-seq:
            #     has_key?(key) -> true or false
            #
            # Returns true if heap contains the key.
            #
            # Complexity: O(1)
            #
            #     minheap = MinHeap.new([1, 2])
            #     minheap.has_key?(2) #=> true
            #     minheap.has_key?(4) #=> false
            def has_key?(key)
              @stored[key] && !@stored[key].empty? ? true : false
            end
            
            # call-seq:
            #     next -> value
            #     next -> nil
            #
            # Returns the value of the next item in heap order, but does not remove it.
            #
            # Complexity: O(1)
            #
            #     minheap = MinHeap.new([1, 2])
            #     minheap.next #=> 1
            #     minheap.size #=> 2
            def next
              @next && @next.value
            end
            
            # call-seq:
            #     next_key -> key
            #     next_key -> nil
            #
            # Returns the key associated with the next item in heap order, but does not remove the value.
            #
            # Complexity: O(1)
            #
            #     minheap = MinHeap.new
            #     minheap.push(1, :a)
            #     minheap.next_key #=> 1
            #
            def next_key
              @next && @next.key
            end
            
            # call-seq:
            #     clear -> nil
            #
            # Removes all elements from the heap, destructively.
            #
            # Complexity: O(1)
            #
            def clear
              @next = nil
              @size = 0
              @stored = {}
              nil
            end
            
            # call-seq:
            #     empty? -> true or false
            #
            # Returns true if the heap is empty, false otherwise.
            def empty?
              @next.nil?
            end
            
            # call-seq:
            #     merge!(otherheap) -> merged_heap
            #
            # Does a shallow merge of all the nodes in the other heap.
            #
            # Complexity: O(1)
            #
            #     heap = MinHeap.new([5, 6, 7, 8])
            #     otherheap = MinHeap.new([1, 2, 3, 4])
            #     heap.merge!(otherheap)
            #     heap.size #=> 8
            #     heap.pop #=> 1
            def merge!(otherheap)
              raise ArgumentError, "Trying to merge a heap with something not a heap" unless otherheap.kind_of? Containers::Heap
              other_root = otherheap.instance_variable_get("@next")
              if other_root
                @stored = @stored.merge(otherheap.instance_variable_get("@stored")) { |key, a, b| (a << b).flatten }
                # Insert othernode's @next node to the left of current @next
                @next.left.right = other_root
                ol = other_root.left
                other_root.left = @next.left
                ol.right = @next
                @next.left = ol
                
                @next = other_root if @compare_fn[other_root.key, @next.key]
              end
              @size += otherheap.size
            end
            
            # call-seq:
            #     pop -> value
            #     pop -> nil
            #
            # Returns the value of the next item in heap order and removes it from the heap.
            #
            # Complexity: O(1)
            #
            #     minheap = MinHeap.new([1, 2])
            #     minheap.pop #=> 1
            #     minheap.size #=> 1
            def pop
              return nil unless @next
              popped = @next
              if @size == 1
                clear
                return popped.value
              end
              # Merge the popped's children into root node
              if @next.child
                @next.child.parent = nil
                
                # get rid of parent
                sibling = @next.child.right
                until sibling == @next.child
                  sibling.parent = nil
                  sibling = sibling.right
                end
                
                # Merge the children into the root. If @next is the only root node, make its child the @next node
                if @next.right == @next
                  @next = @next.child
                else
                  next_left, next_right = @next.left, @next.right
                  current_child = @next.child
                  @next.right.left = current_child
                  @next.left.right = current_child.right
                  current_child.right.left = next_left
                  current_child.right = next_right
                  @next = @next.right
                end
              else
                @next.left.right = @next.right
                @next.right.left = @next.left
                @next = @next.right
              end
              consolidate
              
              unless @stored[popped.key].delete(popped)
                raise "Couldn't delete node from stored nodes hash" 
              end
              @size -= 1
              
              popped.value
            end
            alias_method :next!, :pop
            
            # call-seq:
            #     change_key(key, new_key) -> [new_key, value]
            #     change_key(key, new_key) -> nil
            #
            # Changes the key from one to another. Doing so must not violate the heap property or
            # an exception will be raised. If the key is found, an array containing the new key and 
            # value pair is returned, otherwise nil is returned. 
            #
            # In the case of duplicate keys, an arbitrary key is changed. This will be investigated
            # more in the future.
            #
            # Complexity: amortized O(1)
            # 
            #     minheap = MinHeap.new([1, 2])
            #     minheap.change_key(2, 3) #=> raise error since we can't increase the value in a min-heap
            #     minheap.change_key(2, 0) #=> [0, 2]
            #     minheap.pop #=> 2
            #     minheap.pop #=> 1
            def change_key(key, new_key, delete=false)
              return if @stored[key].nil? || @stored[key].empty? || (key == new_key)
              
              # Must maintain heap property
              raise "Changing this key would not maintain heap property!" unless (delete || @compare_fn[new_key, key])
              node = @stored[key].shift
              if node
                node.key = new_key
                @stored[new_key] ||= []
                @stored[new_key] << node
                parent = node.parent
                if parent
                  # if heap property is violated
                  if delete || @compare_fn[new_key, parent.key]
                    cut(node, parent)
                    cascading_cut(parent)
                  end
                end
                if delete || @compare_fn[node.key, @next.key]
                  @next = node
                end
                return [node.key, node.value]
              end
              nil
            end
            
            # call-seq:
            #     delete(key) -> value
            #     delete(key) -> nil
            #
            # Deletes the item with associated key and returns it. nil is returned if the key 
            # is not found. In the case of nodes with duplicate keys, an arbitrary one is deleted.
            #
            # Complexity: amortized O(log n)
            #
            #     minheap = MinHeap.new([1, 2])
            #     minheap.delete(1) #=> 1
            #     minheap.size #=> 1
            def delete(key)
              pop if change_key(key, nil, true)
            end
            
            # Node class used internally
            class Node # :nodoc:
              attr_accessor :parent, :child, :left, :right, :key, :value, :degree, :marked
          
              def initialize(key, value)
                @key = key
                @value = value
                @degree = 0
                @marked = false
                @right = self
                @left = self
              end
              
              def marked?
                @marked == true
              end
              
            end
            
            # make node a child of a parent node
            def link_nodes(child, parent)
              # link the child's siblings
              child.left.right = child.right
              child.right.left = child.left
          
              child.parent = parent
              
              # if parent doesn't have children, make new child its only child
              if parent.child.nil?
                parent.child = child.right = child.left = child
              else # otherwise insert new child into parent's children list
                current_child = parent.child
                child.left = current_child
                child.right = current_child.right
                current_child.right.left = child
                current_child.right = child
              end
              parent.degree += 1
              child.marked = false
            end
            private :link_nodes
            
            # Makes sure the structure does not contain nodes in the root list with equal degrees
            def consolidate
              roots = []
              root = @next
              min = root
              # find the nodes in the list
              loop do
                roots << root
                root = root.right
                break if root == @next
              end
              degrees = []
              roots.each do |root|
                min = root if @compare_fn[root.key, min.key]
                # check if we need to merge
                if degrees[root.degree].nil?  # no other node with the same degree
                  degrees[root.degree] = root
                  next
                else  # there is another node with the same degree, consolidate them
                  degree = root.degree
                  until degrees[degree].nil? do
                    other_root_with_degree = degrees[degree]
                    if @compare_fn[root.key, other_root_with_degree.key]  # determine which node is the parent, which one is the child
                      smaller, larger = root, other_root_with_degree
                    else
                      smaller, larger = other_root_with_degree, root
                    end
                    link_nodes(larger, smaller)
                    degrees[degree] = nil
                    root = smaller
                    degree += 1
                  end
                  degrees[degree] = root
                  min = root if min.key == root.key # this fixes a bug with duplicate keys not being in the right order
                end
              end
              @next = min
            end
            private :consolidate
            
            def cascading_cut(node)
              p = node.parent
              if p
                if node.marked?
                  cut(node, p)
                  cascading_cut(p)
                else
                  node.marked = true
                end
              end
            end
            private :cascading_cut
            
            # remove x from y's children and add x to the root list
            def cut(x, y)
              x.left.right = x.right
              x.right.left = x.left
              y.degree -= 1
              if (y.degree == 0)
                y.child = nil
              elsif (y.child == x)
                y.child = x.right
              end
              x.right = @next
              x.left = @next.left
              @next.left = x
              x.left.right = x
              x.parent = nil
              x.marked = false
            end
            private :cut
        end

        # A MaxHeap is a heap where the items are returned in descending order of key value.
        class MaxHeap < Heap
        
            # call-seq:
            #     MaxHeap.new(ary) -> new_heap
            #
            # Creates a new MaxHeap with an optional array parameter of items to insert into the heap.
            # A MaxHeap is created by calling Heap.new { |x, y| (x <=> y) == 1 }, so this is a convenience class.
            #
            #     maxheap = MaxHeap.new([1, 2, 3, 4])
            #     maxheap.pop #=> 4
            #     maxheap.pop #=> 3
            def initialize(ary=[])
                super(ary) { |x, y| (x <=> y) == 1 }
            end
            
            # call-seq:
            #     max -> value
            #     max -> nil
            #
            # Returns the item with the largest key, but does not remove it from the heap.
            #
            #     maxheap = MaxHeap.new([1, 2, 3, 4])
            #     maxheap.max #=> 4
            def max
                self.next
            end
            
            # call-seq:
            #     max! -> value
            #     max! -> nil
            #
            # Returns the item with the largest key and removes it from the heap.
            #
            #     maxheap = MaxHeap.new([1, 2, 3, 4])
            #     maxheap.max! #=> 4
            #     maxheap.size #=> 3
            def max!
                self.pop
            end
        end
    end

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

    class BitVec < Array
        def initialize size
            super(size, true)
        end

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
            populate_completions grammar
        end

        def populate_completions grammar
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
        result
    else
        raise "evaluation failed"
    end
end

p calc '12* 123/-(-5 + 2)'
