require 'pry'

class Rule < Struct.new(:lhs, :rhs, :action)
end

Dir['grammars/*.txt'].each do |path|
    p path
    grammar = File.read path
    rules = []

    ops_array = grammar.scan(/'(.+)' -> (\w+)/)
    ops_hash = Hash[ops_array]

    grammar.scan(/(?<lhs>\w+)\s*::=\s*(?<rhs>.+)\n/).each do |lhs, rhs|
        rhs.split('|').map(&:strip).each do |alt|
            rhs_str, code = alt.split(' => ')
            rhs_syms = rhs_str.split(/\s+/)
            rhs_syms.map! do |sym|
                if shorthand = sym[/\A'(.+)'\Z/, 1]
                    raise "unknown shorthand '#{shorthand}'" if ops_hash[shorthand].nil?
                    ops_hash[shorthand]
                else
                    sym
                end
            end
            code = code.strip[/\A\{(.+)\}\Z/, 1] if code
            rules << Rule.new(lhs, rhs_syms, code)
        end
    end

    all_syms = rules.flat_map do |rule|
        [rule.lhs, rule.rhs].flatten
    end
    start_sym = all_syms.first
    all_syms_dedup = all_syms.sort.uniq


    sym_name_assign = all_syms_dedup.join(",\n    ")
    sym_name_arg = all_syms_dedup.join("\n    ")

    rule_stmts = rules.each_with_index.map do |rule, i|
        "grammar.rule lhs: #{rule.lhs}, rhs: [#{rule.rhs.join(", ")}], id: #{i}"
    end.join("\n")

    result_rb = <<-RUBY
require_relative "../earley"

grammar = Earley::Grammar.new
#{sym_name_assign} = grammar.make_n_symbols %w{
    #{sym_name_arg}
}
#{rule_stmts}
grammar.start_symbol = #{start_sym}
p grammar
    RUBY
    File.write "output/#{path[/\/(.*)\.txt\Z/, 1]}.rb", result_rb
end
