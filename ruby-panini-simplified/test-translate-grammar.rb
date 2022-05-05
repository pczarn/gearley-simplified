require_relative "generator"
require 'minitest/autorun'
 
class TestRsGenerator < Minitest::Test
    def test_lexer
        rs_generator = RsGenerator.new(<<-GRAMMAR)
            # for example, 
            /./ -> dot
            /[0-9a-zA-Z_]/ -> alnum
            /[^0-9]/ -> not_digit
            '\'' -> quote
        GRAMMAR

        actual = rs_generator.lexer_klasses
        expected = [
            RsGenerator::InputKlass.new([
                '\'' .. '\'',
            ], false),
            RsGenerator::InputKlass.new(["\u0000" .. "\u{10FFFF}"], false),
            RsGenerator::InputKlass.new([
                '0' .. '9',
                'a' .. 'z',
                'A' .. 'Z',
                '_' .. '_',   
            ], false),
            RsGenerator::InputKlass.new([
                '0' .. '9',
            ], true),
        ]
        assert_equal(actual, expected)

        actual = rs_generator.lexer_input_cases
        expected = [
            RsGenerator::InputMatcher.new("match_class0", [
                "'\\''",
            ], false),
            RsGenerator::AnyInputMatcher.new("match_class1"),
            RsGenerator::InputMatcher.new("match_class2", [
                "'0' ..= '9'",
                "'A' ..= 'Z'",
                "'_'",
                "'a' ..= 'z'",
            ], false),
            RsGenerator::InputMatcher.new("match_class3", [
                "'0' ..= '9'",
            ], true),
        ]
        assert_equal(actual, expected)
    end
end
