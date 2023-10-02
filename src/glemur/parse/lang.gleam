import gleam/result
import gleam/bit_string
import glemur/parse/error.{ParserError}
import glemur/parse/acc
import glemur/parse/parser

const lowercase_a = 97

const lowercase_z = 122

const uppercase_a = 65

const uppercase_z = 90

const hyphen = 45

// Language Identification
// [33]  LanguageID ::= Langcode ('-' Subcode)*
// [34]  Langcode ::= ISO639Code |  IanaCode |  UserCode
// We expand ISO639Code to take a three-letter subcode
// as described in Part 2: ISO 639-2:1998 
// [35]  ISO639Code ::= ([a-z] | [A-Z]) ([a-z] | [A-Z])
// [36]  IanaCode ::= ('i' | 'I') '-' ([a-z] | [A-Z])+
// [37]  UserCode ::= ('x' | 'X') '-' ([a-z] | [A-Z])+
// [38]  Subcode ::= ([a-z] | [A-Z])+
pub fn parse_lang_id(bs: BitString) -> Result(#(BitString, String), ParserError) {
  use #(bs, quote) <- result.try(parser.parse_quote_char(bs))
  use #(bs, lang_id) <- result.try(parser.one_of(
    bs,
    [parse_iso_639_code, parse_iana_or_user_code, parser.parse_empty_str],
  ))
  use bs <- result.try(parser.parse_this_char(bs, quote))
  Ok(#(bs, lang_id))
}

fn parse_iso_639_code(bs: BitString) {
  case bs {
    <<first_letter, second_letter, third_letter, rest:bit_string>> if {
      first_letter >= uppercase_a && first_letter <= uppercase_z || first_letter >= lowercase_a && first_letter <= lowercase_z
    } && {
      second_letter >= uppercase_a && second_letter <= uppercase_z || second_letter >= lowercase_a && second_letter <= lowercase_z
    } && {
      third_letter >= uppercase_a && third_letter <= uppercase_z || third_letter >= lowercase_a && third_letter <= lowercase_z
    } -> parse_subcodes(rest, <<first_letter, second_letter, third_letter>>)
    <<first_letter, second_letter, rest:bit_string>> if {
      first_letter >= uppercase_a && first_letter <= uppercase_z || first_letter >= lowercase_a && first_letter <= lowercase_z
    } && {
      second_letter >= uppercase_a && second_letter <= uppercase_z || second_letter >= lowercase_a && second_letter <= lowercase_z
    } -> parse_subcodes(rest, <<first_letter, second_letter>>)

    <<>> -> error.ueos(bs)
    <<letter>> if letter >= uppercase_a && letter <= uppercase_z || letter >= lowercase_a && letter <= lowercase_z ->
      error.ueos(bs)
    <<first_letter, second_letter>> if {
      first_letter >= uppercase_a && first_letter <= uppercase_z || first_letter >= lowercase_a && first_letter <= lowercase_z
    } && {
      second_letter >= uppercase_a && second_letter <= uppercase_z || second_letter >= lowercase_a && second_letter <= lowercase_z
    } -> error.ueos(bs)
    _ -> error.invalid_char(bs)
  }
}

fn parse_iana_or_user_code(bs: BitString) {
  use #(bs, prefix) <- result.try(parser.parse_one_of_strs(
    bs,
    ["i-", "I-", "x-", "X-"],
  ))
  use #(bs, code) <- result.try(parser.parse_one_or_more_letters(bs))
  parse_subcodes(bs, bit_string.from_string(prefix <> code))
}

fn parse_subcodes(bs: BitString, acc: BitString) {
  case bs {
    <<hyphen_char>> if hyphen_char == hyphen -> error.ueos(bs)
    <<hyphen_char, letter, rest:bit_string>> if hyphen_char == hyphen && {
      letter >= uppercase_a && letter <= uppercase_z || letter >= lowercase_a && letter <= lowercase_z
    } -> parse_subcodes_(rest, <<acc:bit_string, hyphen_char, letter>>)
    _ -> Ok(#(bs, acc.to_str(acc)))
  }
}

fn parse_subcodes_(bs: BitString, acc: BitString) {
  case bs {
    <<letter, rest:bit_string>> if letter >= uppercase_a && letter <= uppercase_z || letter >= lowercase_a && letter <= lowercase_z ->
      parse_subcodes_(rest, <<acc:bit_string, letter>>)
    <<>> -> error.ueos(bs)
    _ -> parse_subcodes(bs, acc)
  }
}
