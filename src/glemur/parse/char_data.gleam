import gleam/string
import gleam/int
import gleam/result
import glemur/parse/char
import glemur/parse/util
import glemur/parse/error.{ParserError}

const newline = 10

const carriage_return = 13

const zero = 48

const nine = 57

const lowercase_a = 97

const lowercase_f = 102

const uppercase_a = 65

const uppercase_f = 70

const less_than = 60

const greater_than = 62

const single_quote = 39

const double_quote = 34

const ampersand = 38

// Parses the next value in character data into a codepoint
// e.g. "N" -> UtfCodepoint(78)
//      
pub fn parse_char_data_codepoint(
  bs: BitString,
) -> Result(#(BitString, UtfCodepoint), ParserError) {
  case bs {
    <<cr, lf, rest:binary>> if cr == carriage_return && lf == newline ->
      Ok(#(rest, unsafe_to_cp(newline)))
    <<cr, rest:binary>> if cr == carriage_return ->
      Ok(#(rest, unsafe_to_cp(newline)))
    <<"&quot;":utf8, rest:binary>> -> Ok(#(rest, unsafe_to_cp(double_quote)))
    <<"&apos;":utf8, rest:binary>> -> Ok(#(rest, unsafe_to_cp(single_quote)))
    <<"&lt;":utf8, rest:binary>> -> Ok(#(rest, unsafe_to_cp(less_than)))
    <<"&gt;":utf8, rest:binary>> -> Ok(#(rest, unsafe_to_cp(greater_than)))
    <<"&amp;":utf8, rest:binary>> -> Ok(#(rest, unsafe_to_cp(ampersand)))
    <<"&#":utf8, rest:binary>> -> {
      use #(bs, cp) <- result.try(parse_char_ref(rest))
      Ok(#(bs, cp))
    }
    <<"&":utf8>> | <<>> -> error.ueos(bs)
    <<"&":utf8, _:binary>> | <<"<":utf8, _:binary>> -> error.invalid_char(bs)
    _ -> {
      use #(state, cp) <- result.try(char.parse_char(bs))
      Ok(#(state, cp))
    }
  }
}

fn unsafe_to_cp(int: Int) -> UtfCodepoint {
  let assert Ok(cp) = string.utf_codepoint(int)
  cp
}

fn parse_char_ref(
  bs: BitString,
) -> Result(#(BitString, UtfCodepoint), ParserError) {
  case bs {
    <<"x":utf8, first_digit, rest:binary>> if first_digit >= zero && first_digit <= nine || first_digit >= uppercase_a && first_digit <= uppercase_f || first_digit >= lowercase_a && first_digit <= lowercase_f ->
      parse_char_ref_(rest, True, <<first_digit>>)
    <<first_digit, rest:binary>> if first_digit >= zero && first_digit <= nine ->
      parse_char_ref_(rest, False, <<first_digit>>)
    <<>> -> error.ueos(bs)
    _ -> error.uc(bs)
  }
}

fn parse_char_ref_(bs: BitString, is_hex: Bool, acc: BitString) {
  case bs {
    <<digit, rest:binary>> if digit >= zero && digit <= nine || is_hex && {
      digit >= uppercase_a && digit <= uppercase_f || digit >= lowercase_a && digit <= lowercase_f
    } -> parse_char_ref_(rest, is_hex, <<acc:bit_string, digit>>)
    <<";":utf8, rest:binary>> -> {
      let base = case is_hex {
        True -> 16
        False -> 10
      }
      let assert Ok(codepoint_val) =
        int.base_parse(util.unsafe_to_string(acc), base)
      case char_codepoint(codepoint_val) {
        Ok(cp) -> Ok(#(rest, cp))
        Error(Nil) -> error.invalid_character_reference(bs)
      }
    }
    <<>> -> error.ueos(bs)
    _ -> error.invalid_character_reference(bs)
  }
}

fn char_codepoint(codepoint_val: Int) -> Result(UtfCodepoint, Nil) {
  case string.utf_codepoint(codepoint_val) {
    Ok(cp) ->
      case char.is_char(cp) {
        True -> Ok(cp)
        False -> Error(Nil)
      }
    Error(Nil) -> Error(Nil)
  }
}
