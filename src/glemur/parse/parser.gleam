import gleam/bit_string
import gleam/string
import gleam/result
import gleam/option.{None, Option, Some}
import glemur/parse/ws
import glemur/parse/acc
import glemur/parse/error.{
  ParserError, UnexpectedCharacter, UnexpectedEndOfStream,
}
import glemur/parse/util

type Parser(any) =
  fn(BitString) -> Result(#(BitString, any), ParserError)

const lowercase_a = 97

const lowercase_z = 122

const uppercase_a = 65

const uppercase_z = 90

const equals = 61

// ([a-z] | [A-Z])+
pub fn parse_one_or_more_letters(
  bs: BitString,
) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<letter, rest:bit_string>> if letter >= lowercase_a && letter <= lowercase_z || letter >= uppercase_a && letter <= uppercase_z ->
      parse_one_or_more_letters_(rest, <<letter>>)
    <<>> -> error.ueos(bs)
    _ -> error.uc(bs)
  }
}

fn parse_one_or_more_letters_(
  bs: BitString,
  acc: BitString,
) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<>> -> error.ueos(bs)
    <<letter, rest:bit_string>> if letter >= lowercase_a && letter <= lowercase_z || letter >= uppercase_a && letter <= uppercase_z ->
      parse_one_or_more_letters_(rest, <<acc:bit_string, letter>>)
    _ -> Ok(#(bs, acc.to_str(acc)))
  }
}

pub fn parse_str(bs: BitString, str: String) -> Result(BitString, ParserError) {
  let str_bs = bit_string.from_string(str)
  let bs_size = bit_string.byte_size(str_bs)
  case bit_string.slice(from: bs, at: 0, take: bs_size) == Ok(str_bs) {
    True -> {
      let assert Ok(new_bs) =
        bit_string.slice(
          from: bs,
          at: bs_size,
          take: bit_string.byte_size(bs) - bs_size,
        )
      Ok(new_bs)
    }
    False ->
      case bs == <<>> || string.starts_with(str, util.unsafe_to_string(bs)) {
        True -> Error(UnexpectedEndOfStream(util.unsafe_to_string(bs)))
        False -> Error(UnexpectedCharacter(util.unsafe_to_string(bs)))
      }
  }
}

const single_quote = 39

const double_quote = 34

pub fn parse_quote_char(bs: BitString) -> Result(#(BitString, Int), ParserError) {
  case bs {
    <<quote, rest:bit_string>> if quote == double_quote || quote == single_quote ->
      Ok(#(rest, quote))
    <<>> -> error.ueos(bs)
    _ -> error.uc(bs)
  }
}

pub fn parse_this_char(
  bs: BitString,
  char: Int,
) -> Result(BitString, ParserError) {
  case bs {
    <<a_char, rest:bit_string>> if a_char == char -> Ok(rest)
    <<>> -> error.ueos(bs)
    _ -> error.uc(bs)
  }
}

pub fn optional(
  bs: BitString,
  parser: fn(BitString) -> Result(#(BitString, any), ParserError),
) -> Result(#(BitString, Option(any)), ParserError) {
  case parser(bs) {
    Error(UnexpectedEndOfStream(v)) -> Error(UnexpectedEndOfStream(v))
    Error(_) -> Ok(#(bs, None))
    Ok(#(new_bs, val)) -> Ok(#(new_bs, Some(val)))
  }
}

pub fn one_of(bs: BitString, parsers: List(Parser(any))) {
  case parsers {
    [p] -> p(bs)
    [p, ..rest] ->
      case p(bs) {
        Ok(r) -> Ok(r)
        Error(UnexpectedEndOfStream(s)) -> Error(UnexpectedEndOfStream(s))
        _ -> one_of(bs, rest)
      }
  }
}

pub fn parse_ws_and(
  bit_string bs: BitString,
  then parser: Parser(any),
) -> Result(#(BitString, any), ParserError) {
  use bs <- result.try(ws.parse_req_ws(bs))
  parser(bs)
}

pub fn parse_equals(bs: BitString) -> Result(BitString, ParserError) {
  let bs = ws.parse_opt_ws(bs)
  use bs <- result.try(parse_this_char(bs, equals))
  bs
  |> ws.parse_opt_ws
  |> Ok
}

pub fn parse_one_of_strs(
  bs: BitString,
  strs: List(String),
) -> Result(#(BitString, String), ParserError) {
  case strs {
    [str] ->
      case parse_str(bs, str) {
        Ok(new_bs) -> Ok(#(new_bs, str))
        Error(e) -> Error(e)
      }
    [str, ..rest] ->
      case parse_str(bs, str) {
        Ok(new_bs) -> Ok(#(new_bs, str))
        Error(UnexpectedEndOfStream(str)) -> Error(UnexpectedEndOfStream(str))
        Error(_) -> parse_one_of_strs(bs, rest)
      }
  }
}

pub fn parse_empty_str(
  bs: BitString,
) -> Result(#(BitString, String), ParserError) {
  Ok(#(bs, ""))
}
