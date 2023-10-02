import gleam/string
import gleam/result
import gleam/string_builder
import gleam/option.{None, Option, Some}
import glemur/parse/char
import glemur/parse/acc
import glemur/parse/util
import glemur/parse/error.{
  ParserError, UnexpectedCharacter, UnexpectedEndOfStream,
}

const underscore = 95

const hyphen = 45

const period = 46

pub type Qname =
  #(Option(String), String)

pub fn parse_qname(bs: BitString) -> Result(#(BitString, Qname), ParserError) {
  use #(bs, first_nc_name) <- result.try(parse_nc_name(bs))
  case bs {
    <<":":utf8, rest:binary>> -> {
      use #(bs, local_part) <- result.try(parse_nc_name(rest))
      Ok(#(bs, #(Some(first_nc_name), local_part)))
    }
    _ -> Ok(#(bs, #(None, first_nc_name)))
  }
}

// NCName	   ::=   	Name - (Char* ':' Char*)	/* An XML Name, minus the ":" */
fn parse_nc_name(bs: BitString) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<>> ->
      bs
      |> util.unsafe_to_string
      |> UnexpectedEndOfStream
      |> Error
    <<char:utf8_codepoint, rest:binary>> -> {
      case is_qname_start_char(char) {
        True -> parse_nc_name_(rest, <<char:utf8_codepoint>>)
        False ->
          bs
          |> util.unsafe_to_string
          |> UnexpectedCharacter
          |> Error
      }
    }
  }
}

fn parse_nc_name_(
  bs: BitString,
  acc: BitString,
) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<":":utf8, _:binary>> -> Ok(#(bs, acc.to_str(acc)))
    <<char:utf8_codepoint, rest:binary>> ->
      case is_qname_char(char) {
        True -> parse_nc_name_(rest, <<acc:bit_string, char:utf8_codepoint>>)
        False -> Ok(#(bs, acc.to_str(acc)))
      }
    _ -> Ok(#(bs, acc.to_str(acc)))
  }
}

pub fn is_qname_char(codepoint: UtfCodepoint) -> Bool {
  let char = string.utf_codepoint_to_int(codepoint)
  is_qname_start_char(codepoint) || char == hyphen || char == period || char.is_digit(
    char,
  ) || char == 0xB7 || { char >= 0x0300 && char <= 0x036F } || {
    char >= 0x203F && char <= 0x2040
  }
}

pub fn is_qname_start_char(cp: UtfCodepoint) -> Bool {
  let char = string.utf_codepoint_to_int(cp)
  char.is_letter(char) || char == underscore || { char >= 0xC0 && char <= 0xD6 } || {
    char >= 0xD8 && char <= 0xF6
  } || { char >= 0xF8 && char <= 0x2FF } || { char >= 0x370 && char <= 0x37D } || {
    char >= 0x37F && char <= 0x1FFF
  } || { char >= 0x200C && char <= 0x200D } || {
    char >= 0x2070 && char <= 0x218F
  } || { char >= 0x2C00 && char <= 0x2FEF } || {
    char >= 0x3001 && char <= 0xD7FF
  } || { char >= 0xF900 && char <= 0xFDCF } || {
    char >= 0xFDF0 && char <= 0xFFFD
  } || { char >= 0x10000 && char <= 0xEFFFF }
}

pub fn to_str(qname: Qname) -> String {
  case qname {
    #(None, local_part) -> local_part
    #(Some(prefix), local_part) ->
      [prefix, ":", local_part]
      |> string_builder.from_strings
      |> string_builder.to_string
  }
}
