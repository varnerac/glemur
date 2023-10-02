import gleam/option.{Option}
import gleam/string
import glemur/parse/error.{InvalidName, ParserError, UnexpectedEndOfStream}
import glemur/parse/qname
import glemur/parse/util
import glemur/parse/acc

const colon = 58

pub fn parse_name(
  bs: BitString,
  acc: Option(BitString),
) -> Result(#(BitString, Option(String)), ParserError) {
  case bs {
    <<>> -> Error(UnexpectedEndOfStream(""))
    <<"xml":utf8, _:binary>>
    | <<"XML":utf8, _:binary>>
    | <<"Xml":utf8, _:binary>>
    | <<"XMl":utf8, _:binary>>
    | <<"XmL":utf8, _:binary>>
    | <<"xMl":utf8, _:binary>>
    | <<"xmL":utf8, _:binary>>
    | <<"xML":utf8, _:binary>> ->
      bs
      |> util.unsafe_to_string
      |> InvalidName
      |> Error
    <<char:utf8_codepoint, rest:binary>> ->
      case is_name_start_char(char) {
        True -> parse_name_(rest, acc.maybe_append(acc, char))
        False ->
          bs
          |> util.unsafe_to_string
          |> InvalidName
          |> Error
      }
  }
}

fn parse_name_(
  bs: BitString,
  acc: Option(BitString),
) -> Result(#(BitString, Option(String)), ParserError) {
  case bs {
    <<char:utf8_codepoint, rest:binary>> ->
      case is_name_char(char) {
        True ->
          parse_name_(
            rest,
            option.map(acc, fn(a) { <<a:bit_string, char:utf8_codepoint>> }),
          )
        False -> Ok(#(rest, option.map(acc, util.unsafe_to_string(_))))
      }
    _ -> Ok(#(bs, option.map(acc, util.unsafe_to_string(_))))
  }
}

fn is_name_start_char(cp: UtfCodepoint) -> Bool {
  qname.is_qname_start_char(cp) || string.utf_codepoint_to_int(cp) == colon
}

fn is_name_char(cp: UtfCodepoint) -> Bool {
  qname.is_qname_char(cp) || string.utf_codepoint_to_int(cp) == colon
}
