import gleam/option.{None, Option, Some}
import gleam/result
import gleam/list
import glemur/parse/config.{Config}
import glemur/parse/comment
import glemur/parse/pi
import glemur/parse/error.{ParserError}

const space = 32

const newline = 10

const carriage_return = 13

const tab = 9

// This is always Misc*
// Misc ::= Comment | PI |  S
// Processing Instructions
// PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
// PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
pub fn parse_misc(
  conf: Config,
  bs: BitString,
) -> Result(#(BitString, List(#(String, Option(String)))), ParserError) {
  parse_misc_(conf, bs, [])
}

fn parse_misc_(
  conf: Config,
  bs: BitString,
  pis: List(#(String, Option(String))),
) -> Result(#(BitString, List(#(String, Option(String)))), ParserError) {
  case bs {
    <<"<!-":utf8>> | <<"<!":utf8>> | <<"<":utf8>> -> error.ueos(bs)
    <<"<!--":utf8, _:binary>> -> {
      use bs <- result.try(comment.parse_comment(conf, bs))
      parse_misc_(conf, bs, pis)
    }
    <<"<?":utf8, _:binary>> -> {
      use #(bs, opt_pi) <- result.try(pi.parse_pi(conf, bs))
      case opt_pi {
        Some(pi) -> parse_misc_(conf, bs, [pi, ..pis])
        None -> parse_misc_(conf, bs, pis)
      }
    }
    <<ws, rest:binary>> if ws == space || ws == newline || ws == carriage_return || ws == tab ->
      parse_misc_(conf, rest, pis)
    _ -> Ok(#(bs, list.reverse(pis)))
  }
}
