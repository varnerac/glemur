import gleam/result
import gleam/option.{None, Option, Some}
import glemur/parse/error.{ForbiddenPi, ParserError}
import glemur/parse/util
import glemur/parse/parser
import glemur/parse/acc
import glemur/parse/config.{Config}
import glemur/parse/name
import glemur/parse/char

// Processing Instructions
// [16]  PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
// [17]  PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
pub fn parse_pi(
  conf: Config,
  bs: BitString,
) -> Result(#(BitString, Option(#(String, Option(String)))), ParserError) {
  use Nil <- result.try(config.allow(conf.pi_policy, ForbiddenPi, bs))
  use bs <- result.try(parser.parse_str(bs, "<?"))
  let target_acc = config.acc_from_policy(conf.pi_policy)
  use #(bs, pi_target) <- result.try(name.parse_name(bs, target_acc))
  let inst_acc = config.acc_from_policy(conf.pi_policy)
  use #(bs, maybe_instruction) <- result.try(parse_pi_instruction(bs, inst_acc))
  case pi_target {
    Some(pit) -> Ok(#(bs, Some(#(pit, maybe_instruction))))
    None -> Ok(#(bs, None))
  }
}

pub fn parse_pi_instruction(
  bs: BitString,
  acc: Option(BitString),
) -> Result(#(BitString, Option(String)), ParserError) {
  case bs {
    <<>> | <<"?":utf8>> -> error.ueos(bs)
    <<"?>":utf8, rest:bit_string>> -> {
      case acc {
        None | Some(<<>>) -> Ok(#(rest, None))
        Some(val) -> Ok(#(rest, Some(util.unsafe_to_string(val))))
      }
    }
    <<cp:utf8_codepoint, rest:bit_string>> ->
      case char.is_char(cp) {
        True -> parse_pi_instruction(rest, acc.maybe_append(acc, cp))
        False -> error.invalid_char(bs)
      }
  }
}
