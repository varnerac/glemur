import gleam/result
import glemur/parse/char
import glemur/parse/parser
import glemur/parse/ws
import glemur/parse/error.{ForbiddenComment, ParserError}
import glemur/parse/config.{Config}

const hyphen = 45

pub fn parse_comment(
  conf: Config,
  bs: BitString,
) -> Result(BitString, ParserError) {
  use Nil <- result.try(config.allow(conf.comment_policy, ForbiddenComment, bs))
  use bs <- result.try(parser.parse_str(bs, "<!--"))
  // Catches <!---
  use Nil <- result.try(fail_on_hyphen(bs))
  let bs = ws.parse_opt_ws(bs)
  parse_comment_(bs)
}

fn fail_on_hyphen(bs: BitString) -> Result(Nil, ParserError) {
  case bs {
    <<h, _:binary>> if h == hyphen -> error.invalid_comment(bs)
    _ -> Ok(Nil)
  }
}

fn parse_comment_(bs: BitString) -> Result(BitString, ParserError) {
  case bs {
    <<"-->":utf8, rest:binary>> -> {
      Ok(rest)
    }
    <<>> | <<"-->":utf8>> | <<"--":utf8>> | <<"-":utf8>> -> error.ueos(bs)
    <<"--":utf8, _>> -> error.uc(bs)
    _ -> {
      use #(bs, _) <- result.try(char.parse_char(bs))
      parse_comment_(bs)
    }
  }
}
