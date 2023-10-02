import glemur/parse/error.{ParserError}

const space = 32

const newline = 10

const carriage_return = 13

const tab = 9

pub fn parse_req_ws(bs: BitString) -> Result(BitString, ParserError) {
  case bs {
    <<>> -> error.ueos(bs)
    <<ws, rest:binary>> if ws == tab || ws == space || ws == newline || ws == carriage_return ->
      Ok(parse_opt_ws(rest))
    _ -> error.uc(bs)
  }
}

pub fn parse_opt_ws(bs: BitString) -> BitString {
  case bs {
    <<ws, rest:binary>> if ws == tab || ws == space || ws == newline || ws == carriage_return ->
      parse_opt_ws(rest)
    _ -> bs
  }
}

pub fn is_next_ws(bs: BitString) -> Bool {
  case bs {
    <<ws, _:binary>> if ws == space || ws == newline || ws == tab || ws == carriage_return ->
      True
    _ -> False
  }
}
