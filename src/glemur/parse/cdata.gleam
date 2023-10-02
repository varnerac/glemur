import gleam/result
import glemur/parse/error.{ParserError}
import glemur/parse/acc
import glemur/parse/char
import glemur/parse/parser

// CDSect ::= CDStart CData CDEnd
// CDStart ::= '<![CDATA['
// CData ::= (Char* - (Char* ']]>' Char*))
// CDEnd ::= ']]>'
pub fn parse_cdata_section(
  bs: BitString,
) -> Result(#(BitString, String), ParserError) {
  use bs <- result.try(parser.parse_str(bs, "<![CDATA["))
  parse_cdata_section_(bs, <<>>)
}

fn parse_cdata_section_(
  bs: BitString,
  acc: BitString,
) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<"]]>":utf8, rest:binary>> -> Ok(#(rest, acc.to_str(acc)))
    <<"]]":utf8>> | <<"]":utf8>> | <<>> -> error.ueos(bs)
    _ -> {
      use #(bs, cp) <- result.try(char.parse_char(bs))
      parse_cdata_section_(bs, <<acc:bit_string, cp:utf8_codepoint>>)
    }
  }
}
