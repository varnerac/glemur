import gleam/result
import gleam/option.{Option}
import glemur/parse/parser
import glemur/parse/ws
import glemur/parse/misc
import glemur/parse/config.{Config}
import glemur/parse/acc
import glemur/parse/error.{ParserError}

const lowercase_a = 97

const lowercase_z = 122

const uppercase_a = 65

const uppercase_z = 90

const zero = 48

const nine = 57

pub type XmlDecl {
  XmlDecl(version: String, encoding: Option(String), standalone: Option(Bool))
}

// XMLDecl? Misc* (doctypedecl Misc*)?
// Misc ::= Comment | PI |  S
pub type Prolog {
  Prolog(xml_decl: Option(XmlDecl), pis: List(#(String, Option(String))))
}

// XMLDecl? Misc* (doctypedecl Misc*)?
pub fn parse_prolog(
  conf: Config,
  bs: BitString,
) -> Result(#(BitString, Prolog), ParserError) {
  use #(bs, opt_xml_decl) <- result.try(parser.optional(bs, parse_xml_decl))
  use #(bs, pis) <- result.try(misc.parse_misc(conf, bs))
  use _ <- result.try(parse_doc_type_decl_and_misc(conf, bs))
  Ok(#(bs, Prolog(xml_decl: opt_xml_decl, pis: pis)))
}

// Document Type Definition
// doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
// markupdecl ::= elementdecl |  AttlistDecl |  EntityDecl |  NotationDecl | PI |  Comment
// TODO: Implement DTD parsing (validation only)
fn parse_doc_type_decl_and_misc(
  _conf: Config,
  bs: BitString,
) -> Result(#(BitString, List(#(String, Option(String)))), ParserError) {
  case bs {
    <<"<!DOCTYPE":utf8, _:binary>> -> error.forbidden_dtd(bs)
    _ -> Ok(#(bs, []))
  }
}

fn parse_xml_decl(bs: BitString) -> Result(#(BitString, XmlDecl), ParserError) {
  use bs <- result.try(parser.parse_str(bs, "<?xml"))
  use bs <- result.try(ws.parse_req_ws(bs))
  use bs <- result.try(parser.parse_str(bs, "version"))
  use bs <- result.try(parser.parse_equals(bs))
  use #(bs, version) <- result.try(parse_version_num(bs))
  use #(bs, enc) <- result.try(parser.optional(bs, parse_encoding_decl))
  use #(bs, standalone) <- result.try(parser.optional(bs, parse_sd_decl))
  let bs = ws.parse_opt_ws(bs)
  use bs <- result.try(parser.parse_str(bs, "?>"))
  Ok(#(bs, XmlDecl(version: version, encoding: enc, standalone: standalone)))
}

// SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
fn parse_sd_decl(bs: BitString) -> Result(#(BitString, Bool), ParserError) {
  use bs <- result.try(ws.parse_req_ws(bs))
  use bs <- result.try(parser.parse_str(bs, "standalone"))
  use bs <- result.try(parser.parse_equals(bs))
  use #(bs, standalone) <- result.try(parse_yes_no(bs))
  Ok(#(bs, standalone))
}

fn parse_yes_no(bs: BitString) -> Result(#(BitString, Bool), ParserError) {
  use #(bs, quote) <- result.try(parser.parse_quote_char(bs))
  let strs = ["yes", "no"]
  use #(bs, yes_or_no) <- result.try(parser.parse_one_of_strs(bs, strs))
  use bs <- result.try(parser.parse_this_char(bs, quote))
  Ok(#(bs, yes_or_no == "yes"))
}

fn parse_version_num(bs: BitString) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<"'1.0'":utf8, rest:bit_string>> | <<"\"1.0\"":utf8, rest:bit_string>> ->
      Ok(#(rest, "1.0"))
    _ -> {
      let versions = ["'1.0'", "\"1.0\""]
      use #(bs, _) <- result.try(parser.parse_one_of_strs(bs, versions))
      Ok(#(bs, "1.0"))
    }
  }
}

// EncodingDecl ::= S 'encoding' Eq ('"' EncName  '"' |  "'" EncName "'" )  
// EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
fn parse_encoding_decl(
  bs: BitString,
) -> Result(#(BitString, String), ParserError) {
  use bs <- result.try(ws.parse_req_ws(bs))
  use bs <- result.try(parser.parse_str(bs, "encoding"))
  use bs <- result.try(parser.parse_equals(bs))
  use #(bs, encoding) <- result.try(parse_enc_name(bs))
  Ok(#(bs, encoding))
}

// EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
fn parse_enc_name(bs: BitString) -> Result(#(BitString, String), ParserError) {
  use #(bs, quote) <- result.try(parser.parse_quote_char(bs))
  parse_enc_name_rest(bs, quote, <<>>)
}

// ([A-Za-z0-9._] | '-')*
fn parse_enc_name_rest(
  bs: BitString,
  quote: Int,
  acc: BitString,
) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<>> -> error.ueos(bs)
    <<char, rest:binary>> if char >= uppercase_a && char <= uppercase_z || char >= lowercase_a && char <= lowercase_z || char >= zero && char <= nine || char == 46 || char == 95 || char == 45 ->
      parse_enc_name_rest(rest, quote, <<acc:bit_string, char>>)
    //single quote
    <<quote_char, rest:binary>> if quote_char == quote ->
      Ok(#(rest, acc.to_str(acc)))
    _ -> error.uc(bs)
  }
}
