import gleam/option.{Option}
import gleam/bit_string
import glemur/parse/el
import glemur/parse/doc.{XmlDocument}
import glemur/parse/error.{ParserError}
import glemur/parse/config.{Config}

pub fn parse_document(
  config: Config,
  str: String,
) -> Result(XmlDocument, ParserError) {
  config
  |> doc.parse_doc(bit_string.from_string(str))
}

pub fn parse_element(
  config: Config,
  str: String,
  default_ns: Option(String),
  ns_declarations: List(#(String, String)),
) {
  el.parse_el(config, bit_string.from_string(str), default_ns, ns_declarations)
}
