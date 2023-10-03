import gleam/option.{Option}
import gleam/result
import gleam/list
import gleam/bit_string
import glemur/parse/util
import glemur/parse/doc
import glemur/parse/el
import glemur/parse/error
import glemur/parse/config.{Config}

pub type ElContent {
  NoContent
  Els(List(Element))
  CharData(String)
}

pub type ParseError {
  InvalidName(String)
  InvalidCharacter(String)
  InvalidCharacterReference(String)
  UnexpectedCharacter(String)
  UnexpectedEndOfStream(String)
  InvalidComment(String)
  ForbiddenComment(String)
  ForbiddenPi(String)
  ForbiddenDtd(String)
  InvalidAttributeNamespacePrefix(String)
  InvalidElementNamespacePrefix(String)
  InvalidElementPrefix(String)
  InvalidNamespaceDecl(String)
  InvalidNamespaceUri(String)
  DuplicateAttributeNames(String)
}

pub type XmlVersion {
  OneDotZero
}

pub type XmlDocument {
  XmlDocument(
    version: Option(XmlVersion),
    encoding: Option(String),
    standalone: Option(Bool),
    pis: List(#(String, Option(String))),
    el: Element,
  )
}

pub type Element {
  Element(
    name: #(Option(String), String),
    attrs: List(#(#(Option(String), String), String)),
    pis: List(#(String, Option(String))),
    content: ElContent,
  )
}

pub fn parse_document(
  config: Config,
  str: String,
) -> Result(XmlDocument, ParseError) {
  config
  |> doc.parse_doc(bit_string.from_string(str))
  |> result.map(fn(d) { convert_document(d) })
  |> result.map_error(fn(e) { convert_error(e) })
}

fn convert_document(doc: doc.XmlDocument) -> XmlDocument {
  let version = option.map(doc.version, fn(_) { OneDotZero })
  XmlDocument(
    version: version,
    encoding: doc.encoding,
    standalone: doc.standalone,
    pis: doc.processing_instructions,
    el: convert_element(doc.el),
  )
}

pub fn parse_element(
  config: Config,
  str: String,
  default_ns: Option(String),
  ns_declarations: List(#(String, String)),
) -> Result(#(Element, String), ParseError) {
  let el_rslt =
    el.parse_el(
      config,
      bit_string.from_string(str),
      default_ns,
      ns_declarations,
    )
    |> result.map_error(convert_error)
    |> result.map(fn(t) { #(t.0, convert_element(t.1)) })
  use #(bs, el) <- result.try(el_rslt)
  Ok(#(el, util.unsafe_to_string(bs)))
}

fn convert_element(element: el.Element) -> Element {
  case element {
    el.Element(sub_els: [_, ..], ..) -> {
      let new_sub_els = list.map(element.sub_els, convert_element)
      Element(
        name: element.name,
        pis: element.pis,
        attrs: element.attrs,
        content: Els(new_sub_els),
      )
    }
    el.Element(cdata: "", sub_els: [], ..) ->
      Element(
        name: element.name,
        pis: element.pis,
        attrs: element.attrs,
        content: NoContent,
      )
    _ ->
      Element(
        name: element.name,
        pis: element.pis,
        attrs: element.attrs,
        content: CharData(element.cdata),
      )
  }
}

fn convert_error(err: error.ParserError) -> ParseError {
  case err {
    error.InvalidName(str) -> InvalidName(str)
    error.InvalidCharacter(str) -> InvalidCharacter(str)
    error.InvalidCharacterReference(str) -> InvalidCharacterReference(str)
    error.UnexpectedCharacter(str) -> UnexpectedCharacter(str)
    error.UnexpectedEndOfStream(str) -> UnexpectedEndOfStream(str)
    error.InvalidComment(str) -> InvalidComment(str)
    error.ForbiddenComment(str) -> ForbiddenComment(str)
    error.ForbiddenPi(str) -> ForbiddenPi(str)
    error.ForbiddenDtd(str) -> ForbiddenDtd(str)
    error.InvalidAttributeNamespacePrefix(str) ->
      InvalidAttributeNamespacePrefix(str)
    error.InvalidElementNamespacePrefix(str) ->
      InvalidElementNamespacePrefix(str)
    error.InvalidElementPrefix(str) -> InvalidElementPrefix(str)
    error.InvalidNamespaceDecl(str) -> InvalidNamespaceDecl(str)
    error.InvalidNamespaceUri(str) -> InvalidNamespaceUri(str)
    error.DuplicateAttributeNames(str) -> DuplicateAttributeNames(str)
  }
}
