import gleam/uri.{Uri}
import gleam/list
import gleam/result
import gleam/string
import gleam/string_builder.{StringBuilder}
import gleam/option.{None, Option, Some}
import glemur/parse/pi
import glemur/parse/parser
import glemur/parse/cdata
import glemur/parse/comment
import glemur/parse/lang
import glemur/parse/error.{
  InvalidAttributeNamespacePrefix, InvalidElementNamespacePrefix, ParserError,
}
import glemur/parse/qname.{Qname}
import glemur/parse/ws
import glemur/parse/config.{Config}
import glemur/parse/char_data
import glemur/parse/util

pub type Element {
  Element(
    name: Qname,
    attrs: List(Attribute),
    pis: List(#(String, Option(String))),
    cdata: String,
    sub_els: List(Element),
  )
}

pub type Attribute =
  #(Qname, String)

type NextInEl {
  EndOfTag(BitString)
  EndOfEmptyTag(BitString)
  Attr(BitString)
  EndOfStream(BitString)
  Unexpected(BitString)
}

type ElementState {
  ElementState(
    default_ns: Option(String),
    ns_declarations: List(#(String, String)),
    pis: List(#(String, Option(String))),
    name: Qname,
    attrs: List(#(Qname, String)),
    cdata: StringBuilder,
    sub_els: List(Element),
  )
}

const xml_ns = "http://www.w3.org/XML/1998/namespace"

const xmlns_ns = "http://www.w3.org/2000/xmlns/"

const space = 32

const newline = 10

const carriage_return = 13

const tab = 9

const less_than = 60

const equals = 61

pub fn parse_el(
  conf: Config,
  bs: BitString,
  default_ns: Option(String),
  ns_declarations: List(#(String, String)),
) -> Result(#(BitString, Element), ParserError) {
  use bs <- result.try(parser.parse_str(bs, "<"))
  use #(bs, name) <- result.try(qname.parse_qname(bs))
  let el_state =
    ElementState(
      name: name,
      default_ns: default_ns,
      ns_declarations: ns_declarations,
      attrs: [],
      pis: [],
      cdata: string_builder.new(),
      sub_els: [],
    )
  parse_attrs(conf, bs, el_state)
}

fn parse_attrs(
  conf: Config,
  bs: BitString,
  el_state: ElementState,
) -> Result(#(BitString, Element), ParserError) {
  case next_in_el(bs) {
    Attr(new_bs) -> parse_attr(conf, new_bs, el_state)
    EndOfTag(new_bs) -> parse_el_content(conf, new_bs, el_state)
    EndOfEmptyTag(new_bs) -> {
      use element <- result.try(make_element(new_bs, el_state))
      Ok(#(new_bs, element))
    }
    Unexpected(new_bs) -> error.uc(new_bs)
    EndOfStream(new_bs) -> error.ueos(new_bs)
  }
}

fn parse_attr(
  conf: Config,
  bs: BitString,
  el_state: ElementState,
) -> Result(#(BitString, Element), ParserError) {
  use #(bs, attr_name) <- result.try(qname.parse_qname(bs))
  use bs <- result.try(parser.parse_this_char(bs, equals))
  let attr_val_rslt = case attr_name {
    #(Some("xml"), "lang") -> lang.parse_lang_id(bs)
    _ -> parse_quoted_attr_val(bs)
  }
  use #(bs, val) <- result.try(attr_val_rslt)
  use el_state <- result.try(add_attr(bs, el_state, #(attr_name, val)))
  parse_attrs(conf, bs, el_state)
}

fn next_in_el(bs: BitString) -> NextInEl {
  case bs {
    <<>> | <<"/":utf8>> -> EndOfStream(bs)
    <<">":utf8, rest:binary>> -> EndOfTag(rest)
    <<"/>":utf8, rest:binary>> -> EndOfEmptyTag(rest)
    _ ->
      case ws.is_next_ws(bs) {
        True -> {
          let bs = ws.parse_opt_ws(bs)
          case bs {
            <<>> | <<"/":utf8>> -> EndOfStream(bs)
            <<">":utf8, rest:binary>> -> EndOfTag(rest)
            <<"/>":utf8, rest:binary>> -> EndOfEmptyTag(rest)
            _ -> Attr(bs)
          }
        }
        False -> Unexpected(bs)
      }
  }
}

// Convert an ElementState into an Element
fn make_element(
  bs: BitString,
  el_state: ElementState,
) -> Result(Element, ParserError) {
  let attrs_result =
    el_state.attrs
    |> update_attrs(el_state.ns_declarations, [])
    |> result.replace_error(InvalidAttributeNamespacePrefix(util.to_str(bs)))
  use new_attrs <- result.try(attrs_result)
  let name_result =
    el_state
    |> update_el_name
    |> result.replace_error(InvalidElementNamespacePrefix(util.to_str(bs)))
  use new_name <- result.try(name_result)
  let attr_names = list.map(new_attrs, fn(attr: Attribute) { attr.0 })
  let has_duplicate_attr_names =
    list.length(list.unique(attr_names)) != list.length(attr_names)
  case has_duplicate_attr_names {
    True -> error.duplicate_attr_names(bs)
    False ->
      Ok(Element(
        name: new_name,
        attrs: new_attrs,
        pis: list.reverse(el_state.pis),
        cdata: string_builder.to_string(el_state.cdata),
        sub_els: el_state.sub_els,
      ))
  }
}

fn update_el_name(el_state: ElementState) -> Result(Qname, Nil) {
  case el_state.name {
    #(None, local_part) -> Ok(#(el_state.default_ns, local_part))
    #(Some("xml"), local_part) -> Ok(#(Some(xml_ns), local_part))
    #(Some("xmlns"), _) -> Error(Nil)
    #(Some(ns_prefix), local_part) ->
      case list.key_find(el_state.ns_declarations, ns_prefix) {
        Ok(ns) -> Ok(#(Some(ns), local_part))
        Error(Nil) -> Error(Nil)
      }
  }
}

fn update_attrs(
  attrs: List(#(Qname, String)),
  ns_declarations: List(#(String, String)),
  acc: List(#(Qname, String)),
) -> Result(List(#(Qname, String)), Nil) {
  case attrs {
    [] -> Ok(acc)
    [#(#(None, _local_part), _val) as attr, ..rest] ->
      update_attrs(rest, ns_declarations, [attr, ..acc])
    [#(#(Some("xml"), local_part), val), ..rest] ->
      update_attrs(
        rest,
        ns_declarations,
        [#(#(Some(xml_ns), local_part), val), ..acc],
      )
    [#(#(Some("xmlns"), local_part), val), ..rest] ->
      update_attrs(
        rest,
        ns_declarations,
        [#(#(Some(xmlns_ns), local_part), val), ..acc],
      )
    [#(#(Some(prefix), local_part), val), ..rest] ->
      case list.key_find(ns_declarations, prefix) {
        Ok(ns) ->
          update_attrs(
            rest,
            ns_declarations,
            [#(#(Some(ns), local_part), val), ..acc],
          )
        Error(Nil) -> Error(Nil)
      }
  }
}

fn parse_quoted_attr_val(bs: BitString) {
  use #(bs, quote_char) <- result.try(parser.parse_quote_char(bs))
  parse_attr_val(bs, quote_char, True, <<>>)
}

fn parse_attr_val(
  bs: BitString,
  quote_char: Int,
  is_previous_ws: Bool,
  acc: BitString,
) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<qc, rest:binary>> if qc == quote_char -> {
      let val =
        acc
        |> util.to_str
        |> string.trim_right
      Ok(#(rest, val))
    }
    <<ws, rest:binary>> if is_previous_ws && {
      ws == space || ws == tab || ws == newline || ws == carriage_return
    } -> parse_attr_val(rest, quote_char, True, acc)
    _ -> {
      use #(bs, cp) <- result.try(char_data.parse_char_data_codepoint(bs))
      let acc = <<acc:bit_string, cp:utf8_codepoint>>
      parse_attr_val(bs, quote_char, False, acc)
    }
  }
}

fn add_attr(
  bs: BitString,
  el_state: ElementState,
  attr: #(Qname, String),
) -> Result(ElementState, ParserError) {
  let #(#(prefix, local_part), val) = attr
  case prefix, local_part, val {
    // The preﬁx xml is by deﬁnition bound to the namespace name
    // http://www.w3.org/XML/1998/namespace. It MAY, but need not, be declared,
    // and MUST NOT be bound to any other namespace name. Other preﬁxes
    // MUST NOT be bound to this namespace name, and it MUST NOT be declared as
    // the default namespace
    // The preﬁx xmlns is used only to declare namespace bindings and is by
    // deﬁnition bound to the namespace name http://www.w3.org/2000/xmlns/.
    // It MUST NOT be declared . Other preﬁxes MUST NOT be bound to this
    // namespace name, and it MUST NOT be declared as the default namespace.
    // Element names MUST NOT have the preﬁx xmlns
    None, "xmlns", "" -> Ok(ElementState(..el_state, default_ns: None))
    Some("xmlns"), "xml", "http://www.w3.org/XML/1998/namespace" -> Ok(el_state)
    None, "xmlns", "http://www.w3.org/2000/xmlns/"
    | None, "xmlns", "http://www.w3.org/XML/1998/namespace" ->
      error.invalid_ns_decl(bs)
    None, "xmlns", new_default_ns -> {
      use Nil <- result.try(validate_namespace(bs, new_default_ns))
      Ok(ElementState(..el_state, default_ns: Some(new_default_ns)))
    }
    Some("xmlns"), _, "http://www.w3.org/XML/1998/namespace"
    | Some("xmlns"), "xml" <> _, _ -> error.invalid_ns_decl(bs)
    Some("xmlns"), namespace, "" -> {
      let new_ns_decls = case
        list.key_pop(el_state.ns_declarations, namespace)
      {
        Ok(#(_, rest)) -> rest
        Error(Nil) -> el_state.ns_declarations
      }
      Ok(ElementState(..el_state, ns_declarations: new_ns_decls))
    }
    Some("xmlns"), nc_name, ns -> {
      use Nil <- result.try(validate_namespace(bs, ns))
      let new_ns_declarations =
        el_state.ns_declarations
        |> list.key_set(nc_name, ns)
      Ok(ElementState(..el_state, ns_declarations: new_ns_declarations))
    }
    _, _, _ -> Ok(ElementState(..el_state, attrs: [attr, ..el_state.attrs]))
  }
}

fn validate_namespace(bs: BitString, ns: String) -> Result(Nil, ParserError) {
  case uri.parse(ns), ns {
    Ok(Uri(host: Some(_), ..)), _ -> Ok(Nil)
    _, "" -> Ok(Nil)
    _, _ -> error.invalid_ns_uri(bs)
  }
}

// content ::= (element | CharData | Reference | CDSect | PI | Comment)*
fn parse_el_content(
  conf: Config,
  bs: BitString,
  el_state: ElementState,
) -> Result(#(BitString, Element), ParserError) {
  case bs {
    <<"<":utf8>> | <<"<!":utf8>> -> error.ueos(bs)
    <<"</":utf8, _:binary>> -> parse_el_end_tag(bs, el_state)
    <<"<!-":utf8, _:binary>> -> {
      use bs <- result.try(comment.parse_comment(conf, bs))
      parse_el_content(conf, bs, el_state)
    }
    <<"<?":utf8, _:binary>> -> {
      use #(bs, opt_pi) <- result.try(pi.parse_pi(conf, bs))
      let new_pis = case opt_pi {
        Some(pi) -> [pi, ..el_state.pis]
        None -> el_state.pis
      }
      parse_el_content(conf, bs, ElementState(..el_state, pis: new_pis))
    }
    <<"<!":utf8, _:binary>> -> {
      use #(bs, char_data) <- result.try(cdata.parse_cdata_section(bs))
      let new_el_state = add_char_data(el_state, char_data)
      parse_el_content(conf, bs, new_el_state)
    }
    <<"<":utf8, _:binary>> -> {
      use #(bs, sub_el) <- result.try(parse_el(
        conf,
        bs,
        el_state.default_ns,
        el_state.ns_declarations,
      ))
      let new_el_state =
        ElementState(..el_state, sub_els: [sub_el, ..el_state.sub_els])
      parse_el_content(conf, bs, new_el_state)
    }
    _ -> {
      case ws.is_next_ws(bs) {
        True ->
          bs
          |> ws.parse_opt_ws
          |> parse_el_content(conf, _, el_state)
        False -> {
          use #(bs, char_data) <- result.try(parse_char_data(bs, <<>>))
          let new_el_state = add_char_data(el_state, char_data)
          parse_el_content(conf, bs, new_el_state)
        }
      }
    }
  }
}

fn parse_char_data(
  bs: BitString,
  acc: BitString,
) -> Result(#(BitString, String), ParserError) {
  case bs {
    <<lt, _:binary>> if lt == less_than -> Ok(#(bs, util.to_str(acc)))
    _ -> {
      use #(bs, cp) <- result.try(char_data.parse_char_data_codepoint(bs))
      parse_char_data(bs, <<acc:bit_string, cp:utf8_codepoint>>)
    }
  }
}

fn parse_el_end_tag(
  bs: BitString,
  el_state: ElementState,
) -> Result(#(BitString, Element), ParserError) {
  use state <- result.try(parser.parse_str(bs, "</"))
  use state <- result.then(parser.parse_str(state, qname.to_str(el_state.name)))
  let state = ws.parse_opt_ws(state)
  use state <- result.try(parser.parse_str(state, ">"))
  use element <- result.try(make_element(state, el_state))
  Ok(#(state, element))
}

fn add_char_data(el_state: ElementState, char_data: String) -> ElementState {
  ElementState(
    ..el_state,
    cdata: string_builder.append(el_state.cdata, char_data),
  )
}
