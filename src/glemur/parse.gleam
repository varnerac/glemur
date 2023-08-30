import gleam/option.{None, Option, Some}
import gleam/string
import gleam/list
import gleam/int
import gleam/dynamic
import gleam/string_builder
import gleam/result
import gleam/uri.{Uri}
import gleam/bit_string

pub type Qname =
  #(Option(String), String)

pub type ParseError {
  MissingWhitespace(state: State)
  MissingString(state: State, missing: String)
  MissingYesNo(state: State)
  InvalidXmlVersion(state: State)
  MissingQuotationMark(state: State)
  InvalidStandaloneDeclaration(state: State)
  InvalidCharacter(state: State)
  InvalidEncodingName(state: State)
  InvalidName(state: State)
  InvalidElement(state: State)
  InvalidComment(state: State)
  ForbiddenDtd(state: State)
  ForbiddenComment(state: State)
  ForbiddenPI(state: State)
  InvalidCharacterReference(state: State)
  InvalidElementName(state: State)
  InvalidNamespace(state: State)
  InvalidAttributeNamespacePrefix(state: State)
  InvalidElementNamespacePrefix(state: State)
  DuplicateAttributeNames(state: State)
}

pub type EntityPolicy {
  Keep
  Ignore
  Forbid
}

pub type State {
  State(
    entities: List(XmlEntity),
    stream: BitString,
    col: Int,
    line: Int,
    whitespace: Bool,
    validate_ns_uris: Bool,
    multiple_documents: Bool,
    stream_length: Int,
    depth: Int,
    comment_policy: EntityPolicy,
    pi_policy: EntityPolicy,
    dtd_policy: EntityPolicy,
    trim_el_content_char_data: Bool,
    trim_comments: Bool,
  )
}

pub type PI {
  PI(target: String, val: Option(String))
}

pub type XmlVersion {
  OneDotZero
}

pub type XmlEntity {
  XmlDeclEntity(
    version: XmlVersion,
    encoding: Option(String),
    standalone: Option(Bool),
  )
  ElementEntity(Element)
  CommentEntity(String)
  PIEntity(PI)
}

pub fn new_state(str: String) {
  State(
    entities: [],
    stream: bit_string.from_string(str),
    stream_length: 0,
    col: 0,
    line: 0,
    whitespace: False,
    validate_ns_uris: True,
    multiple_documents: False,
    depth: 0,
    comment_policy: Ignore,
    dtd_policy: Ignore,
    pi_policy: Ignore,
    trim_el_content_char_data: True,
    trim_comments: True,
  )
}

pub type Attribute =
  #(Qname, String)

pub type ElementContent {
  PIContent(PI)
  CharDataContent(String)
  ElContent(Element)
  CommentContent(String)
}

pub type Element {
  Element(name: Qname, attrs: List(Attribute), content: List(ElementContent))
}

type NextInEl {
  EndOfTag(State)
  EndOfEmptyTag(State)
  Attr(State)
  Unexpected(State)
}

type ElementState {
  ElementState(
    default_ns: Option(String),
    ns_declarations: List(#(String, String)),
    name: Qname,
    attrs: List(#(Qname, String)),
    content: List(ElementContent),
  )
}

fn parse_str(state: State, str: String) -> Result(State, ParseError) {
  let bs = bit_string.from_string(str)
  let bs_size = bit_string.byte_size(bs)
  case bit_string.slice(from: state.stream, at: 0, take: bs_size) == Ok(bs) {
    True -> {
      let assert Ok(new_stream) =
        bit_string.slice(
          from: state.stream,
          at: bs_size,
          take: bit_string.byte_size(state.stream) - bs_size,
        )
      state
      |> advance(new_stream, bs_size)
      |> Ok
    }

    False -> Error(MissingString(state, str))
  }
}

// [1]  document ::= prolog element Misc*
pub fn parse_doc(
  state: State,
  default_ns: Option(String),
  ns_declarations: List(#(String, String)),
) -> Result(List(XmlEntity), ParseError) {
  use state <- result.try(parse_prolog(state))
  use #(state, el) <- result.try(parse_el(state, default_ns, ns_declarations))
  let state = add_entity(state, ElementEntity(el))
  use state <- result.try(parse_misc(state))
  state.entities
  |> list.reverse
  |> Ok
}

pub fn parse_el(
  state: State,
  default_ns: Option(String),
  ns_declarations: List(#(String, String)),
) -> Result(#(State, Element), ParseError) {
  use state <- result.try(parse_str(state, "<"))
  use #(state, name) <- result.try(parse_qname(state))
  let el_state =
    ElementState(
      name: name,
      default_ns: default_ns,
      ns_declarations: ns_declarations,
      attrs: [],
      content: [],
    )
  parse_attrs(state, el_state)
}

fn parse_attrs(
  state: State,
  el_state: ElementState,
) -> Result(#(State, Element), ParseError) {
  case next_in_el(state) {
    Attr(state) -> parse_attr(state, el_state)
    EndOfTag(state) -> parse_el_content(state, el_state)
    EndOfEmptyTag(state) -> {
      use element <- result.try(make_element(state, el_state))
      Ok(#(state, element))
    }
    Unexpected(state) -> Error(InvalidCharacter(state))
  }
}

fn add_el_content(
  el_state: ElementState,
  content: ElementContent,
) -> ElementState {
  ElementState(..el_state, content: [content, ..el_state.content])
}

// content ::= (element | CharData | Reference | CDSect | PI | Comment)*
fn parse_el_content(
  state: State,
  el_state: ElementState,
) -> Result(#(State, Element), ParseError) {
  case state.stream {
    <<"</":utf8, _:binary>> -> parse_el_end_tag(state, el_state)
    <<"<!--":utf8, _:binary>> -> {
      case parse_comment(state) {
        Ok(#(state, None)) -> parse_el_content(state, el_state)
        Ok(#(state, Some(c))) ->
          parse_el_content(state, add_el_content(el_state, CommentContent(c)))
        Error(e) -> Error(e)
      }
    }
    // Processing Instruction
    <<"<?":utf8, _:binary>> -> {
      use #(state, opt_pi) <- result.try(parse_pi(state))
      let el_content = case opt_pi {
        Some(pi) -> [PIContent(pi), ..el_state.content]
        None -> el_state.content
      }
      parse_el_content(state, ElementState(..el_state, content: el_content))
    }
    <<"<![CDATA[":utf8, _:binary>> -> {
      use #(state, char_data) <- result.try(parse_cdata_section(state))
      let new_el_state = add_char_data(el_state, char_data)
      parse_el_content(state, new_el_state)
    }
    <<"<":utf8, _:binary>> -> {
      use #(state, sub_el) <- result.try(parse_el(
        state,
        el_state.default_ns,
        el_state.ns_declarations,
      ))
      let new_el_state =
        ElementState(
          ..el_state,
          content: [ElContent(sub_el), ..el_state.content],
        )
      parse_el_content(state, new_el_state)
    }
    _ -> {
      case is_next_ws(state.stream) && state.trim_el_content_char_data {
        True ->
          state
          |> parse_req_ws
          |> result.then(parse_el_content(_, el_state))
        False -> {
          use #(state, char_data) <- result.try(parse_char_data(
            state,
            <<>>,
            less_than_ascii_code,
          ))
          let new_el_state = add_char_data(el_state, char_data)
          parse_el_content(state, new_el_state)
        }
      }
    }
  }
}

fn allow(policy: EntityPolicy, error: ParseError) -> Result(Nil, ParseError) {
  case policy {
    Forbid -> Error(error)
    _ -> Ok(Nil)
  }
}

fn acc_from_policy(policy: EntityPolicy) -> Option(BitString) {
  case policy {
    Keep -> Some(<<>>)
    _ -> None
  }
}

fn parse_pi(state: State) -> Result(#(State, Option(PI)), ParseError) {
  case state.stream {
    <<"<?":utf8, rest:binary>> -> {
      let policy = state.pi_policy
      use _ <- result.try(allow(policy, ForbiddenPI(state)))
      let state = advance(state, rest, 2)
      let acc = acc_from_policy(policy)
      use #(state, target) <- result.try(parse_name(state, acc))
      case state.stream, target, policy {
        _, Some("xml"), _ -> Error(InvalidName(state))
        <<0x20, rest:binary>>, _, _
        | <<0x9, rest:binary>>, _, _
        | <<0xD, rest:binary>>, _, _
        | <<0xA, rest:binary>>, _, _ -> {
          state
          |> newline(rest)
          |> parse_opt_ws
          |> parse_pi_(target, acc_from_policy(state.pi_policy))
        }
        <<"?>":utf8, rest:binary>>, Some(t), Keep ->
          Ok(#(advance(state, rest, 2), Some(PI(target: t, val: None))))
        <<"?>":utf8, rest:binary>>, _, Ignore ->
          Ok(#(advance(state, rest, 2), None))
        _, _, _ -> Error(InvalidCharacter(state))
      }
    }
  }
}

fn parse_pi_(
  state: State,
  target: Option(String),
  acc: Option(BitString),
) -> Result(#(State, Option(PI)), ParseError) {
  case state.stream, acc {
    <<"?>":utf8, rest:binary>>, None -> Ok(#(advance(state, rest, 2), None))
    <<"?>":utf8, rest:binary>>, Some(_) -> {
      let t = option.unwrap(target, "")
      let v = option.map(acc, unsafe_to_string)
      let pi = PI(target: t, val: v)
      Ok(#(advance(state, rest, 2), Some(pi)))
    }
    <<char:utf8_codepoint, rest:binary>>, _ ->
      case is_char(char) {
        True -> {
          let state = advance(state, rest, 1)
          parse_pi_(
            state,
            target,
            option.map(acc, acc_append_codepoint(_, char)),
          )
        }
        False -> Error(InvalidCharacter(state))
      }
  }
}

fn add_char_data(el_state: ElementState, char_data: String) -> ElementState {
  let new_content = case el_state.content {
    [CharDataContent(existing_cdata), ..rest] -> [
      CharDataContent(string.append(existing_cdata, char_data)),
      ..rest
    ]
    _ -> [CharDataContent(char_data), ..el_state.content]
  }
  ElementState(..el_state, content: new_content)
}

// Normalizes char data it parses
// Terminator is the the ASCII value of teh character that terminates parsing
// A terminator of "<" would terminate parsing in an element
// A single quote or double quote would stop parsing of an attribute va

const space_ascii_code = 32

const newline_ascii_code = 10

const less_than_ascii_code = 60

const greater_than_ascii_code = 62

const apostrophe_ascii_code = 39

const quote_ascii_code = 34

const ampersand_ascii_code = 38

const lower_a_ascii_code = 97

const lower_z_ascii_code = 122

const upper_a_ascii_code = 65

const upper_z_ascii_code = 90

fn parse_attr_val(state: State) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<quote, rest:binary>> if quote == apostrophe_ascii_code || quote == quote_ascii_code ->
      state
      |> advance(rest, 1)
      |> parse_attr_val_(quote, True, <<>>)
    _ -> Error(InvalidCharacter(state))
  }
}

fn parse_attr_val_(
  state: State,
  quote: Int,
  is_previous_ws: Bool,
  acc: BitString,
) -> Result(#(State, String), ParseError) {
  case state.stream, is_previous_ws {
    <<quote_char, rest:binary>>, _ if quote_char == quote -> {
      let val =
        acc
        |> unsafe_to_string
        |> string.trim_right
      let state = advance(state, rest, 1)
      Ok(#(state, val))
    }
    <<0xD, 0xA, rest:binary>>, True
    | <<0xA, rest:binary>>, True
    | <<0xD, rest:binary>>, True ->
      state
      |> newline(rest)
      |> parse_attr_val_(quote, True, acc)
    <<0x20, rest:binary>>, True | <<0x9, rest:binary>>, True ->
      state
      |> advance(rest, 1)
      |> parse_attr_val_(quote, True, acc)
    <<0xD, 0xA, rest:binary>>, False
    | <<0xA, rest:binary>>, False
    | <<0xD, rest:binary>>, False ->
      state
      |> newline(rest)
      |> parse_attr_val_(quote, True, acc_append(acc, space_ascii_code))
    <<0x20, rest:binary>>, False | <<0x9, rest:binary>>, False ->
      state
      |> advance(rest, 1)
      |> parse_attr_val_(quote, True, acc_append(acc, space_ascii_code))
    <<"&quot;":utf8, rest:binary>>, _ ->
      state
      |> advance(rest, 6)
      |> parse_attr_val_(quote, False, acc_append(acc, quote_ascii_code))
    <<"&apos;":utf8, rest:binary>>, _ ->
      state
      |> advance(rest, 6)
      |> parse_attr_val_(quote, False, acc_append(acc, apostrophe_ascii_code))
    <<"&lt;":utf8, rest:binary>>, _ ->
      state
      |> advance(rest, 4)
      |> parse_attr_val_(quote, False, acc_append(acc, less_than_ascii_code))
    <<"&gt;":utf8, rest:binary>>, _ ->
      state
      |> advance(rest, 4)
      |> parse_attr_val_(quote, False, acc_append(acc, greater_than_ascii_code))
    <<"&amp;":utf8, rest:binary>>, _ ->
      state
      |> advance(rest, 5)
      |> parse_attr_val_(quote, False, acc_append(acc, ampersand_ascii_code))
    <<"&#":utf8, rest:binary>>, _ -> {
      let state = advance(state, rest, 2)
      use #(state, codepoint) <- result.try(parse_char_ref(state))
      parse_attr_val_(state, quote, False, acc_append_codepoint(acc, codepoint))
    }
    <<"&":utf8, _:binary>>, _ | <<"<":utf8, _:binary>>, _ ->
      Error(InvalidCharacter(state))
    <<other:utf8_codepoint, rest:binary>>, _ -> {
      case is_char(other) {
        True ->
          state
          |> advance(rest, 1)
          |> parse_attr_val_(quote, False, acc_append_codepoint(acc, other))
        False -> Error(InvalidCharacter(state))
      }
    }
  }
}

fn parse_char_data(
  state: State,
  acc: BitString,
  terminator: Int,
) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<char, _:binary>> if char == terminator -> {
      let char_data = case state.trim_el_content_char_data {
        True ->
          acc
          |> unsafe_to_string
          |> string.trim_right
        False -> unsafe_to_string(acc)
      }
      Ok(#(state, char_data))
    }

    // To simplify the tasks of applications, the XML processor must behave
    // as if it normalized all line breaks in external parsed entities 
    // (including the document entity) on input, before parsing, by
    // translating both the two-character sequence #xD #xA and any #xD 
    // that is not followed by #xA to a single #xA character.
    <<0xD, 0xA, rest:binary>> | <<0xD, rest:binary>> | <<0xA, rest:binary>> ->
      state
      |> newline(rest)
      |> parse_char_data(acc_append(acc, newline_ascii_code), terminator)
    <<"&quot;":utf8, rest:binary>> ->
      state
      |> advance(rest, 5)
      |> parse_char_data(acc_append(acc, quote_ascii_code), terminator)
    <<"&apos;":utf8, rest:binary>> ->
      state
      |> advance(rest, 5)
      |> parse_char_data(acc_append(acc, apostrophe_ascii_code), terminator)
    <<"&lt;":utf8, rest:binary>> ->
      state
      |> advance(rest, 4)
      |> parse_char_data(acc_append(acc, less_than_ascii_code), terminator)
    <<"&gt;":utf8, rest:binary>> ->
      state
      |> advance(rest, 4)
      |> parse_char_data(acc_append(acc, greater_than_ascii_code), terminator)
    <<"&amp;":utf8, rest:binary>> ->
      state
      |> advance(rest, 5)
      |> parse_char_data(acc_append(acc, ampersand_ascii_code), terminator)
    <<"&#":utf8, maybe_dec_digit, rest:binary>> if maybe_dec_digit >= 48 && maybe_dec_digit <= 57 -> {
      let state = advance(state, <<maybe_dec_digit, rest:bit_string>>, 2)
      use #(state, codepoint) <- result.try(parse_char_ref(state))
      parse_char_data(state, acc_append_codepoint(acc, codepoint), terminator)
    }
    <<"&":utf8, _:binary>> -> Error(InvalidCharacter(state))
    <<other:utf8_codepoint, rest:binary>> ->
      case is_char(other) {
        True ->
          state
          |> advance(rest, 1)
          |> parse_char_data(acc_append_codepoint(acc, other), terminator)
        False -> Error(InvalidCharacter(state))
      }
  }
}

fn parse_char_ref(state: State) -> Result(#(State, UtfCodepoint), ParseError) {
  case state.stream {
    <<"x":utf8, rest:binary>> ->
      parse_char_ref_(advance(state, rest, 1), True, 1, <<>>)
    _ -> parse_char_ref_(state, False, 0, <<>>)
  }
}

fn parse_char_ref_(
  state: State,
  is_hex: Bool,
  parsed_chars: Int,
  acc: BitString,
) {
  case state.stream {
    <<digit, rest:binary>> if digit >= 48 && digit <= 57 || is_hex && {
      digit >= 65 && digit <= 70 || digit >= 97 && digit <= 102
    } ->
      state
      |> advance(rest, 1)
      |> parse_char_ref_(is_hex, parsed_chars + 1, acc_append(acc, digit))
    <<";":utf8, rest:binary>> -> {
      let state = advance(state, rest, 1)
      let base = case is_hex {
        True -> 16
        False -> 10
      }
      let assert Ok(codepoint_val) = int.base_parse(unsafe_to_string(acc), base)
      case char_codepoint(codepoint_val) {
        Ok(cp) -> Ok(#(state, cp))
        Error(Nil) -> Error(InvalidCharacterReference(state))
      }
    }
    _ -> Error(InvalidCharacterReference(state))
  }
}

fn char_codepoint(codepoint_val: Int) -> Result(UtfCodepoint, Nil) {
  case string.utf_codepoint(codepoint_val) {
    Ok(cp) ->
      case is_char(cp) {
        True -> Ok(cp)
        False -> Error(Nil)
      }
    Error(Nil) -> Error(Nil)
  }
}

fn parse_attr(
  state: State,
  el_state: ElementState,
) -> Result(#(State, Element), ParseError) {
  use #(state, attr_name) <- result.try(parse_qname(state))
  use state <- result.try(parse_eq(state))
  let attr_val_rslt = case attr_name {
    #(Some("xml"), "lang") -> parse_lang_id(state)
    _ -> parse_attr_val(state)
  }
  use #(state, val) <- result.try(attr_val_rslt)
  use el_state <- result.try(add_attr(state, el_state, #(attr_name, val)))
  parse_attrs(state, el_state)
}

fn validate_namespace(state: State, ns: String) -> Result(Nil, ParseError) {
  case uri.parse(ns) {
    Ok(Uri(host: Some(_), ..)) -> Ok(Nil)
    _ -> Error(InvalidNamespace(state))
  }
}

fn add_attr(
  state: State,
  el_state: ElementState,
  attr: #(Qname, String),
) -> Result(ElementState, ParseError) {
  let #(#(prefix, local_part), val) = attr
  case prefix, local_part, val {
    None, "xmlns", "" -> Ok(ElementState(..el_state, default_ns: None))
    None, "xmlns", "http://www.w3.org/2000/xmlns/"
    | None, "xmlns", "http://www.w3.org/XML/1998/namespace" ->
      Error(InvalidNamespace(state))
    None, "xmlns", new_default_ns -> {
      use Nil <- result.try(validate_namespace(state, new_default_ns))
      Ok(ElementState(..el_state, default_ns: Some(new_default_ns)))
    }
    Some("xmlns"), "xml", "http://www.w3.org/XML/1998/namespace" -> Ok(el_state)
    Some("xmlns"), _, "http://www.w3.org/XML/1998/namespace"
    | Some("xmlns"), "xml" <> _, _ -> Error(InvalidNamespace(state))
    Some("xmlns"), namespace, "" -> {
      use Nil <- result.try(validate_namespace(state, namespace))
      let new_ns_decls = case
        list.key_pop(el_state.ns_declarations, namespace)
      {
        Ok(#(_, rest)) -> rest
        Error(Nil) -> el_state.ns_declarations
      }
      Ok(ElementState(..el_state, ns_declarations: new_ns_decls))
    }
    Some("xmlns"), nc_name, ns -> {
      let new_ns_declarations =
        el_state.ns_declarations
        |> list.key_set(nc_name, ns)
      Ok(ElementState(..el_state, ns_declarations: new_ns_declarations))
    }
    _, _, _ -> Ok(ElementState(..el_state, attrs: [attr, ..el_state.attrs]))
  }
}

fn next_in_el(state: State) -> NextInEl {
  case state.stream {
    <<">":utf8, rest:binary>> ->
      state
      |> advance(new_stream: rest, by: 1)
      |> EndOfTag
    <<"/>":utf8, rest:binary>> ->
      state
      |> advance(new_stream: rest, by: 2)
      |> EndOfEmptyTag
    _ ->
      case is_next_ws(state.stream) {
        True -> {
          let state = parse_opt_ws(state)
          case state.stream {
            <<">":utf8, rest:binary>> ->
              state
              |> advance(new_stream: rest, by: 1)
              |> EndOfTag
            <<"/>":utf8, rest:binary>> ->
              state
              |> advance(new_stream: rest, by: 2)
              |> EndOfEmptyTag
            _ -> Attr(state)
          }
        }
        False -> Unexpected(state)
      }
  }
}

// Convert an ElementState into an Element
fn make_element(
  state: State,
  el_state: ElementState,
) -> Result(Element, ParseError) {
  let attrs_result =
    el_state.attrs
    |> update_attrs(el_state.ns_declarations, [])
    |> result.replace_error(InvalidAttributeNamespacePrefix(state))
  use new_attrs <- result.try(attrs_result)
  let name_result =
    el_state
    |> update_el_name
    |> result.replace_error(InvalidElementNamespacePrefix(state))
  use new_name <- result.try(name_result)
  let attr_names = list.map(new_attrs, fn(attr: Attribute) { attr.0 })
  let has_duplicate_attr_names =
    list.length(list.unique(attr_names)) != list.length(attr_names)
  case has_duplicate_attr_names {
    True -> Error(DuplicateAttributeNames(state))
    False ->
      Ok(Element(
        name: new_name,
        attrs: new_attrs,
        content: list.reverse(el_state.content),
      ))
  }
}

fn update_el_name(el_state: ElementState) -> Result(Qname, Nil) {
  case el_state.name {
    #(None, local_part) -> Ok(#(el_state.default_ns, local_part))
    #(Some("xml"), local_part) -> Ok(#(Some(xml_ns), local_part))
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

fn qname_to_string(qname: Qname) -> String {
  case qname {
    #(None, local_part) -> local_part
    #(Some(prefix), local_part) ->
      [prefix, ":", local_part]
      |> string_builder.from_strings
      |> string_builder.to_string
  }
}

fn parse_el_end_tag(
  state: State,
  el_state: ElementState,
) -> Result(#(State, Element), ParseError) {
  use state <- result.then(parse_str(state, "</"))
  use state <- result.then(parse_str(state, qname_to_string(el_state.name)))
  let state = parse_opt_ws(state)
  use state <- result.then(parse_str(state, ">"))
  use element <- result.then(make_element(state, el_state))
  Ok(#(state, element))
}

// CDSect ::= CDStart CData CDEnd
// CDStart ::= '<![CDATA['
// CData ::= (Char* - (Char* ']]>' Char*))
// CDEnd ::= ']]>'
fn parse_cdata_section(state: State) -> Result(#(State, String), ParseError) {
  state
  |> parse_str("<![CDATA[")
  |> result.then(parse_cdata_section_(_, <<>>))
}

fn parse_cdata_section_(
  state: State,
  acc: BitString,
) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<"]]>":utf8, rest:binary>> ->
      Ok(#(advance(state, rest, 3), unsafe_to_string(acc)))
    <<0xD, 0xA, rest:binary>> | <<0xD, rest:binary>> | <<0xA, rest:binary>> -> {
      let new_acc = acc_append(acc, newline_ascii_code)
      state
      |> newline(rest)
      |> parse_cdata_section_(new_acc)
    }
    <<char:utf8_codepoint, rest:binary>> ->
      case is_char(char) {
        True ->
          parse_cdata_section_(
            advance(state, rest, 1),
            acc_append_codepoint(acc, char),
          )
        False -> Error(InvalidCharacter(state))
      }
    _ -> Error(InvalidCharacter(state))
  }
}

fn acc_append(acc: BitString, char: Int) -> BitString {
  <<acc:bit_string, char>>
}

fn acc_append_codepoint(acc: BitString, codepoint: UtfCodepoint) -> BitString {
  <<acc:bit_string, codepoint:utf8_codepoint>>
}

fn maybe_add_entity(state: State, entity: Option(XmlEntity)) -> State {
  case entity {
    Some(e) -> State(..state, entities: [e, ..state.entities])
    None -> state
  }
}

fn add_entity(state: State, entity: XmlEntity) -> State {
  State(..state, entities: [entity, ..state.entities])
}

// XMLDecl? Misc* (doctypedecl Misc*)?
fn parse_prolog(state: State) -> Result(State, ParseError) {
  use #(state, opt_xml_decl) <- result.try(parse_opt_xml_decl(state))
  let state = case opt_xml_decl {
    None -> state
    Some(xml_decl) -> State(..state, entities: [xml_decl, ..state.entities])
  }
  state
  |> parse_misc
  |> result.then(parse_doc_type_decl_and_misc)
}

// Document Type Definition
// doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
//  markupdecl ::= elementdecl |  AttlistDecl |  EntityDecl |  NotationDecl | PI |  Comment
fn parse_doc_type_decl_and_misc(state: State) -> Result(State, ParseError) {
  case state.stream {
    <<"<!DOCTYPE":utf8, _:binary>> -> Error(ForbiddenDtd(state))
    _ -> Ok(state)
  }
}

const period = 46

const hyphen = 45

const underscore = 95

const colon = 58

fn parse_name(
  state: State,
  acc: Option(BitString),
) -> Result(#(State, Option(String)), ParseError) {
  case state.stream {
    <<"xml":utf8, _:binary>>
    | <<"XML":utf8, _:binary>>
    | <<"Xml":utf8, _:binary>>
    | <<"XMl":utf8, _:binary>>
    | <<"XmL":utf8, _:binary>>
    | <<"xMl":utf8, _:binary>>
    | <<"xmL":utf8, _:binary>>
    | <<"xML":utf8, _:binary>> -> Error(InvalidName(state))
    <<char:utf8_codepoint, rest:binary>> ->
      case is_name_start_char(char) {
        True ->
          state
          |> advance(rest, 1)
          |> parse_name_(option.map(acc, acc_append_codepoint(_, char)))
        False -> Error(InvalidName(state))
      }
  }
}

fn parse_name_(
  state: State,
  acc: Option(BitString),
) -> Result(#(State, Option(String)), ParseError) {
  case state.stream {
    <<char:utf8_codepoint, rest:binary>> ->
      case is_name_char(char) {
        True ->
          state
          |> advance(rest, 1)
          |> parse_name_(option.map(acc, acc_append_codepoint(_, char)))
        False -> Ok(#(state, option.map(acc, unsafe_to_string(_))))
      }
    _ -> Ok(#(state, option.map(acc, unsafe_to_string(_))))
  }
}

fn is_name_start_char(cp: UtfCodepoint) -> Bool {
  is_qname_start_char(cp) || string.utf_codepoint_to_int(cp) == colon
}

fn is_qname_start_char(cp: UtfCodepoint) -> Bool {
  let char = string.utf_codepoint_to_int(cp)
  is_letter(char) || char == underscore || { char >= 0xC0 && char <= 0xD6 } || {
    char >= 0xD8 && char <= 0xF6
  } || { char >= 0xF8 && char <= 0x2FF } || { char >= 0x370 && char <= 0x37D } || {
    char >= 0x37F && char <= 0x1FFF
  } || { char >= 0x200C && char <= 0x200D } || {
    char >= 0x2070 && char <= 0x218F
  } || { char >= 0x2C00 && char <= 0x2FEF } || {
    char >= 0x3001 && char <= 0xD7FF
  } || { char >= 0xF900 && char <= 0xFDCF } || {
    char >= 0xFDF0 && char <= 0xFFFD
  } || { char >= 0x10000 && char <= 0xEFFFF }
}

fn is_name_char(cp: UtfCodepoint) -> Bool {
  is_qname_char(cp) || string.utf_codepoint_to_int(cp) == colon
}

fn is_qname_char(codepoint: UtfCodepoint) -> Bool {
  let char = string.utf_codepoint_to_int(codepoint)
  is_qname_start_char(codepoint) || char == hyphen || char == period || is_digit(
    char,
  ) || char == 0xB7 || { char >= 0x0300 && char <= 0x036F } || {
    char >= 0x203F && char <= 0x2040
  }
}

// NSAttName	   ::=   	PrefixedAttNam | DefaultAttName
// PrefixedAttName	   ::=   	'xmlns:' NCName	[NSC: Reserved Prefixes and Namespace Names]
// DefaultAttName	   ::=   	'xmlns'
// NCName	   ::=   	Name - (Char* ':' Char*)	/* An XML Name, minus the ":" */
// QName ::= PrefixedName | UnprefixedName
// PrefixedName ::= Prefix ':' LocalPart
// UnprefixedName ::= LocalPart
// Prefix ::= NCName
// LocalPart ::= NCName
fn parse_qname(state: State) -> Result(#(State, Qname), ParseError) {
  use #(state, first_nc_name) <- result.try(parse_nc_name(state))
  case state.stream {
    <<":":utf8, rest:binary>> -> {
      let state = advance(state, rest, 1)
      use #(state, local_part) <- result.try(parse_nc_name(state))
      Ok(#(state, #(Some(first_nc_name), local_part)))
    }
    _ -> Ok(#(state, #(None, first_nc_name)))
  }
}

// NCName	   ::=   	Name - (Char* ':' Char*)	/* An XML Name, minus the ":" */
fn parse_nc_name(state: State) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<char:utf8_codepoint, rest:binary>> -> {
      case is_qname_start_char(char) {
        True ->
          state
          |> advance(rest, 1)
          |> parse_nc_name_(<<char:utf8_codepoint>>)
        False -> Error(InvalidName(state))
      }
    }
  }
}

fn parse_nc_name_(
  state: State,
  acc: BitString,
) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<":":utf8, _:binary>> -> Ok(#(state, unsafe_to_string(acc)))
    <<char:utf8_codepoint, rest:binary>> ->
      case is_qname_char(char) {
        True ->
          state
          |> advance(rest, 1)
          |> parse_nc_name_(<<acc:bit_string, char:utf8_codepoint>>)
        False -> Ok(#(state, unsafe_to_string(acc)))
      }
    _ -> Ok(#(state, unsafe_to_string(acc)))
  }
}

// This is always Misc*
// Misc ::= Comment | PI |  S
// Processing Instructions
// PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
// PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
fn parse_misc(state: State) -> Result(State, ParseError) {
  case state.stream {
    <<"<!---":utf8, rest:binary>> ->
      Error(InvalidCharacter(advance(state, rest, 4)))
    <<"<!--":utf8, _:binary>> -> {
      use #(state, opt_comment) <- result.try(parse_comment(state))
      let comment_ent = option.map(opt_comment, fn(cs) { CommentEntity(cs) })
      state
      |> maybe_add_entity(comment_ent)
      |> parse_misc
    }
    <<"<?xml":utf8, rest:binary>> ->
      Error(InvalidCharacter(advance(state, rest, 2)))
    <<"<?":utf8, _:binary>> -> {
      use #(state, opt_pi) <- result.try(parse_pi(state))
      let pi_ent = option.map(opt_pi, fn(pi) { PIEntity(pi) })
      state
      |> maybe_add_entity(pi_ent)
      |> parse_misc
    }
    <<0x20, _:binary>>
    | <<0x9, _:binary>>
    | <<0xD, _:binary>>
    | <<0xA, _:binary>> ->
      state
      |> parse_opt_ws
      |> parse_misc
    _ -> Ok(state)
  }
}

//#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
fn is_char(codepoint: UtfCodepoint) -> Bool {
  let char = string.utf_codepoint_to_int(codepoint)
  { char >= 0x20 && char <= 0xD7FF } || char == 0x9 || char == 0xA || char == 0xD || {
    char >= 0xE000 && char <= 0xFFFD
  } || { char >= 0x10000 && char <= 0x10FFFF }
}

fn parse_comment(state: State) -> Result(#(State, Option(String)), ParseError) {
  case state.stream, state.comment_policy {
    <<"<!--":utf8, _:binary>>, Forbid -> Error(ForbiddenComment(state))
    <<"<!--":utf8, rest:binary>>, _ -> {
      let state = advance(state, rest, 4)
      let acc = acc_from_policy(state.comment_policy)
      let state = case state.trim_comments {
        True -> parse_opt_ws(state)
        False -> state
      }
      parse_comment_(state, acc)
    }
  }
}

fn parse_comment_(
  state: State,
  acc: Option(BitString),
) -> Result(#(State, Option(String)), ParseError) {
  case state.stream {
    <<"-->":utf8, rest:binary>> -> {
      let state = advance(state, rest, 3)
      let val = option.map(acc, unsafe_to_string)
      let val = case state.trim_comments {
        True -> option.map(val, string.trim_right)
        False -> val
      }
      Ok(#(state, val))
    }
    <<first:utf8_codepoint, rest:binary>> -> {
      let state = advance(state, rest, 1)
      case is_char(first), acc {
        True, None -> parse_comment_(state, acc)
        True, Some(a) ->
          parse_comment_(state, Some(<<a:bit_string, first:utf8_codepoint>>))
        False, _ -> Error(InvalidCharacter(state))
      }
    }
  }
}

// XMLDecl	::=	'<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'XMLDecl
// VersionInfo	::=	S 'version' Eq (' VersionNum ' | " VersionNum ")
// Eq	::=	S? '=' S?
// VersionNum ::= ([a-zA-Z0-9_.:] | '-')+
fn parse_opt_xml_decl(
  state: State,
) -> Result(#(State, Option(XmlEntity)), ParseError) {
  case state.stream {
    <<"<?xml":utf8, rest:binary>> -> {
      let state = advance(state, rest, 5)
      use state <- result.try(parse_req_ws(state))
      use state <- result.try(parse_str(state, "version"))
      use state <- result.try(parse_eq(state))
      use #(state, version) <- result.try(parse_version_num(state))
      use #(state, encoding) <- result.then(parse_opt_encoding_decl(state))
      use #(state, standalone) <- result.try(parse_opt_sd_decl(state))
      let state = parse_opt_ws(state)
      use state <- result.try(parse_str(state, "?>"))
      Ok(#(
        state,
        Some(XmlDeclEntity(
          version: version,
          encoding: encoding,
          standalone: standalone,
        )),
      ))
    }
    _ -> Ok(#(state, None))
  }
}

// SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
fn parse_opt_sd_decl(state: State) -> Result(#(State, Option(Bool)), ParseError) {
  case is_str_after_req_ws(state, "standalone") {
    True -> {
      use state <- result.try(parse_req_ws(state))
      use state <- result.try(parse_str(state, "standalone"))
      use state <- result.try(parse_eq(state))
      use #(state, standalone) <- result.try(parse_yes_no(state))
      Ok(#(state, Some(standalone)))
    }
    False -> Ok(#(state, None))
  }
}

fn is_str_after_req_ws(state: State, str: String) -> Bool {
  is_str_after_ws(state.stream, str, True)
}

fn is_str_after_ws(bs: BitString, str: String, req_ws: Bool) -> Bool {
  case after_ws(bs), req_ws {
    Some(new_bs), _ -> is_str_after_ws(new_bs, str, False)
    None, True -> False
    None, False ->
      bit_string.slice(from: bs, at: 0, take: string.byte_size(str)) == Ok(bit_string.from_string(
        str,
      ))
  }
}

fn parse_eq(state: State) -> Result(State, ParseError) {
  let state = parse_opt_ws(state)
  use state <- result.then(parse_str(state, "="))
  Ok(parse_opt_ws(state))
}

fn parse_yes_no(state: State) -> Result(#(State, Bool), ParseError) {
  case state.stream {
    <<"\"yes\"":utf8, rest:binary>> | <<"'yes'":utf8, rest:binary>> ->
      Ok(#(advance(state, rest, 5), True))
    <<"\"no\"":utf8, rest:binary>> | <<"'no'":utf8, rest:binary>> ->
      Ok(#(advance(state, rest, 3), False))
    _ -> Error(InvalidCharacter(state))
  }
}

// EncodingDecl ::= S 'encoding' Eq ('"' EncName  '"' |  "'" EncName "'" )  
// EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
fn parse_opt_encoding_decl(
  state,
) -> Result(#(State, Option(String)), ParseError) {
  case is_str_after_req_ws(state, "encoding") {
    True -> {
      use state <- result.try(parse_req_ws(state))
      use state <- result.try(parse_str(state, "encoding"))
      use state <- result.try(parse_eq(state))
      use #(state, encoding) <- result.try(parse_enc_name(state))
      Ok(#(state, Some(encoding)))
    }
    False -> Ok(#(state, None))
  }
}

type QuoteType {
  SingleQuote
  DoubleQuote
}

// EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
fn parse_enc_name(state: State) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<"'":utf8, char, rest:binary>> if char >= 65 && char <= 90 || char >= 97 && char <= 122 -> {
      state
      |> advance(rest, 1)
      |> parse_enc_name_rest(SingleQuote, <<>>)
    }
    <<"\"":utf8, char, rest:binary>> if char >= 65 && char <= 90 || char >= 97 && char <= 122 -> {
      state
      |> advance(rest, 1)
      |> parse_enc_name_rest(DoubleQuote, <<>>)
    }
    _ -> Error(InvalidEncodingName(state))
  }
}

// ([A-Za-z0-9._] | '-')*
fn parse_enc_name_rest(
  state: State,
  quote_type: QuoteType,
  acc: BitString,
) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<char, rest:binary>> if char >= 65 && char <= 90 || char >= 97 && char <= 122 || char >= 48 && char <= 57 || char == 46 || char == 95 || char == 45 ->
      state
      |> advance(new_stream: rest, by: 1)
      |> parse_enc_name_rest(quote_type, <<acc:bit_string, char>>)
    //single quote
    <<quote, rest:binary>> if quote == apostrophe_ascii_code && quote_type == SingleQuote || quote == quote_ascii_code && quote_type == DoubleQuote ->
      Ok(#(advance(state, rest, 1), unsafe_to_string(acc)))
    _ -> Error(InvalidEncodingName(state))
  }
}

fn unsafe_to_string(bs: BitString) -> String {
  bs
  |> dynamic.from
  |> dynamic.unsafe_coerce
}

fn newline(state s: State, new_stream ns: BitString) -> State {
  State(..s, stream: ns, col: 0, line: s.line + 1)
}

fn advance(state s: State, new_stream ns: BitString, by b: Int) -> State {
  State(..s, stream: ns, col: s.col + b)
}

fn parse_version_num(state: State) -> Result(#(State, XmlVersion), ParseError) {
  case state.stream {
    <<"'1.0'":utf8, rest:binary>> | <<"\"1.0\"":utf8, rest:binary>> ->
      Ok(#(advance(state, rest, 5), OneDotZero))
    _ -> Error(InvalidXmlVersion(state))
  }
}

fn parse_quote(state: State) -> Result(#(State, Int), ParseError) {
  case state.stream {
    <<"'":utf8, rest:bit_string>> ->
      Ok(#(advance(state, rest, 1), apostrophe_ascii_code))
    <<"\"":utf8, rest:bit_string>> ->
      Ok(#(advance(state, rest, 1), quote_ascii_code))
    _ -> Error(InvalidCharacter(state))
  }
}

// Language Identification
// [33]  LanguageID ::= Langcode ('-' Subcode)*
// [34]  Langcode ::= ISO639Code |  IanaCode |  UserCode
// [35]  ISO639Code ::= ([a-z] | [A-Z]) ([a-z] | [A-Z])
// [36]  IanaCode ::= ('i' | 'I') '-' ([a-z] | [A-Z])+
// [37]  UserCode ::= ('x' | 'X') '-' ([a-z] | [A-Z])+
// [38]  Subcode ::= ([a-z] | [A-Z])+
fn parse_lang_id(state: State) -> Result(#(State, String), ParseError) {
  use #(state, quote_char) <- result.try(parse_quote(state))
  let lang_code_result = case state.stream {
    <<"i-":utf8, _:bit_string>>
    | <<"I-":utf8, _:bit_string>>
    | <<"x-":utf8, _:bit_string>>
    | <<"X-":utf8, _:bit_string>> -> parse_iana_or_user_code(state, quote_char)
    <<char1, char2, maybe_quote, rest:bit_string>> if maybe_quote == quote_char && {
      char1 >= lower_a_ascii_code && char1 <= lower_z_ascii_code || char1 >= upper_a_ascii_code && char1 <= upper_z_ascii_code
    } && {
      char2 >= lower_a_ascii_code && char2 <= lower_z_ascii_code || char2 >= upper_a_ascii_code && char2 <= upper_z_ascii_code
    } -> Ok(#(advance(state, rest, 2), unsafe_to_string(<<char1, char2>>)))
    <<maybe_quote, rest:bit_string>> if maybe_quote == quote_char ->
      Ok(#(advance(state, rest, 1), ""))
    _ -> Error(InvalidCharacter(state))
  }
  use #(state, lang_code) <- result.try(lang_code_result)
  case state.stream {
    <<"-":utf8, rest:bit_string>> -> {
      let subcode_result =
        state
        |> advance(rest, 1)
        |> parse_one_or_more_letters(quote_char, <<"-":utf8>>)
      use #(state, subcode) <- result.try(subcode_result)
      Ok(#(state, lang_code <> "-" <> subcode))
    }
    _ -> Ok(#(state, lang_code))
  }
}

fn parse_iana_or_user_code(
  state: State,
  quote_char: Int,
) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<i_or_x, dash, char3, rest:bit_string>> if char3 >= lower_a_ascii_code && char3 <= lower_z_ascii_code || char3 >= upper_a_ascii_code && char3 <= upper_z_ascii_code ->
      state
      |> advance(rest, 3)
      |> parse_one_or_more_letters(quote_char, <<i_or_x, dash, char3>>)
    <<_i_or_x, _dash, rest:bit_string>> ->
      state
      |> advance(rest, 2)
      |> InvalidCharacter
      |> Error
  }
}

// ([a-z] | [A-Z])+
fn parse_one_or_more_letters(
  state: State,
  quote_char: Int,
  acc: BitString,
) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<letter, rest:bit_string>> if letter >= lower_a_ascii_code && letter <= lower_z_ascii_code || letter >= upper_a_ascii_code && letter <= upper_z_ascii_code ->
      state
      |> advance(rest, 1)
      |> parse_one_or_more_letters_(quote_char, <<acc:bit_string, letter>>)
    _ -> Error(InvalidCharacter(state))
  }
}

// ([a-z] | [A-Z])+
fn parse_one_or_more_letters_(
  state: State,
  quote_char: Int,
  acc: BitString,
) -> Result(#(State, String), ParseError) {
  case state.stream {
    <<letter, rest:bit_string>> if letter >= lower_a_ascii_code && letter <= lower_z_ascii_code || letter >= upper_a_ascii_code && letter <= upper_z_ascii_code ->
      state
      |> advance(rest, 1)
      |> parse_one_or_more_letters_(quote_char, <<acc:bit_string, letter>>)
    <<maybe_quote, rest:bit_string>> if maybe_quote == quote_char ->
      Ok(#(advance(state, rest, 1), unsafe_to_string(acc)))
    _ -> Error(InvalidCharacter(state))
  }
}

fn is_next_ws(bit_string bs: BitString) -> Bool {
  case bs {
    <<0x20, _:binary>>
    | <<0x9, _:binary>>
    | <<0xD, _:binary>>
    | <<0xA, _:binary>> -> True
    _ -> False
  }
}

// S	::=	(#x20 | #x9 | #xD | #xA)+
fn parse_req_ws(state: State) -> Result(State, ParseError) {
  case state.stream {
    //SPACE
    <<0x20, rest:binary>>
    | // Horizontal TAB
    <<0x9, rest:binary>> ->
      state
      |> advance(by: 1, new_stream: rest)
      |> parse_opt_ws
      |> Ok
    //CR/LF
    <<0xD, 0xA, rest:binary>>
    | //CR
    <<0xD, rest:binary>>
    | //LF
    <<0xA, rest:binary>> ->
      state
      |> newline(rest)
      |> parse_opt_ws
      |> Ok
    _ -> Error(MissingWhitespace(state))
  }
}

// S	::=	(#x20 | #x9 | #xD | #xA)+
fn parse_opt_ws(state: State) -> State {
  case state.stream {
    //SPACE
    <<0x20, rest:binary>>
    | // Horizontal TAB
    <<0x9, rest:binary>> ->
      state
      |> advance(by: 1, new_stream: rest)
      |> parse_opt_ws
    //CR/LF
    <<0xD, 0xA, rest:binary>>
    | //CR
    <<0xD, rest:binary>>
    | //LF
    <<0xA, rest:binary>> ->
      state
      |> newline(rest)
      |> parse_opt_ws
    _ -> state
  }
}

fn after_ws(bs: BitString) -> Option(BitString) {
  case bs {
    <<0x20, rest:binary>>
    | <<0x9, rest:binary>>
    | <<0xD, rest:binary>>
    | <<0xA, rest:binary>> -> Some(rest)
    _ -> None
  }
}

const xml_ns = "http://www.w3.org/XML/1998/namespace"

const xmlns_ns = "http://www.w3.org/2000/xmlns/"

pub fn is_ideographic(c: Int) -> Bool {
  c >= 0x4e00 && c <= 0x9fa5 || c == 0x3007 || c >= 0x3021 && c <= 0x3029
}

pub fn is_base_char(c: Int) -> Bool {
  c >= 0x0041 && c <= 0x005A || c >= 0x0061 && c <= 0x007A || c >= 0x00C0 && c <= 0x00D6 || c >= 0x00D8 && c <= 0x00F6 || c >= 0x00F8 && c <= 0x00FF || c >= 0x0100 && c <= 0x0131 || c >= 0x0134 && c <= 0x013E || c >= 0x0141 && c <= 0x0148 || c >= 0x014A && c <= 0x017E || c >= 0x0180 && c <= 0x01C3 || c >= 0x01CD && c <= 0x01F0 || c >= 0x01F4 && c <= 0x01F5 || c >= 0x01FA && c <= 0x0217 || c >= 0x0250 && c <= 0x02A8 || c >= 0x02BB && c <= 0x02C1 || c == 0x0386 || c >= 0x0388 && c <= 0x038A || c == 0x038C || c >= 0x038E && c <= 0x03A1 || c >= 0x03A3 && c <= 0x03CE || c >= 0x03D0 && c <= 0x03D6 || c == 0x03DA || c == 0x03DC || c == 0x03DE || c == 0x03E0 || c >= 0x03E2 && c <= 0x03F3 || c >= 0x0401 && c <= 0x040C || c >= 0x040E && c <= 0x044F || c >= 0x0451 && c <= 0x045C || c >= 0x045E && c <= 0x0481 || c >= 0x0490 && c <= 0x04C4 || c >= 0x04C7 && c <= 0x04C8 || c >= 0x04CB && c <= 0x04CC || c >= 0x04D0 && c <= 0x04EB || c >= 0x04EE && c <= 0x04F5 || c >= 0x04F8 && c <= 0x04F9 || c >= 0x0531 && c <= 0x0556 || c == 0x0559 || c >= 0x0561 && c <= 0x0586 || c >= 0x05D0 && c <= 0x05EA || c >= 0x05F0 && c <= 0x05F2 || c >= 0x0621 && c <= 0x063A || c >= 0x0641 && c <= 0x064A || c >= 0x0671 && c <= 0x06B7 || c >= 0x06BA && c <= 0x06BE || c >= 0x06C0 && c <= 0x06CE || c >= 0x06D0 && c <= 0x06D3 || c == 0x06D5 || c >= 0x06E5 && c <= 0x06E6 || c >= 0x0905 && c <= 0x0939 || c == 0x093D || c >= 0x0958 && c <= 0x0961 || c >= 0x0985 && c <= 0x098C || c >= 0x098F && c <= 0x0990 || c >= 0x0993 && c <= 0x09A8 || c >= 0x09AA && c <= 0x09B0 || c == 0x09B2 || c >= 0x09B6 && c <= 0x09B9 || c >= 0x09DC && c <= 0x09DD || c >= 0x09DF && c <= 0x09E1 || c >= 0x09F0 && c <= 0x09F1 || c >= 0x0A05 && c <= 0x0A0A || c >= 0x0A0F && c <= 0x0A10 || c >= 0x0A13 && c <= 0x0A28 || c >= 0x0A2A && c <= 0x0A30 || c >= 0x0A32 && c <= 0x0A33 || c >= 0x0A35 && c <= 0x0A36 || c >= 0x0A38 && c <= 0x0A39 || c >= 0x0A59 && c <= 0x0A5C || c == 0x0A5E || c >= 0x0A72 && c <= 0x0A74 || c >= 0x0A85 && c <= 0x0A8B || c == 0x0A8D || c >= 0x0A8F && c <= 0x0A91 || c >= 0x0A93 && c <= 0x0AA8 || c >= 0x0AAA && c <= 0x0AB0 || c >= 0x0AB2 && c <= 0x0AB3 || c >= 0x0AB5 && c <= 0x0AB9 || c == 0x0ABD || c == 0x0AE0 || c >= 0x0B05 && c <= 0x0B0C || c >= 0x0B0F && c <= 0x0B10 || c >= 0x0B13 && c <= 0x0B28 || c >= 0x0B2A && c <= 0x0B30 || c >= 0x0B32 && c <= 0x0B33 || c >= 0x0B36 && c <= 0x0B39 || c == 0x0B3D || c >= 0x0B5C && c <= 0x0B5D || c >= 0x0B5F && c <= 0x0B61 || c >= 0x0B85 && c <= 0x0B8A || c >= 0x0B8E && c <= 0x0B90 || c >= 0x0B92 && c <= 0x0B95 || c >= 0x0B99 && c <= 0x0B9A || c == 0x0B9C || c >= 0x0B9E && c <= 0x0B9F || c >= 0x0BA3 && c <= 0x0BA4 || c >= 0x0BA8 && c <= 0x0BAA || c >= 0x0BAE && c <= 0x0BB5 || c >= 0x0BB7 && c <= 0x0BB9 || c >= 0x0C05 && c <= 0x0C0C || c >= 0x0C0E && c <= 0x0C10 || c >= 0x0C12 && c <= 0x0C28 || c >= 0x0C2A && c <= 0x0C33 || c >= 0x0C35 && c <= 0x0C39 || c >= 0x0C60 && c <= 0x0C61 || c >= 0x0C85 && c <= 0x0C8C || c >= 0x0C8E && c <= 0x0C90 || c >= 0x0C92 && c <= 0x0CA8 || c >= 0x0CAA && c <= 0x0CB3 || c >= 0x0CB5 && c <= 0x0CB9 || c == 0x0CDE || c >= 0x0CE0 && c <= 0x0CE1 || c >= 0x0D05 && c <= 0x0D0C || c >= 0x0D0E && c <= 0x0D10 || c >= 0x0D12 && c <= 0x0D28 || c >= 0x0D2A && c <= 0x0D39 || c >= 0x0D60 && c <= 0x0D61 || c >= 0x0E01 && c <= 0x0E2E || c == 0x0E30 || c >= 0x0E32 && c <= 0x0E33 || c >= 0x0E40 && c <= 0x0E45 || c >= 0x0E81 && c <= 0x0E82 || c == 0x0E84 || c >= 0x0E87 && c <= 0x0E88 || c == 0x0E8A || c == 0x0E8D || c >= 0x0E94 && c <= 0x0E97 || c >= 0x0E99 && c <= 0x0E9F || c >= 0x0EA1 && c <= 0x0EA3 || c == 0x0EA5 || c == 0x0EA7 || c >= 0x0EAA && c <= 0x0EAB || c >= 0x0EAD && c <= 0x0EAE || c == 0x0EB0 || c >= 0x0EB2 && c <= 0x0EB3 || c == 0x0EBD || c >= 0x0EC0 && c <= 0x0EC4 || c >= 0x0F40 && c <= 0x0F47 || c >= 0x0F49 && c <= 0x0F69 || c >= 0x10A0 && c <= 0x10C5 || c >= 0x10D0 && c <= 0x10F6 || c == 0x1100 || c >= 0x1102 && c <= 0x1103 || c >= 0x1105 && c <= 0x1107 || c == 0x1109 || c >= 0x110B && c <= 0x110C || c >= 0x110E && c <= 0x1112 || c == 0x113C || c == 0x113E || c == 0x1140 || c == 0x114C || c == 0x114E || c == 0x1150 || c >= 0x1154 && c <= 0x1155 || c == 0x1159 || c >= 0x115F && c <= 0x1161 || c == 0x1163 || c == 0x1165 || c == 0x1167 || c == 0x1169 || c >= 0x116D && c <= 0x116E || c >= 0x1172 && c <= 0x1173 || c == 0x1175 || c == 0x119E || c == 0x11A8 || c == 0x11AB || c >= 0x11AE && c <= 0x11AF || c >= 0x11B7 && c <= 0x11B8 || c == 0x11BA || c >= 0x11BC && c <= 0x11C2 || c == 0x11EB || c == 0x11F0 || c == 0x11F9 || c >= 0x1E00 && c <= 0x1E9B || c >= 0x1EA0 && c <= 0x1EF9 || c >= 0x1F00 && c <= 0x1F15 || c >= 0x1F18 && c <= 0x1F1D || c >= 0x1F20 && c <= 0x1F45 || c >= 0x1F48 && c <= 0x1F4D || c >= 0x1F50 && c <= 0x1F57 || c == 0x1F59 || c == 0x1F5B || c == 0x1F5D || c >= 0x1F5F && c <= 0x1F7D || c >= 0x1F80 && c <= 0x1FB4 || c >= 0x1FB6 && c <= 0x1FBC || c == 0x1FBE || c >= 0x1FC2 && c <= 0x1FC4 || c >= 0x1FC6 && c <= 0x1FCC || c >= 0x1FD0 && c <= 0x1FD3 || c >= 0x1FD6 && c <= 0x1FDB || c >= 0x1FE0 && c <= 0x1FEC || c >= 0x1FF2 && c <= 0x1FF4 || c >= 0x1FF6 && c <= 0x1FFC || c == 0x2126 || c >= 0x212A && c <= 0x212B || c == 0x212E || c >= 0x2180 && c <= 0x2182 || c >= 0x3041 && c <= 0x3094 || c >= 0x30A1 && c <= 0x30FA || c >= 0x3105 && c <= 0x312C || c >= 0xAC00 && c <= 0xD7A3
}

pub fn is_letter(c: Int) -> Bool {
  is_base_char(c) || is_ideographic(c)
}

pub fn is_combining_char(c: Int) -> Bool {
  c >= 0x0300 && c <= 0x0345 || c >= 0x0360 && c <= 0x0361 || c >= 0x0483 && c <= 0x0486 || c >= 0x0591 && c <= 0x05A1 || c >= 0x05A3 && c <= 0x05B9 || c >= 0x05BB && c <= 0x05BD || c == 0x05BF || c >= 0x05C1 && c <= 0x05C2 || c == 0x05C4 || c >= 0x064B && c <= 0x0652 || c == 0x0670 || c >= 0x06D6 && c <= 0x06DC || c >= 0x06DD && c <= 0x06DF || c >= 0x06E0 && c <= 0x06E4 || c >= 0x06E7 && c <= 0x06E8 || c >= 0x06EA && c <= 0x06ED || c >= 0x0901 && c <= 0x0903 || c == 0x093C || c >= 0x093E && c <= 0x094C || c == 0x094D || c >= 0x0951 && c <= 0x0954 || c >= 0x0962 && c <= 0x0963 || c >= 0x0981 && c <= 0x0983 || c == 0x09BC || c == 0x09BE || c == 0x09BF || c >= 0x09C0 && c <= 0x09C4 || c >= 0x09C7 && c <= 0x09C8 || c >= 0x09CB && c <= 0x09CD || c == 0x09D7 || c >= 0x09E2 && c <= 0x09E3 || c == 0x0A02 || c == 0x0A3C || c == 0x0A3E || c == 0x0A3F || c >= 0x0A40 && c <= 0x0A42 || c >= 0x0A47 && c <= 0x0A48 || c >= 0x0A4B && c <= 0x0A4D || c >= 0x0A70 && c <= 0x0A71 || c >= 0x0A81 && c <= 0x0A83 || c == 0x0ABC || c >= 0x0ABE && c <= 0x0AC5 || c >= 0x0AC7 && c <= 0x0AC9 || c >= 0x0ACB && c <= 0x0ACD || c >= 0x0B01 && c <= 0x0B03 || c == 0x0B3C || c >= 0x0B3E && c <= 0x0B43 || c >= 0x0B47 && c <= 0x0B48 || c >= 0x0B4B && c <= 0x0B4D || c >= 0x0B56 && c <= 0x0B57 || c >= 0x0B82 && c <= 0x0B83 || c >= 0x0BBE && c <= 0x0BC2 || c >= 0x0BC6 && c <= 0x0BC8 || c >= 0x0BCA && c <= 0x0BCD || c == 0x0BD7 || c >= 0x0C01 && c <= 0x0C03 || c >= 0x0C3E && c <= 0x0C44 || c >= 0x0C46 && c <= 0x0C48 || c >= 0x0C4A && c <= 0x0C4D || c >= 0x0C55 && c <= 0x0C56 || c >= 0x0C82 && c <= 0x0C83 || c >= 0x0CBE && c <= 0x0CC4 || c >= 0x0CC6 && c <= 0x0CC8 || c >= 0x0CCA && c <= 0x0CCD || c >= 0x0CD5 && c <= 0x0CD6 || c >= 0x0D02 && c <= 0x0D03 || c >= 0x0D3E && c <= 0x0D43 || c >= 0x0D46 && c <= 0x0D48 || c >= 0x0D4A && c <= 0x0D4D || c == 0x0D57 || c == 0x0E31 || c >= 0x0E34 && c <= 0x0E3A || c >= 0x0E47 && c <= 0x0E4E || c == 0x0EB1 || c >= 0x0EB4 && c <= 0x0EB9 || c >= 0x0EBB && c <= 0x0EBC || c >= 0x0EC8 && c <= 0x0ECD || c >= 0x0F18 && c <= 0x0F19 || c == 0x0F35 || c == 0x0F37 || c == 0x0F39 || c == 0x0F3E || c == 0x0F3F || c >= 0x0F71 && c <= 0x0F84 || c >= 0x0F86 && c <= 0x0F8B || c >= 0x0F90 && c <= 0x0F95 || c == 0x0F97 || c >= 0x0F99 && c <= 0x0FAD || c >= 0x0FB1 && c <= 0x0FB7 || c == 0x0FB9 || c >= 0x20D0 && c <= 0x20DC || c == 0x20E1 || c >= 0x302A && c <= 0x302F || c == 0x3099 || c == 0x309A
}

pub fn is_digit(c: Int) -> Bool {
  c >= 0x0030 && c <= 0x0039 || c >= 0x0660 && c <= 0x0669 || c >= 0x06F0 && c <= 0x06F9 || c >= 0x0966 && c <= 0x096F || c >= 0x09E6 && c <= 0x09EF || c >= 0x0A66 && c <= 0x0A6F || c >= 0x0AE6 && c <= 0x0AEF || c >= 0x0B66 && c <= 0x0B6F || c >= 0x0BE7 && c <= 0x0BEF || c >= 0x0C66 && c <= 0x0C6F || c >= 0x0CE6 && c <= 0x0CEF || c >= 0x0D66 && c <= 0x0D6F || c >= 0x0E50 && c <= 0x0E59 || c >= 0x0ED0 && c <= 0x0ED9 || c >= 0x0F20 && c <= 0x0F29
}

pub fn is_extender(c: Int) -> Bool {
  c == 0x00B7 || c == 0x02d0 || c == 0x02d1 || c == 0x0387 || c == 0x0640 || c == 0x0e46 || c == 0x0ec6 || c == 0x3005 || c >= 0x3031 && c <= 0x3035 || c >= 0x309d && c <= 0x309E || c >= 0x30fc && c <= 0x30FE
}
