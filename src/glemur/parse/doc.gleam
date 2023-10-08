import gleam/result
import gleam/list
import gleam/option.{None, Option, Some}
import glemur/parse/prolog
import glemur/parse/el.{Element}
import glemur/parse/misc
import glemur/parse/error.{ParserError}
import glemur/parse/config.{Config}

pub type XmlVersion {
  OneDotZero
}

pub type XmlDocument {
  XmlDocument(
    version: Option(XmlVersion),
    encoding: Option(String),
    standalone: Option(Bool),
    processing_instructions: List(#(String, Option(String))),
    el: Element,
  )
}

pub fn parse_doc(
  conf: Config,
  bs: BitString,
) -> Result(XmlDocument, ParserError) {
  use #(bs, prolog) <- result.try(prolog.parse_prolog(conf, bs))
  use #(bs, el) <- result.try(el.parse_el(conf, bs, None, []))
  use #(bs, trailing_pis) <- result.try(misc.parse_misc(conf, bs))
  use Nil <- result.try(stream_is_empty(bs))
  let pis = list.append(prolog.pis, trailing_pis)
  let #(version, encoding, standalone) = case prolog.xml_decl {
    None -> #(None, None, None)
    Some(decl) -> #(Some(decl.version), decl.encoding, decl.standalone)
  }
  let version = option.map(version, fn(_) { OneDotZero })

  Ok(XmlDocument(
    version: version,
    encoding: encoding,
    standalone: standalone,
    processing_instructions: pis,
    el: el,
  ))
}

fn stream_is_empty(bs: BitString) {
  case bs {
    <<>> -> Ok(Nil)
    _ -> error.uc(bs)
  }
}
