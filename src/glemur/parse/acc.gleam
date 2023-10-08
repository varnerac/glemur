import gleam/option.{None, Option, Some}

pub fn maybe_append(
  acc: Option(BitString),
  codepoint: UtfCodepoint,
) -> Option(BitString) {
  case acc {
    Some(a) -> Some(<<a:bit_string, codepoint:utf8_codepoint>>)
    None -> None
  }
}
