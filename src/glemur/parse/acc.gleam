import gleam/option.{None, Option, Some}
import gleam/dynamic

pub fn maybe_append(
  acc: Option(BitString),
  codepoint: UtfCodepoint,
) -> Option(BitString) {
  case acc {
    Some(a) -> Some(<<a:bit_string, codepoint:utf8_codepoint>>)
    None -> None
  }
}

pub fn to_str(bs: BitString) -> String {
  bs
  |> dynamic.from
  |> dynamic.unsafe_coerce
}
