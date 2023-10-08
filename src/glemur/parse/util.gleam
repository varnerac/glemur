import gleam/bit_string

@external(erlang, "glemur_erl", "to_str")
pub fn to_str(bs: BitString) -> String {
  let assert Ok(str) = bit_string.to_string(bs)
  str
}
