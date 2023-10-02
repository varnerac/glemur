import gleam/dynamic

pub fn unsafe_to_string(bs: BitString) -> String {
  bs
  |> dynamic.from
  |> dynamic.unsafe_coerce
}
