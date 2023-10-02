import gleam/option.{None, Option, Some}
import glemur/parse/error.{ParserError}
import glemur/parse/util

pub type EntityPolicy {
  Keep
  Ignore
  Forbid
}

pub type Config {
  Config(
    validate_ns_uris: Bool,
    comment_policy: EntityPolicy,
    pi_policy: EntityPolicy,
    dtd_policy: EntityPolicy,
  )
}

pub fn acc_from_policy(policy: EntityPolicy) -> Option(BitString) {
  case policy {
    Keep -> Some(<<>>)
    _ -> None
  }
}

pub fn allow(policy, error, bs) -> Result(Nil, ParserError) {
  case policy {
    Forbid ->
      bs
      |> util.unsafe_to_string
      |> error
      |> Error
    _ -> Ok(Nil)
  }
}
