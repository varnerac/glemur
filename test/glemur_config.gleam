import glemur/parse/config.{Config, Ignore, Keep}

pub const config1 = Config(
  comment_policy: Ignore,
  pi_policy: Ignore,
  dtd_policy: Ignore,
  validate_ns_uris: True,
)

pub const config2 = Config(
  comment_policy: Ignore,
  pi_policy: Keep,
  dtd_policy: Keep,
  validate_ns_uris: False,
)
