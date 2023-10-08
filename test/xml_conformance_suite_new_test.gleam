import gleam/list
import gleam/string
import gleam/bit_string
import gleam/io
import simplifile
import glemur_config as configs
import glemur_cover
import gleeunit/should
import glemur/parse/config.{Config}
import glemur/parse/error.{UnexpectedEndOfStream}
import glemur/parse/doc

pub type Timeout {
  Timeout
}

pub fn conformance_test_() {
  #(
    Timeout,
    1000,
    fn() {
      glemur_cover.start()
      should.equal(
        Ok(Nil),
        parse(configs.config1, "test/files/valid/", expect: Valid),
      )
      should.equal(
        Ok(Nil),
        parse(configs.config2, "test/files/valid/", expect: Valid),
      )
      should.equal(
        Ok(Nil),
        parse(configs.config1, "test/files/invalid/", expect: Invalid),
      )
      should.equal(
        Ok(Nil),
        parse(configs.config2, "test/files/invalid/", expect: Invalid),
      )
      should.equal(
        Ok(Nil),
        parse(configs.config1, "test/files/ueos/", expect: UEOS),
      )
      should.equal(
        Ok(Nil),
        parse_maybe_unexpected_eos(configs.config1, "test/files/valid/"),
      )
      should.equal(
        Ok(Nil),
        parse_maybe_unexpected_eos(configs.config2, "test/files/valid/"),
      )
      glemur_cover.analyse_to_file()
    },
  )
}

type ExpectedResult {
  Valid
  Invalid
  UEOS
}

fn parse(
  config: Config,
  dir: String,
  expect e: ExpectedResult,
) -> Result(Nil, Nil) {
  dir
  |> dir_files
  |> parse_(config, e)
}

fn dir_files(dir: String) {
  let assert Ok(files) = simplifile.list_contents(dir)
  files
  |> list.map(fn(f) { dir <> f })
}

fn parse_(
  paths: List(String),
  config: Config,
  expect: ExpectedResult,
) -> Result(Nil, Nil) {
  case paths {
    [] -> Ok(Nil)
    [path, ..rest] -> {
      io.println_error("Parsing " <> path <> " for " <> string.inspect(expect))
      let parse_result = parse_file(config, path)
      case expect {
        Valid -> {
          let assert Ok(_) = parse_result
          parse_(rest, config, expect)
        }
        Invalid -> {
          let assert Error(_) = parse_result
          parse_(rest, config, expect)
        }
        UEOS -> {
          let assert Error(UnexpectedEndOfStream(_)) = parse_result
          parse_(rest, config, expect)
        }
      }
    }
  }
}

fn parse_file(config: Config, path: String) {
  let assert Ok(str) = simplifile.read(path)
  doc.parse_doc(config, bit_string.from_string(str))
}

fn parse_string(conf: Config, str: String) {
  conf
  |> doc.parse_doc(bit_string.from_string(str))
}

fn parse_maybe_unexpected_eos(
  config: Config,
  valid_dir: String,
) -> Result(Nil, Nil) {
  let [first, ..rest] = dir_files(valid_dir)
  let assert Ok(str) = simplifile.read(first)
  let cps =
    str
    |> string.to_utf_codepoints
    |> remove_trailing_codepoint
  let _ = parse_maybe_unexpected_eos_(config, cps, rest)
  glemur_cover.analyse_to_file()
}

fn parse_maybe_unexpected_eos_(
  config: Config,
  current: List(UtfCodepoint),
  remaining_paths: List(String),
) {
  case current, remaining_paths {
    [_], [] -> Ok(Nil)
    [_], [next, ..rest] -> {
      io.println_error("maybe unexpected eos parsing: " <> next)
      let assert Ok(str) = simplifile.read(next)
      str
      |> string.to_utf_codepoints
      |> remove_trailing_codepoint
      |> parse_maybe_unexpected_eos_(config, _, rest)
    }
    cps, _ ->
      case parse_string(config, string.from_utf_codepoints(cps)) {
        Ok(_) | Error(UnexpectedEndOfStream(_)) ->
          cps
          |> remove_trailing_codepoint
          |> parse_maybe_unexpected_eos_(config, _, remaining_paths)

        Error(_) -> {
          io.println_error(
            "Didn't receive unexpected end of stream or ok on:\n" <> string.from_utf_codepoints(
              cps,
            ),
          )
          Error(Nil)
        }
      }
  }
}

fn remove_trailing_codepoint(cps: List(UtfCodepoint)) -> List(UtfCodepoint) {
  let [_last_char, ..rest] = list.reverse(cps)
  list.reverse(rest)
}
