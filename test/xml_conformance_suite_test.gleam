import gleam/erlang/file
import gleam/list
import gleam/io
import gleam/option.{None}
import glemur/parse
import gleeunit/should

pub fn conformance_test() {
  should.equal(Ok(Nil), parse("test/files/valid/", expect_valid: True))
  should.equal(Ok(Nil), parse("test/files/invalid/", expect_valid: False))
}

fn parse(dir: String, expect_valid ev: Bool) -> Result(Nil, Nil) {
  let assert Ok(files) = file.list_directory(dir)
  files
  |> list.map(fn(f) { dir <> f })
  |> parse_(ev)
}

fn parse_(paths: List(String), expect_valid: Bool) -> Result(Nil, Nil) {
  case paths {
    [] -> Ok(Nil)
    [path, ..rest] -> {
      io.println_error("Parsing " <> path)
      let assert Ok(str) = file.read(path)
      let state = parse.new_state(str)
      let parse_result = parse.parse_doc(state, None, [])
      case expect_valid {
        True -> {
          let assert Ok(_) = parse_result
          parse_(rest, expect_valid)
        }
        False -> {
          let assert Error(_) = parse_result
          parse_(rest, expect_valid)
        }
      }
    }
  }
}
