import gleam/io
import gleam/string

@external(erlang, "cover_ffi", "analyse_to_file")
pub fn analyse_to_file() -> Result(Nil, Nil) {
  Ok(Nil)
}

@external(erlang, "cover", "start")
fn do_start() -> Nil {
  Nil
}

@external(erlang, "cover_ffi", "imported_modules")
fn imported_modules() -> List(String) {
  []
}

pub fn imported_mods() -> List(String) {
  imported_modules()
}

@external(erlang, "cover_ffi", "compile_beam_directory")
fn compile_beam_directory(_dir: String) -> Result(Nil, Nil) {
  Ok(Nil)
}

pub fn start() -> Nil {
  do_start()
  let assert Ok(Nil) = compile_beam_directory("build/dev/erlang/glemur/ebin")
  io.println_error(string.inspect(imported_mods()))
  Nil
}
