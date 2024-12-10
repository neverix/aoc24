import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

fn runwrap(x: Result(value, error)) -> value {
  result.lazy_unwrap(x, or: fn() { panic })
}

pub fn main() {
  let assert Ok(result) = simplifile.read(from: "input.txt")
  let lines =
    result
    |> string.split(on: "\n")
    |> list.map(string.split_once(_, on: ": "))
    |> list.map(runwrap)
    |> list.map(fn(ab) {
      case ab {
        #(a, b) -> #(
          a |> int.parse |> runwrap,
          b
            |> string.split(on: " ")
            |> list.map(fn(s) { runwrap(int.parse(s)) }),
        )
      }
    })

  io.debug(lines)
}
