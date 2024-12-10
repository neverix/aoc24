import gleam/float
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/result
import gleam/string
import parallel_map.{WorkerAmount}
import simplifile

fn runwrap(x: Result(value, error)) -> value {
  result.lazy_unwrap(x, or: fn() { panic })
}

fn sum_up(vals: List(Int), index: Int) {
  let #(current, next) = #(index % 3, index / 3)
  case vals {
    [first, second, ..rest] -> {
      let next = sum_up([second, ..rest], next)
      case current {
        0 -> first + next
        1 -> first * next
        _ -> int.parse(int.to_string(next) <> int.to_string(first)) |> runwrap
      }
    }
    [only] -> only
    _ -> panic
  }
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

  let can_match =
    lines
    |> parallel_map.list_pmap(
      fn(line) {
        let #(target, vals) = line
        let possibilities =
          list.range(
            0,
            int.power(3, int.to_float(list.length(vals)) -. 1.0)
              |> runwrap
              |> float.round,
          )
        let revals = list.reverse(vals)
        let does_match =
          possibilities
          |> list.any(fn(i) { sum_up(revals, i) == target })
        case does_match {
          True -> target
          False -> 0
        }
      },
      WorkerAmount(16),
      10_000,
    )
    |> list.map(runwrap)
    |> list.fold(0, int.add)

  io.debug(can_match)
}
