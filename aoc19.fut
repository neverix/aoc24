type option 't = #some t | #none

def index_of 't [n] (cond: t -> bool) (x: [n]t): option i64 =
    let indices = x
        |> zip (indices x)
        |> filter (\(_, v) -> cond v)
        |> map (\(i, _) -> i)
    in if length indices == 0 then #none else #some (head indices)

def unwrap 't (default: t) (v: option t): t =
    match v
        case #some x -> x
        case #none -> assert false default
let unwrap_i64 = unwrap 0i64


def main [n] (board: [n]u8) =
    let newline_idx = unwrap_i64 (index_of (== '\n') board)
    let width = newline_idx+1
    let height = (length board) / width
    let board = unflatten (board :> [height*width]u8)
    let (board, width) = (map init board, width - 1)
    let digits = map (map (+ (-'0'))) (map (map i64.u8) board)
    let path_counts = map (map ((== 9) >-> i64.bool)) digits
    let (path_counts, _) = loop (path_counts, i) = (path_counts, 8) while i >= 0 do
        ((tabulate height (\y -> tabulate width (\x ->
            if digits[y, x] != i then 0 else (0
             + (if y > 0 then path_counts[y-1, x] else 0)
             + (if y < height - 1 then path_counts[y+1, x] else 0)
             + (if x > 0 then path_counts[y, x-1] else 0)
             + (if x < width - 1 then path_counts[y, x+1] else 0)
            )
        ))), i-1)
    in path_counts
        |> map (foldl (+) 0)
        |> foldl (+) 0
