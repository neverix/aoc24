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

def sum_up (n: i64) (f: i64 -> i64): i64 =
    tabulate n f
        |> reduce (+) 0

def main [n] (board: [n]u8) =
    let newline_idx = unwrap_i64 (index_of (== '\n') board)
    let width = newline_idx+1
    let height = (length board) / width
    let board = unflatten (board :> [height*width]u8)
    let (board, width) = (map init board, width - 1)
    let unique_locs = sum_up height (\y1 ->
        sum_up width (\x1 ->
            if sum_up height (\y2 ->
                sum_up width (\x2 ->
                    (1...(i64.max height width))
                    |> map (\p ->
                        let (y3, x3) = (y1 + (y2 - y1) * (p + 1), x1 + (x2 - x1) * (p + 1))
                        let (y2, x2) = (y1 + (y2 - y1) * p, x1 + (x2 - x1) * p)
                        in if y3 < 0 || x3 < 0 || y3 >= height || x3 >= width then 0 else
                        let (a1, a2, a3) = (board[y1, x1], board[y2, x2], board[y3, x3])
                        in if a2 != '.' && a2 == a3 then 1 else 0
                    )
                    |> reduce (+) 0
                )
            ) > 0 then 1 else 0
        )
    )
    in unique_locs