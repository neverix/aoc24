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


def find_visited [height] [width] (digits: [height][width]i64): [][]i64 =
    let start = map (map ((== 9) >-> i64.bool)) digits
    let (_, visited, _) = loop (current, tagged, i) = (start, manifest start, 8) while i >= 0 do
        let new_path = (tabulate height (\y -> tabulate width (\x ->
            let update = 0
             + (if y > 0 then current[y-1, x] else 0)
             + (if y < height - 1 then current[y+1, x] else 0)
             + (if x > 0 then current[y, x-1] else 0)
             + (if x < width - 1 then current[y, x+1] else 0)
            let update = if digits[y, x] != i then 0 else update
            in (update, if digits[y, x] != i then tagged[y, x] else update)
        )))
        in ((map (map (.0)) new_path), (map (map (.1)) new_path), i-1)
    in visited

def main [n] (board: [n]u8) =
    let newline_idx = unwrap_i64 (index_of (== '\n') board)
    let width = newline_idx+1
    let height = (length board) / width
    let board = unflatten (board :> [height*width]u8)
    let (board, width) = (map init board, width - 1)
    let digits = map (map (+ (-'0'))) (map (map i64.u8) board)
    let visited_fw = find_visited (digits)
    let visited_bw = find_visited (map (map (\x -> 9 - x)) digits)
    let visited_at_all = map2 (map2 (&&)) (map (map (>0)) visited_fw) (map (map (>0)) visited_bw)
    let visited = map3 (map3 (\x y z -> if z then y else 0)) visited_fw visited_bw visited_at_all
    let nines_nearby: i64 = tabulate height (\y ->
        tabulate width (\x ->
            if digits[y, x] == 9 then (
                let options = [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]
                in options
                    |> filter (\(y, x) -> x >= 0 && y >= 0 && x < width && y < height)
                    |> map (\(y, x) -> visited[y, x])
                    |> foldl (+) 0
            ) else 0
        )
        |> foldl (+) 0
    ) |> foldl (+) 0
    -- in nines_nearby
    in visited
