import "parsing"

-- def w = 7i64
-- def h = 7i64
-- def first = 12i64
def w = 71i64
def h = 71i64
def first = 1024i64


def main [n] (text: [n]u8) =
    let segments = parsing.split_by '\n' text
    let parsed = segments
        |> map (\se ->
            let segment = text[se[0]:se[1]]
            let parts = parsing.split_by ',' segment
            let n1 = segment[parts[0][0]:parts[0][1]]
            let n2 = segment[parts[1][0]:parts[1][1]]
            in [parsing.parse_number n1, parsing.parse_number n2]
        )
    let parsed = parsed[:first]
    let grid = replicate h (replicate w 0i64)
    let grid = scatter_2d grid (parsed |> map (\x -> (x[0], x[1]))) (rep 1)
    let grid = grid with [0, 0] = 2
    let (i, _) = loop (i, grid) = (0i64, grid)
        while grid[h-1, w-1] != 2 do
        (i + 1, tabulate h (\y -> tabulate w (\x ->
            if grid[y, x] == 1 then 1 else
            if y > 0 && grid[y-1, x] == 2
            || x > 0 && grid[y, x-1] == 2
            || y < h-1 && grid[y+1, x] == 2
            || x < w-1 && grid[y, x+1] == 2
            then 2 else 0
        )))
    in i
