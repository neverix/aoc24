type~ charseq = {seq: []u8}

def split_by 't [n] (v: u8) (a: [n]u8): [][2]i64 =
    let indices = (zip (iota n) a)
        |> filter (\(_, x) -> x == v)
        |> map (.0) 
        |> concat [-1]
    let indices = indices ++ [n]
    in zip (init indices |> map (+1)) (tail indices)
        |> map (\(a, b) -> [a, b])


let newline_ascii: u8 = 10
let bar_ascii: u8 = 124
let lines = split_by newline_ascii

def main (x: []u8) =
    let idx = (tabulate ((length x) - 1) (\i ->
        if (x[i], x[i+1]) ==
            (newline_ascii, newline_ascii)
        then i else 0
    )) |> (reduce (+) 0)
    let (first, second) = split (x :> [idx + ((length x) - idx)]u8)
    let second = drop 2 second
    let (first_lines, second_lines) = (lines first, lines second)

    let first_pairs = first_lines
        |> map (\se ->
            let (s, e) = (se[0], se[1])
            let bar_elems = split_by bar_ascii first[s:e]
            let elems_typed = bar_elems :> [2][2]i64
            in map (map (+s)) elems_typed
        )
    

    in first_pairs[1][1]
