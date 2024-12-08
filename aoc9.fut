type~ charseq = {seq: []u8}

def split_by 't [n] (v: u8) (a: [n]u8): [][2]i64 =
    let indices = (zip (iota n) a)
        |> filter (\(_, x) -> x == v)
        |> map (.0) 
        |> concat [-1]
    let indices = indices ++ [n]
    in zip (init indices |> map (+1)) (tail indices)
        |> map (\(a, b) -> [a, b])

def splitter (v: u8) (a: []u8) (i: [2]i64): [][2]i64 =
    let split_results = split_by v a[i[0]:i[1]]
    in map (map (+i[0])) split_results

let zero_ascii: i64 = 48

def parse_number [n] (v: [n]u8): i64 =
    let digits = v
        |> map (i64.u8)
        |> map ((+) (-zero_ascii))
    let digits = assert ((all (>0) digits) && (all (<10) digits)) digits

    in (reverse digits)
        |> zip (indices digits)
        |> map (\(i, d) -> ((10 ** i) * d))
        |> reduce (+) 0

def number_parser (v: []u8) (index: [2]i64): i64 =
    parse_number (v[index[0]:index[1]])

let newline_ascii: u8 = 10
let bar_ascii: u8 = 124
let comma_ascii: u8 = 44
let lines = split_by newline_ascii

def main (x: []u8) =
    let double_newline_idx = (tabulate ((length x) - 1) (\i ->
        if (x[i], x[i+1]) ==
            (newline_ascii, newline_ascii)
        then i else 0
    )) |> (reduce (+) 0)
    let (first, second) = split (x :> [double_newline_idx + ((length x) - double_newline_idx)]u8)
    let second = drop 2 second
    let (first_lines, second_lines) = (lines first, lines second)

    let first_pairs = first_lines
        |> map (\se ->
            let (s, e) = (se[0], se[1])
            let bar_elems = split_by bar_ascii first[s:e]
            let elems_typed = bar_elems :> [2][2]i64
            in map (map (+s)) elems_typed
        )
        |> map (map (number_parser first))
    let max_val = 1 + i64.maximum (map i64.maximum first_pairs)
    let earlier_idces = first_pairs
        |> map (\pair -> (pair[0], pair[1]))
    let earlier = replicate max_val (replicate max_val false)
    let earlier = scatter_2d earlier earlier_idces (rep true)

    let second_matches = second_lines
        |> map (\idces ->
            let numbers = splitter comma_ascii second idces
                |> map (number_parser second)
            let pairs_match = numbers
                |> zip (indices numbers)
                |> init
                |> map (\(i, x) ->
                    numbers[i+1:]
                        |> map (\y -> earlier[x, y])
                        |> reduce (&&) true
                )
                |> reduce (&&) true
            in if pairs_match then numbers[(length numbers) // 2] else 0
        )

    in second_matches
        |> reduce (+) 0
