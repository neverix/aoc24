type option 't = #some t | #none

def or_default 't (default: t) (v: option t): t =
    match v
        case #some x -> x
        case #none -> default

def unwrap 't (default: t) (v: option t): t =
    match v
        case #some x -> x
        case #none -> assert false default
let unwrap_i64 = unwrap 0i64

def is_some 't (v: option t): bool =
    match v
        case #some _ -> true
        case #none -> false

module parsing = {

    def index_of 't [n] (cond: t -> bool) (x: [n]t): option i64 =
        let indices = x
            |> zip (indices x)
            |> filter (\(_, v) -> cond v)
            |> map (\(i, _) -> i)
        in if length indices == 0 then #none else #some (head indices)

    def split_by 't [n] (v: u8) (a: [n]u8): [][2]i64 =
        let indices = (zip (iota n) a)
            |> filter (\(_, x) -> x == v)
            |> map (.0) 
            |> concat [-1]
        let indices = indices ++ [n]
        in zip (init indices |> map (+1)) (tail indices)
            |> map (\(a, b) -> [a, b])

    def arreq [n] (a: [n]u8) (b: [n]u8): bool =
        map2(\x y -> x == y) a b |> and

    def split_by_long 't [n] [m] (v: [m]u8) (a: [n]u8): [][2]i64 =
        let indices = iota n
            |> filter (\i -> if i+m <= n then arreq (a[i:i+m] :> [m]u8) v else false)
            |> concat [-m]
        let indices = indices ++ [n]
        in zip (init indices |> map (+m)) (tail indices)
            |> map (\(a, b) -> [a, b])

    def splitter (v: u8) (a: []u8) (i: [2]i64): [][2]i64 =
        let split_results = split_by v a[i[0]:i[1]]
        in map (map (+i[0])) split_results

    let zero_ascii: i64 = 48
    def parse_number [n] (v: [n]u8): i64 =
        let digits = v
            |> map (i64.u8)
            |> map ((+) (-zero_ascii))
        let digits = assert ((all (>=0) digits) && (all (<10) digits)) digits

        in (reverse digits)
            |> zip (indices digits)
            |> map (\(i, d) -> ((10 ** i) * d))
            |> reduce (+) 0

    def number_parser (v: []u8) (index: [2]i64): i64 =
        parse_number (v[index[0]:index[1]])
}