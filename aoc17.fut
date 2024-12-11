-- https://futhark-lang.org/examples/binary-search.html
-- can be replaced with eytzinger search
def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n-1) while l < r do
    let t = l + (r - l) / 2
    in if xs[t] `lte` x
       then (l, t)
       else (t+1, r)
  in l

-- TODO add sorting
-- TODO add eytzinger
def hyperscatter 'i 't [n] (transformer: i64 -> i -> t) (target: [n]t) (slices: [](i64, i64, i)): [n]t =
    target
        |> zip (indices target)
        |> map (\(i, x) ->
            let slice_match = binary_search (\(l, r, _) (x, _, _) -> x < r) slices (i, i, slices[0].2)
            let (l, r, y) = slices[slice_match]
            in if l <= i && i < r then transformer i y else x
        )

def superscatter = hyperscatter (\_ x -> x)


def main [n] (input: [n]u8) =
    let digits = map (+ (-'0')) (map i64.u8 input)
    let digits = if length digits % 2 == 1 then digits ++ [0] else digits
    let n = length digits
    let digits = unflatten (digits :> [(n/2)*2]i64)
    let digits = map (\x -> (x[0], x[1])) digits
    let total_used = reduce (+) 0 (map (.0) digits)
    let total_empty = reduce (+) 0 (map (.1) digits)
    let idx_dense = digits
        |> map (.0)
        |> ([0]++)
        |> init
        |> scan (+) 0
        :> [n/2]i64
    let slices_dense = digits
            |> zip idx_dense
            |> zip (indices digits)
            |> map (\(id, (i, (f, _))) -> (i, i+f, id))
    let flat_ids = superscatter (replicate total_used 0) slices_dense
    let idx_sparse = digits
        |> map (\(x, y) -> x + y)
        |> ([0]++)
        |> init
        |> scan (+) 0
        :> [n/2]i64
    let slices_sparse = digits
        |> zip idx_sparse
        |> zip (indices digits)
        |> map (\(id, (i, (f, _))) -> (i, i+f, id))
    let answer = superscatter (replicate total_used 0) slices_sparse
    let to_the_right = digits
        |> reverse
        |> map (.1)
        |> ([0]++)
        |> init
        |> scan (+) 0
        |> reverse
        :> [n/2]i64
    -- superscatter is not enough, we need a...
    let slices_empty = digits
        |> zip idx_sparse
        |> zip (indices digits)
        |> map (\(id, (i, (f, d))) -> (i+f, i+f+d, id))
    let answer = hyperscatter (\i id ->
        let ttr = to_the_right[id]
        let (emp_start, emp_end, _) = slices_empty[id]
        let id_among_empties = ttr + (emp_end - i - 1)
        in flat_ids[id_among_empties + (total_used-total_empty)]
    ) answer slices_empty
    let checksum = answer
        |> zip (indices answer)
        |> map (\(i, a) -> i * a)
        |> foldl (+) 0
    in checksum
