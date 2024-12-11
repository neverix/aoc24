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
    let total = reduce (+) 0 (map (\(x, y) -> x + y) digits)
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
    let answer = superscatter (replicate total (-1)) (manifest slices_sparse)
    let (answer, _) = loop (fs, i) = (answer, n/2 - 1) while i >= 0 do
        let (l, r, _) = slices_sparse[i]
        let w = r - l
        let fits = \j -> j < l && j + w <= total && all (== -1) fs[j:j+w]
        let s = loop j = 0 while j + w <= total && !(fits j) do j+1
        in if fits s then (((fs with [s:s+w] = replicate w i) with [l:r] = replicate w (-1)), i-1) else (fs, i-1)
    let checksum = answer
        |> zip (indices answer)
        |> map (\(i, a) -> if a == -1 then 0 else i * a)
        |> foldl (+) 0
    in checksum
