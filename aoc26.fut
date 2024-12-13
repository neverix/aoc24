import "parsing"

-- def prefix = ""
def prefix = "100000"

def matmul [n][m] 't (add: t -> t -> t) (mul: t -> t -> t) (zero: t) (x: [n][m]t) (y: [m]t): [n]t =
    map (\z -> reduce add zero (map2 mul y z)) x

def matmul_f64 = matmul (+) (*) (0f64)

def main [n] (text: [n]u8) =
    let segments = parsing.split_by_long ['\n', '\n'] text
    let parsed = segments
        |> map (\se ->
            let segment = text[se[0]:se[1]]
            let lines = parsing.split_by '\n' segment
            let line1 = segment[lines[0][0]:lines[0][1]]
            let line2 = segment[lines[1][0]:lines[1][1]]
            let line3 = segment[lines[2][0]:lines[2][1]]
            let line3_halved = parsing.split_by_long [',', ' ', 'Y', '='] line3
            let (x_end, y_start) = (line3_halved[0][1], line3_halved[1][0])
            let x_start = (parsing.split_by_long ['X', '='] line3)[1][0]
            in {
                a_x = parsing.parse_number line1[12:14],
                a_y = parsing.parse_number line1[18:20],
                b_x = parsing.parse_number line2[12:14],
                b_y = parsing.parse_number line2[18:20],
                t_x = parsing.parse_number (prefix ++ line3[x_start:x_end]),
                t_y = parsing.parse_number (prefix ++ line3[y_start:])
            }
        )
    let smallest = parsed
        |> map (\{a_x, a_y, b_x, b_y, t_x, t_y} -> (
            -- let null = 0i64
            let null = [0i64, 0i64, 0i64, 0i64]
            let base_iters = reduce i64.min i64.highest [(t_x/a_x + 1), (t_y/a_y + 1)]
            let idces = [{a_x, t_x, b_x}, {a_x = a_y, t_x = t_y, b_x = b_y}] |> map (\{a_x, t_x, b_x} ->
                let conds = tabulate 10000 (\a -> (t_x - a * a_x) % b_x == 0)
                let idces = conds
                    |> zip (indices conds) |> filter (.1) |> map (.0)
                in (if length idces == 0 then #none else #some (idces[:4] :> [4]i64)) :> (option([4]i64)))
            in match (idces[0], idces[1])
                case (#none, #none) -> null
                case (#some _, #none) -> null
                case (#none, #some _) -> null
                case (#some a, #some b) ->
                    let (a_b, a_w) = (a[0], a[1] - a[0])
                    let (b_b, b_w) = (b[0], b[1] - b[0])
                    let matches = tabulate 1000 (\a_v ->
                        let rem = (a_b + a_w * a_v) - b_b
                        in rem >= 0 && rem % b_w == 0
                    )
                    let idces = matches
                        |> zip (indices matches) |> filter (.1) |> map (.0)
                    in match length idces
                        case 0 -> null
                        case 1 -> assert false (replicate 4 idces[0])
                        case _ ->
                            let (a_b_2, a_w_2) = (idces[0], idces[1] - idces[0])
                            let (a_b, a_w) = (a_b + a_b_2 * a_w, a_w_2 * a_w)
                            let (x_rem0, y_rem0) = ((t_x - a_b * a_x) / b_x, (t_y - a_b * a_y) / b_y)
                            let (x_rem1, y_rem1) = ((t_x - (a_b + a_w) * a_x) / b_x, (t_y - (a_b + a_w) * a_y) / b_y)

                            -- let a_w = a_w * 2
                            -- in [(t_x - a_b * a_x) % b_x + 1, (t_y - a_b * a_y) % b_y + 1,
                            --     (t_x - (a_b + a_w) * a_x) % b_x + 1, (t_y - (a_b + a_w) * a_y) % b_y + 1]

                            -- a + bx = c + dx
                            -- a - c + bx = dx
                            -- a - c = dx - bx
                            -- (a-c)/(b-d)

                            let a = x_rem0
                            let b = x_rem1 - x_rem0
                            let c = y_rem0
                            let d = y_rem1 - y_rem0
                            -- (b-d)x = (c-a)
                            -- x = (a-c)/(b-d)
                            in [t_x - x_rem0, t_x - x_rem1, 0, 0]
                            -- in [a-c, b-d, (a-c)%(b-d), (a-c)/(b-d)]
                            -- in if (c - a) % (b - d) != 0 then null else
                            -- let _ = 0
                            -- in [a, b, c, d]
                            -- if (a - c) / (b - d) <= 0 then null else
                            -- let a = (a - c) / (b - d)
                            -- in [a, a, a, a]

                        --     -- in [length idces, x_b, x_w, idces[0]]
                        --     in tabulate 4 (\a_v_2 ->
                        --         let a_v = a_v_2 * a_w_2 + a_b_2
                        --         let b_v = ((a_b + a_w * a_v) - b_b) / b_w
                        --         in t_x - (a_x * a_v) % b_v
                        --     )
                        -- in [a_b, a_w, b_b, b_w]
            -- in indices |> map (or_default [0, 0, 0, 0])
            -- -- let cost = tabulate iters (\a ->
            -- --     let b = (t_x - a * a_x) / b_x
            -- --     in if a * a_x + b * b_x != t_x || a * a_y + b * b_y != t_y
            -- --         then #none
            -- --         else #some (a * 3 + b)
            -- -- )
            -- -- |> reduce_comm (\a b -> match a
            -- --     case #none -> b
            -- --     case #some x -> match b
            -- --         case #none -> a
            -- --         case #some y ->
            -- --             if x < y then a else b
            -- -- ) #none
            -- -- in cost |> or_default 0
        ))
    in smallest 
    -- in reduce (+) 0 smallest
