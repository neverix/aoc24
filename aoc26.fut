import "parsing"

-- def prefix = ""
def prefix = "1000000000"

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
        |> map (\{a_x, a_y, b_x, b_y, t_x, t_y} ->
            let null = [0f64, 0f64, 0f64, 0f64]
            -- let null = 0i64
            let det = a_x * b_y - a_y * b_x
            in if det == 0 then assert false null else
            let inv_mat = [[b_y, -b_x], [-a_y, a_x]] |> map (map (f64.i64 >-> (/(f64.i64 det))))
            let t_vec = [f64.i64 t_x, f64.i64 t_y]
            let inv_vec = matmul_f64 inv_mat t_vec
            let mat = [[a_x, b_x], [a_y, b_y]] |> map (map (f64.i64))
            in if all (<1e-1) <| map (f64.abs) <| map2 (-) (matmul_f64 mat inv_vec) t_vec then
                let sqr = \x -> x * x
                let loss_fn = \inv_vec -> (
                    f64.sum (map sqr <| map2 (-) (matmul_f64 mat inv_vec) t_vec)
                    + f64.sum (map sqr <| map2 (-) (map f64.round inv_vec) inv_vec)
                )
                let loss_grad = \x -> vjp loss_fn x 1f64
                let new_inv_vec = (.1) <| loop (i, x) = (0, inv_vec) while i < 100 do
                    (i + 1, map2 (-) x <| map (*(1e-2/(f64.sum <| map f64.abs inv_vec))) <| loss_grad x)
                in (new_inv_vec ++ (matmul_f64 mat new_inv_vec)) :> [4]f64
                -- let inv_vec = new_inv_vec
                -- in if all (\x -> f64.abs(x - f64.round x) < 1e-2) inv_vec then
                --     i64.f64 <| f64.round (inv_vec[0] * 3 + inv_vec[1])
                --     -- i64.f64 new_inv_vec[0]
                --     -- (new_inv_vec ++ (matmul_f64 mat new_inv_vec))
                -- else null
            else
                null
        )
    in smallest 
    -- in reduce (+) 0 smallest
