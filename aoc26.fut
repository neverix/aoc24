import "parsing"

def prefix = 0i64
-- def prefix = 10000000000000

import "parsing"


type gcds = {gcd: i64, aw: i64, bw: i64}
def egcd (a: i64) (b: i64): gcds =
    let (a, _, a_w_a, a_w_b, _, _) = loop (a, b, a_w_a, a_w_b, b_w_a, b_w_b) = (a, b, 1, 0, 0, 1)
        for _ in 0...200 do
            if b == 0 then (a, b, a_w_a, a_w_b, b_w_a, b_w_b) else
            let m = a / b
            in (b, a - b * m, b_w_a, b_w_b, a_w_a - b_w_a * m, a_w_b - b_w_b * m)
    in {gcd = a, aw = a_w_a, bw = a_w_b}
def gcd_flip ({gcd, aw, bw}: gcds) =
    {gcd = -gcd, aw = -aw, bw = -bw}

def egcd_example a b =
    let r = egcd a b
    in [r.gcd, r.aw, r.bw]

-- > egcd_example 9 6

-- > egcd_example 22 94

-- > egcd_example 22 -94

-- > egcd_example -94 22

type dio_sol = {a0: i64, aw: i64, b0: i64, bw: i64}

def solve_dio (a: i64) (b: i64) (c: i64): option(dio_sol) =
    let {gcd, aw, bw} = egcd a b
    in if c % gcd != 0 then #none else
    let m = c / gcd
    let (a0, b0) = (aw * m, bw * m)
    in #some {a0, b0, aw=b/gcd, bw= -a/gcd}


def dio_pos (ds: dio_sol): dio_sol =
    let {a0, aw, b0, bw} = ds
    in if a0 >= 0 && b0 >= 0 then ds else
    if a0 < 0 then
        let m = -(-a0 - (-aw - 1)) / (-aw)
        in {a0 = a0 + aw * m, b0 = b0 + bw * m, aw, bw}
    else
        let m = -(-b0 ) / (-bw)
        in {a0 = a0 + aw * m, b0 = b0 + bw * m, aw, bw}



def dio_min ({a0, aw, b0, bw}: dio_sol): dio_sol =
    let m = if a0 < 0 then
        (-a0 + aw - 1) / aw
    else
        -(a0) / aw
    in {a0 = a0 + aw * m, b0 = b0 + bw * m, aw, bw}

def ddio = {a0=0, aw=0, b0=0, bw=0} :> dio_sol
def dunwrap = unwrap (ddio)
def dio_example a b c =
    let r = dunwrap (solve_dio a b c)
    -- let r = dio_pos <| dio_min r
    let r = dio_pos r
    in [r.a0, r.b0, r.aw, r.bw,
        r.a0 * a + r.b0 * b, r.aw * a + r.bw * b]

-- > dio_example 22 94 138

-- > dio_example 22 94 8400

-- > dio_example 94 22 8400

-- returns a linked dio_sol
def dio_combine_single (ds: dio_sol): option(dio_sol) =
    -- ds.a0 + x * ds.aw = ds.b0 + y * ds.bw
    -- ds.a0 - ds.b0 = x * (-ds.aw) + y * ds.bw
    solve_dio (-ds.aw) (ds.bw) (ds.a0 - ds.b0)

def dcs_example a0 aw b0 bw =
    let r = dunwrap (dio_combine_single {a0, aw, b0, bw})
    in [r.a0, r.b0, r.aw, r.bw]

-- > dcs_example 2 3 0 4

def dio_combine_single_simple (ds: dio_sol): option((i64, i64)) =
    match dio_combine_single ds
    case #none -> #none
    case #some {a0, aw, b0 = _, bw = _} -> #some (
        a0 * ds.aw + ds.a0, aw * ds.aw
    )

def dcss_example a0 aw b0 bw =
    let (a, b) = unwrap (0, 0) (dio_combine_single_simple {a0, aw, b0, bw})
    in [a, b]

-- > dcss_example 2 3 0 4

-- > dcss_example 147 67 80 11

-- > dcss_example 40 -47 6 -34

def dio_combine (a: dio_sol) (b: dio_sol): option(dio_sol) =
    match dio_combine_single {a0 = a.a0, aw = a.aw, b0=b.a0, bw=b.aw}
    case #none -> #none
    case #some {a0=x_a0, b0=x_b0, aw=x_aw, bw=x_bw} ->
    match dio_combine_single {a0 = a.b0, aw = a.bw, b0=b.b0, bw=b.bw}
    case #none -> #none
    case #some {a0=y_a0, b0=y_b0, aw=y_aw, bw=y_bw} ->
    #some {a0=x_a0, aw=x_aw, b0=y_a0, bw=y_aw}
    -- #some {a0=x_b0, aw=x_bw, b0=y_b0, bw=y_bw}


def doc_example a_x0 a_xw a_y0 a_yw b_x0 b_xw b_y0 b_yw =
    let sol = dunwrap (dio_combine
        {a0=a_x0, aw=a_xw, b0=a_y0, bw=a_yw}
        {a0=b_x0, aw=b_xw, b0=b_y0, bw=b_yw})
    -- in [sol.a0, sol.aw, sol.b0, sol.bw]
    let (u, v) = unwrap (0, 0) <| dio_combine_single_simple sol
    -- in [u, v]
    in [sol.a0, sol.aw, sol.b0, sol.bw]

-- -- > doc_example -1 1 -1 -2 8 -4 3 -3

-- > doc_example 9 1 9 -2 18 -4 13 -3

    -- match dio_combine_single {a0 = a.a0, aw = a.aw, b0=b.a0, bw=b.aw}
    -- case #none -> #none
    -- case #some ds1 ->
    -- match dio_combine_single {a0 = a.b0, aw = a.bw, b0=b.b0, bw=b.bw}
    -- case #none -> #none
    -- case #some ds2 ->
    -- -- ds1: some weighting of x's and y's solutions that
    -- --  preserves the correctness
    -- match dio_combine_single_simple {
    --     a0=ds1.a0, aw=ds1.aw, b0=ds2.a0, bw=ds2.aw
    -- }
    -- case #none -> #none
    -- case #some (a0, aw) ->
    -- match dio_combine_single_simple {
    --     a0=ds1.b0, aw=ds1.bw, b0=ds2.b0, bw=ds2.bw
    -- }

    -- case #none -> #none
    -- case #some (b0, bw) -> #some (
    --     -- let a_b_combinations = {a0, aw, b0, bw}
    --     -- in a_b_combinations
    --     {
    --         a0=a.a0 + a.aw*a0 + b.a0 + b.aw*b0,
    --         -- aw=a.aw*aw + b.aw*bw,
    --         aw=a.aw*aw,
    --         b0=a.b0 + a.bw*a0 + b.b0 + b.bw*b0,
    --         -- bw=a.bw*aw + b.bw*bw
    --         bw=a.bw*aw
    --     }
    -- )

    -- -- let a_options = dio_combine_single {a0 = a.a0, aw = a.aw, b0=b.a0, bw=b.aw}
    -- match dio_combine_single_simple {a0 = a.a0, aw = a.aw, b0=b.a0, bw=b.aw}
    -- case #none -> #none
    -- case #some (a0, aw) ->
    -- match dio_combine_single_simple {a0 = a.b0, aw = a.bw, b0=b.b0, bw=b.bw}
    -- case #none -> #none
    -- case #some (b0, bw) ->
    -- -- if (a0 - a.a0) % aw != 0 then #none else
    -- -- if (b0 - b.b0) % bw != 0 then #none else
    -- -- #some {a0 = a.a0, aw, b0 = b.b0, bw}
    -- #some {a0, aw, b0, bw}
    -- -- in #some a

def dc_example a b c d e f =
    let x = dunwrap (solve_dio a b e)
    let y = dunwrap (solve_dio c d f)
    -- let x = dio_pos x
    -- let y = dio_pos y

    let z = x

    -- find weights that make a/b overlap

    let u = dunwrap <| dio_combine_single {a0 = x.a0, b0 = y.a0, aw = x.aw, bw = y.aw}
    let v = dunwrap <| dio_combine_single {a0 = x.b0, b0 = y.b0, aw = x.bw, bw = y.bw}
    let (g, h) = unwrap (0, 0) <| dio_combine_single_simple {a0 = u.a0, aw = u.aw, b0 = v.a0, bw = v.aw}

    in [u.a0, u.aw, v.a0, v.aw, g, h]
    -- in [z.a0, z.b0, z.aw, z.bw,
    --     z.a0 * a + z.b0 * b, z.a0 * c + z.b0 * d]
    -- let z: (dio_sol, dio_sol) = unwrap (ddio, ddio) <| dio_combine x y
    -- in [
    --     [z.0.a0, z.0.b0, z.0.aw, z.0.bw],
    --     [z.1.a0, z.1.b0, z.1.aw, z.1.bw]
    -- ]

-- > dc_example 94 22 34 67 8400 5400

-- > dc_example 94 22 34 67 100000000000008400 100000000000005400

def solve a b c d e f: option dio_sol =
    match solve_dio a b e
    case #none -> #none
    case #some x ->
    match solve_dio c d f
    case #none -> #none
    case #some y ->
    -- let z = dunwrap <| dio_combine x y
    match dio_combine x y
    case #none -> #none
    case #some z ->
    -- #some (dio_pos z)
    let u = dio_pos z
    in if u.a0 < 0 || u.b0 < 0 then #none
    else #some u


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
                t_x = prefix + parsing.parse_number (line3[x_start:x_end]),
                t_y = prefix + parsing.parse_number (line3[y_start:])
            }
        )
    let smallest = parsed
        |> map (\{a_x, a_y, b_x, b_y, t_x, t_y} -> (
            if t_x / a_x == t_y / a_y then [t_x / a_x * 3, 0, 0, 0] else
            if t_x / b_x == t_y / b_y then [t_x / b_x, 0, 0, 0] else
            let null = [0, 0, 0, 0]
            in match solve a_x b_x a_y b_y t_x t_y
            case #none -> null
            case #some x ->
                null
        ))
    in smallest
    -- in smallest
    --     |> map (\x -> (x[0] ,x[1]))
    --     |> map (\(a, b) -> a * 3 + b)
    --     |> reduce (+) 0
