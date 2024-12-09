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

def is_some 't (v: option t): bool =
    match v
        case #some _ -> true
        case #none -> false

type arrow = #left | #up | #right | #down
def parse_arrow (v: u8): option arrow =
    if v == 'v' then #some #down else
    if v == '>' then #some #right else
    if v == '<' then #some #left else
    if v == '^' then #some #up else
    #none
let unwrap_arrow: (v: option arrow) -> arrow = unwrap #down
def is_arrow (v: u8): bool =
    is_some (parse_arrow v)
def arrow_direction (a: arrow): [2]i64 =
    match a
        case #down -> [1, 0]
        case #right -> [0, 1]
        case #left -> [0, -1]
        case #up -> [-1, 0]
def rot90 (a: arrow): arrow =
    match a
        case #down -> #left
        case #left -> #up
        case #up -> #right
        case #right -> #down


def check_bound (h: i64) (w: i64) (yx: [2]i64): bool =
    let (y, x) = (yx[0], yx[1])
    in y >= 0 && x >= 0 && y < h && x < w 

def main [n] (x: [n]u8) =
    let newline_idx = unwrap_i64 (index_of (== '\n') x)
    let width = newline_idx+1
    let height = (length x) / width
    let x = unflatten (x :> [height*width]u8)
    let (x, width) = (map init x, width - 1)

    let arr_idces = map (index_of is_arrow) x
    let arr_y = unwrap_i64 (index_of is_some arr_idces)
    let arr_x = unwrap_i64 arr_idces[arr_y]
    let arr_dir = unwrap_arrow (parse_arrow x[arr_y, arr_x])
    let x = scatter_2d x [(arr_y, arr_x)] ['X']
    
    let arr_coord = [arr_y, arr_x]
    let in_bounds = check_bound height width
    let (x, arr_coord, arr_dir) = loop (x, arr_coord, arr_dir)
        while in_bounds arr_coord do
            let new_coord = map2 (+) arr_coord (arrow_direction arr_dir)
            in if !(in_bounds new_coord) then (x, new_coord, arr_dir) else
                let cell = x[new_coord[0], new_coord[1]]
                in if cell == '#' then (x, arr_coord, rot90 arr_dir)
                else (x with [new_coord[0], new_coord[1]] = 'X', new_coord, arr_dir)

    in x
        |> map (\x -> x
            |> map ((== 'X'))
            |> map i64.bool
            |> reduce (+) 0
        )
        |> reduce (+) 0