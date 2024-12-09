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
def arrow_index(a: arrow): i64 =
    match a
        case #left -> 0
        case #down -> 1
        case #right -> 2
        case #up -> 3

def check_bound (h: i64) (w: i64) (yx: [2]i64): bool =
    let (y, x) = (yx[0], yx[1])
    in y >= 0 && x >= 0 && y < h && x < w 

type sim_res = #looped | #escaped
def is_looped (r: sim_res) = match r case #looped -> true case #escaped -> false
def simulate (h: i64) (w: i64) (arr_coord: [2]i64) (arr_dir: arrow) (x: [][]u8): sim_res =
    let in_bounds = check_bound h w
    let status: sim_res = #escaped
    let boards = replicate 4 x
    let x = manifest x
    -- let (_, _, _, result) = loop (boards, arr_coord, arr_dir, status)
    --     while (status == #escaped) && in_bounds arr_coord do
    --     let arr_idx = arrow_index arr_dir
    --     let new_coord = map2 (+) arr_coord (arrow_direction arr_dir)
    --     let new_boards = (manifest boards) with [arr_idx, arr_coord[0], arr_coord[1]] = 'X'
    --     in if boards[arr_idx, arr_coord[0], arr_coord[1]] == 'X' then (boards, arr_coord, arr_dir, #looped)
    --     else if !(in_bounds new_coord) then (boards, new_coord, arr_dir, status) else
    --         let cell = x[new_coord[0], new_coord[1]]
    --         in if cell == '#' then (new_boards, arr_coord, rot90 arr_dir, status) else
    --             (new_boards, new_coord, arr_dir, status)
    in status

def main [n] (board: [n]u8) =
    let newline_idx = unwrap_i64 (index_of (== '\n') board)
    let width = newline_idx+1
    let height = (length board) / width
    let board = unflatten (board :> [height*width]u8)
    let (board, width) = (map init board, width - 1)

    let arr_idces = map (index_of is_arrow) board
    let arr_y = unwrap_i64 (index_of is_some arr_idces)
    let arr_x = unwrap_i64 arr_idces[arr_y]
    let arr_dir = unwrap_arrow (parse_arrow board[arr_y, arr_x])
    
    let arr_coord = [arr_y, arr_x]
    let simulator = simulate height width arr_coord arr_dir
    let loops = tabulate height (\y ->
        tabulate width (\x ->
            if [y, x] == arr_coord then #escaped else
            simulator ((manifest board) with [y, x] = '#')
        )
    )

    in loops
        |> map (\x -> x
            |> map is_looped
            |> map i64.bool
            |> reduce (+) 0
        )
        |> reduce (+) 0