import "parsing"

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
                t_x = parsing.parse_number line3[x_start:x_end],
                t_y = parsing.parse_number line3[y_start:]
            }
        )
    let smallest = parsed
        |> map (\{a_x, a_y, b_x, b_y, t_x, t_y} ->
            let results = tabulate 100 (\a -> tabulate 100 (\b ->
                { matched = (a * a_x + b * b_x == t_x) && (a * a_y + b * b_y == t_y), cost = a * 3 + b }
            )) |> flatten
            let best_result = results
                |> map (\{matched, cost} -> (if matched then #some cost else #none) :> option i64)
                |> reduce (\a b -> match a
                    case #some x -> (match b
                        case #some y -> if x < y then a else b
                        case #none -> a)
                    case #none -> b
                    ) #none
            in or_default 0 best_result
        )
    in reduce (+) 0 smallest

    -- let newline_idx = unwrap_i64 (index_of (== '\n') board)
    -- let width = newline_idx+1
    -- let height = (length board) / width
    -- let board = unflatten (board :> [height*width]u8)
    -- let (board, width) = (map init board, width - 1)