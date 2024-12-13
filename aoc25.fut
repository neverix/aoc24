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
                a_y = parsing.parse_number line1[16:18],
                b_x = parsing.parse_number line2[12:14],
                b_y = parsing.parse_number line2[16:18],
                t_x = parsing.parse_number line3[x_start:x_end],
                t_y = parsing.parse_number line3[y_start:]
            }
        )
    in map (.t_y) parsed
    -- let newline_idx = unwrap_i64 (index_of (== '\n') board)
    -- let width = newline_idx+1
    -- let height = (length board) / width
    -- let board = unflatten (board :> [height*width]u8)
    -- let (board, width) = (map init board, width - 1)