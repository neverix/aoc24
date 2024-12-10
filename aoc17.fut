def main [n] (input: [n]u8) =
    let digits = map (+ (-'0')) (map i64.u8 input)
    let digits = if length digits % 2 == 1 then digits ++ [0] else digits
    let n = length digits
    let digits = unflatten (digits :> [(n/2)*2]i64)
    let digits = map (\x -> (x[0], x[1])) digits
    let free_before = digits
        |> map (.1)
        |> scan (+) 0
    let would_be_used_after = digits
        |> reverse
        |> map (.0)
        |> scan (+) 0
        |> reverse
    let exceeds = map2 (>) would_be_used_after free_before
    let valuable_index = exceeds
        |> map i64.bool
        |> reduce (+) 0
        |> (+) (-1)
    -- let can_be_used = free_before[valuable_index - 1]
    -- let need_to_slot = would_be_used_after[valuable_index + 1]
    -- let position_in_block = need_to_slot - can_be_used
    -- in [valuable_index, position_in_block]
    let spaces_before_including = free_before[valuable_index]
    let blocks_after = would_be_used_after[valuable_index + 1]
    let blocks_remaining = digits[valuable_index].0 -
        if blocks_after <= spaces_before_including
        then 0
        else blocks_after - spaces_before_including
    -- in [valuable_index, blocks_remaining]
    -- in [exceeds]
    in [free_before, would_be_used_after]