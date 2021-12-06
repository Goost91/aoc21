module AdventOfCode.Day1
open AdventOfCode.Common

(* This could probably be cleaned up a little bit*)

let part1 (values:int list) =
    let rec inner values prev =
        match values with
        | head :: tail when head > prev -> 1 + inner tail head
        | head :: tail when head <= prev -> 0 + inner tail head
        | _ -> 0
    inner values.Tail values.Head



let part2 (values: int32 list) =
    let measurements =
        [ 0 .. values.Length - 3 ]
        |> List.map (fun x -> values.[x..x + 2] |> List.sum)

    part1 measurements

let input = readInts "inputs/1a.txt"

let day1 =
    printfn "part 1 is %d" <| part1 input
    printfn "part 2 is %d" <| part2 input
