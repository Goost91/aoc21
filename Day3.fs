module AdventOfCode.Day3

open AdventOfCode.Common

let parse c = int c - int '0'

let part1 (input: string list) =
    let values = transposeWith parse input
    let sum =
        [ 0 .. input.[0].Length - 1 ]
        |> List.map
            (fun i ->
                let sum = getRow i values |> Array.sum
                if sum > (input.Length / 2) then 1 else 0)
        |> List.fold (fun acc elem -> acc <<< 1 ||| elem) 0

    let inverse = inverse sum
    sum * inverse


let input = lines "inputs/3a.txt" |> Seq.toList

let day3 = printfn "day 3 part 1 = %d" <| part1 input
    
