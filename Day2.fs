module AdventOfCode.Day2

open AdventOfCode.Common

let parseDirection (s: string) =
    let parts = s.Split(' ')
    let units = parts.[1] |> int

    match parts.[0] with
    | "forward" -> (units, 0)
    | "down" -> (0, units)
    | "up" -> (0, -units)
    | _ -> (0, 0)

let sumTuple (ax, ay) (dx, dy) = (ax + dx, ay + dy)

let part1 (input: string seq) =
    let x, y =
        input
        |> Seq.map parseDirection
        |> Seq.fold sumTuple (0, 0)

    x * y


let parseDirectionAim (s: string) =
    let parts = s.Split(' ')
    let units = parts.[1] |> int

    match parts.[0] with
    | "forward" -> (units, 0, 0)
    | "down" -> (0, units, units)
    | "up" -> (0, -units, -units)
    | _ -> (0, 0, 0)
    

let sumTupleAim (ax, ay, aAim) (dx, dy, dAim) =
    let aim = aAim + dAim
    (ax + dx, ay+dx*aim, aim)
let part2 (input: string seq) =
    let x, y, _ =
        input
        |> Seq.map parseDirectionAim
        |> Seq.fold sumTupleAim (0, 0, 0)
    
    x*y

let input = lines "inputs/2a.txt"
let day2 =
    printfn "part 1 is %d" <| part1 input
    printfn "part 2 is %d" <| part2 input
