module AdventOfCode.Common


let lines : string -> seq<string> = System.IO.File.ReadLines

let readInts file =
    lines file  
    |> Seq.map (fun x -> int x)
    |> Seq.toList