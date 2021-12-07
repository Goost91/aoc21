module AdventOfCode.Common

open System.Numerics


let lines : string -> seq<string> = System.IO.File.ReadLines

let readInts file =
    lines file  
    |> Seq.map (fun x -> int x)
    |> Seq.toList
    
let flatten (A:'a[,]) = A |> Seq.cast<'a>
let getRow r (A:_[,]) =
    flatten A.[r..r,*] |> Seq.toArray
    
let transposeWith parse (input : string list) =
    Array2D.init input.[0].Length input.Length (fun i j -> input.[j].[i] |> parse)
    
let inverse (x : int32) =
    ~~~x &&& ((1 <<< (32-BitOperations.LeadingZeroCount(uint x))) - 1)
    