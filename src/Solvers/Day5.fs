module Day5

open System
open System.Linq

let doJumps (jumps: int[]) =
    let mutable position = 0
    [
        while 0 <= position && position < jumps.Length do
            let jumpLength = jumps.ElementAt position
            jumps.SetValue(jumpLength + 1, position)
    ]

let rec countJumps (jumps: int[]) =
    let (position, distance) =
        infinity
        |> Seq.map (fun position _ ->
             let jumpLength = jumps.ElementAt position
             jumps.SetValue(jumpLength + 1, position)
             position + jumpLength
        ) 0
        |> Seq.takeWhile (fun position -> position >= 0 && position < jumps.Length)
    distance

let solveJumps (lines : seq<string>) =
    let jumps = Array.ofSeq (Seq.map Int32.Parse lines)
    let answer = countJumps jumps
    answer.ToString()
