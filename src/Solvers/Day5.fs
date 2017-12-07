module Day5

open System
open System.Linq

let rec countJumps (jumps: int[]) =
    let (position, distance) =
        Seq.initInfinite (fun i -> i)
        |> Seq.scan (fun (position, distance) _ ->
             let jumpLength = jumps.ElementAt position
             let newJumpLength = if jumpLength >= 3 then jumpLength - 1 else jumpLength + 1
             jumps.SetValue(newJumpLength, position)
             (position + jumpLength, distance + 1)
        ) (0, 0)
        |> Seq.takeWhile (fun (position, distance) -> position >= 0 && position < jumps.Length)
        |> Seq.last
    distance + 1

let solveJumps (lines : seq<string>) =
    let jumps = Array.ofSeq (Seq.map Int32.Parse lines)
    let answer = countJumps jumps
    answer.ToString()
