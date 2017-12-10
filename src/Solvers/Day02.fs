module Day2

open System
open System.Linq

let parseIntArray (line : string) =
    (line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse)

type table =
    { values : seq<seq<int>> }

let calcRowChecksum ints =
    let min = Seq.min ints
    let max = Seq.max ints
    max - min

let calcRowChecksum2 (ints : seq<int>) =
    Seq.allPairs ints ints
    |> Seq.map (fun (x, y) ->
           if x % y = 0 then x / y
           else 1)
    |> Seq.max

let calcChecksum table =
    table.values
    |> Seq.map calcRowChecksum2
    |> Seq.sum

let solveCorruptionChecksum (lines : seq<string>) =
    let table = { values = (lines |> Seq.map parseIntArray).ToArray() }
    let answer = calcChecksum table
    answer.ToString()
