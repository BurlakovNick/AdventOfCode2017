module Day15

open System
open System.Linq

let getNext (factor: UInt64) (prev: UInt64) (filter: UInt64) =
    Seq.initInfinite ignore
    |> Seq.scan (fun prev _ -> prev * factor % 2147483647UL) prev
    |> Seq.skip 1
    |> Seq.skipWhile (fun x -> x % filter <> 0UL)
    |> Seq.head

let generate (factor: UInt64) (seed: UInt64) (filter: UInt64) =
    Seq.initInfinite ignore
    |> Seq.scan (fun prev _ -> getNext factor prev filter) seed
    |> Seq.skip 1
    |> Seq.take 5_000_000

let generateA (seed: UInt64) =
    generate 16807UL seed 4UL

let generateB (seed: UInt64) =
    generate 48271UL seed 8UL

let getFirst16Bits (x: UInt64) =
    x &&& ((uint64) 0xFFFF)

let isMatch (x: UInt64) (y: UInt64) =
    (getFirst16Bits x) = (getFirst16Bits y)

let solveDuelingGenerators (lines : seq<string>) =
    let seedA = UInt64.Parse (lines.ElementAt 0)
    let seedB = UInt64.Parse (lines.ElementAt 1)

    let answer =
        Seq.zip (generateA seedA) (generateB seedB)
        |> Seq.filter (fun (x, y) -> isMatch x y)
        |> Seq.length

    answer.ToString()