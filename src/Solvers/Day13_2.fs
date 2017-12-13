module Day13_Hard

open System
open System.Linq

let parseScanner (line : string) =
    let tokens = line.Split([| ": " |], StringSplitOptions.RemoveEmptyEntries)
    let position = Int32.Parse (tokens.ElementAt 0)
    let range = Int32.Parse (tokens.ElementAt 1)
    (position, range)

let isSafe delay scanners =
    scanners
    |> Seq.filter (fun (position, range) -> range = 1 || (position + delay) % ((range - 1) * 2) = 0)
    |> Seq.isEmpty

let solvePacketScanners (lines : seq<string>) =
    let scanners = (lines |> Seq.map parseScanner).ToArray()
    let delay =
        [0 .. 10000000]
        |> Seq.filter (fun delay -> isSafe delay scanners)
        |> Seq.head

    delay.ToString()