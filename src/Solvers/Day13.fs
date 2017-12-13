module Day13

open System
open System.Linq

let parseScanner (line : string) =
    let tokens = line.Split([| ": " |], StringSplitOptions.RemoveEmptyEntries)
    let position = Int32.Parse (tokens.ElementAt 0)
    let range = Int32.Parse (tokens.ElementAt 1)
    (position, range)

let solvePacketScanners (lines : seq<string>) =
    let scanners = lines |> Seq.map parseScanner
    let severity =
        scanners
        |> Seq.filter (fun (position, range) -> range = 1 || position % ((range - 1) * 2) = 0)
        |> Seq.sumBy (fun (position, range) -> position * range)
    severity.ToString()