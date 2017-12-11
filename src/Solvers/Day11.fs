module Day11

open System
open System.Linq

let parseSteps (line : string) =
    line.Split([| ','; ' ' |], StringSplitOptions.RemoveEmptyEntries)

let toVector direction =
    match direction with
    | "n"  -> ( 0, -1)
    | "s"  -> ( 0,  1)
    | "nw" -> (-1,  0)
    | "ne" -> ( 1, -1)
    | "sw" -> (-1,  1)
    | "se" -> ( 1,  0)
    | _    -> ( 0,  0)

let add (x, y) (dx, dy) = (x + dx, y + dy)

let toCube (x, y) = (x, -x-y, y)

let getDist3 (x, y, z) = [| (int)x; y; z; |] |> Seq.map Math.Abs |> Seq.max

let getDist2 (x, y) = (x, y) |> toCube |> getDist3

let getDistance steps =
    steps
    |> Seq.map toVector
    |> Seq.scan add (0, 0)
    |> Seq.map getDist2
    |> Seq.max

let solveHexEd (lines : seq<string>) =
    let steps = lines |> Seq.head |> parseSteps
    let distance = getDistance steps
    distance.ToString()