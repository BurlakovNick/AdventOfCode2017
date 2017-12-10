module Day10

open System
open System.Linq

let parseIntArray (line : string) =
    (line.Split([| ','; ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse).ToArray()

let shiftLeft list position =
    let left = Seq.skip position list
    let right = Seq.take position list
    Seq.append left right

let shiftRight list position =
    shiftLeft list (Seq.length list - position)

let tie list position skipSize length =
    let list = shiftLeft list position

    let reversedPart =
        list
        |> Seq.take length
        |> Seq.rev

    let otherPart =
        list
        |> Seq.skip length

    shiftRight (Seq.append reversedPart otherPart) position

let tieKnot list lengthList =
    let cycleLength = Seq.length list
    let (list, position, skipSize) =
        lengthList
        |> Seq.fold (fun (list, position, skipSize) length ->
            let tiedList = tie list position skipSize length
            (tiedList.ToArray(), (position + length + skipSize) % cycleLength, skipSize + 1)
        ) (list, 0, 0)
    list

let solveKnotHash (lines : seq<string>) =
    let listSize = Int32.Parse (lines.ElementAt 0)
    let lengthList = parseIntArray (lines.ElementAt 1)
    let list = [ 0 .. listSize - 1 ].ToArray()
    let knot = tieKnot list lengthList
    let hash = (knot.ElementAt 0) * (knot.ElementAt 1)
    hash.ToString()