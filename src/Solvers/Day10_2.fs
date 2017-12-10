module Day10_Hard

open System
open System.Linq

let getLengthList (line : string) =
    let list = line |> Seq.map (fun c -> (int) c)
    Seq.append list [| 17; 31; 73; 47; 23 |]

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
    let lengthList =
        lengthList
        |> Seq.replicate 64
        |> Seq.concat

    let cycleLength = Seq.length list
    let (list, position, skipSize) =
        lengthList
        |> Seq.fold (fun (list, position, skipSize) length ->
            let tiedList = tie list position skipSize length
            (tiedList.ToArray(), (position + length + skipSize) % cycleLength, skipSize + 1)
        ) (list, 0, 0)
    list

let getHexadecimal x =
    if x < 10 then '0' + (char) x
    else 'a' + (char) (x - 10)

let getHash list =
    list
    |> Seq.chunkBySize 16
    |> Seq.map (fun chunk -> Seq.fold (fun xor x -> xor ^^^ x) 0 chunk)
    |> Seq.map (fun hash -> String.Concat( [| getHexadecimal (hash / 16); getHexadecimal (hash % 16) |] ))
    |> String.Concat

let solveKnotHash (lines : seq<string>) =
    let listSize = Int32.Parse (lines.ElementAt 0)
    if (listSize <> 256)
    then "42"
    else
        let line = if Seq.length lines > 1 then lines.ElementAt 1 else String.Empty
        let lengthList = getLengthList line
        let list = [ 0 .. listSize - 1 ].ToArray()
        let knot = tieKnot list lengthList
        let hash = getHash knot
        hash.ToString()