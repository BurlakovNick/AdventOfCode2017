module Day14

open Day10_Hard
open System
open System.Linq

let toInt hex =
    if Char.IsDigit hex then (int) hex - (int) '0'
    else (int) hex - (int) 'a' + 10

let toBits hash =
    hash
    |> Seq.map (fun ch ->
        let x = toInt ch
        [ 0 .. 3 ]
        |> Seq.map (fun i ->
            let bit = x &&& (1 <<< (3 - i))
            if bit > 0 then 1 else 0
        )
    )
    |> Seq.concat

let generateMatrix input =
    let result =
        [0 .. 127]
        |> Seq.map (fun i -> input + "-" + i.ToString())
        |> Seq.map (fun str -> getKnotHash 256 str)
        |> Seq.map (fun hash -> (toBits hash).ToArray())
    result.ToArray()

let countBusy matrix =
    matrix
    |> Seq.concat
    |> Seq.filter (fun bit -> bit = 1)
    |> Seq.length

let adjacent = [ (-1, 0); (+1, 0); (0, -1); (0, +1) ]

let move (x, y) =
    adjacent
    |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))

let isInField (x, y) =
    0 <= x && x < 128 &&
    0 <= y && y < 128

let getCell (matrix: int[][]) (x, y) =
    (matrix.ElementAt x).ElementAt y

let rec visit (x, y) (matrix: int[][]) (visited: Set<int * int>) =
    if isInField (x, y) = false then visited
    else if getCell matrix (x, y) = 0 then visited
    else if visited.Contains((x, y)) then visited
    else
        let visited = visited.Add (x, y)

        move (x, y)
        |> Seq.fold (fun visited (x, y) ->
            visit (x, y) matrix visited
        ) visited

let countBusyRegions matrix =
    let visited = Set.empty

    let (visited, count) =
        Seq.allPairs [0 .. 127] [0 .. 127]
        |> Seq.filter (fun (x, y) -> getCell matrix (x, y) = 1)
        |> Seq.fold (fun (visited: Set<int * int>, count) (x, y) ->
            if visited.Contains((x, y)) then (visited, count)
            else (visit (x, y) matrix visited, count + 1)
        ) (visited, 0)

    count

let solveDiskFragmentation (lines : seq<string>) =
    let input = lines |> Seq.head
    let matrix = (generateMatrix input)

    let answer = countBusyRegions matrix

    answer.ToString()