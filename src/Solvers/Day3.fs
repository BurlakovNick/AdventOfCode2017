module Day3

open System
open System.Collections
open System.Linq

type direction = 
    | Right
    | Up
    | Left
    | Down

type vector = 
    { name : direction
      dx : int
      dy : int }

let directions = 
    [| { name = Right
         dx = 1
         dy = 0 }
       { name = Up
         dx = 0
         dy = 1 }
       { name = Left
         dx = -1
         dy = 0 }
       { name = Down
         dx = 0
         dy = -1 } |]

let getNextDirection direction = 
    let name = 
        match direction.name with
        | Right -> Up
        | Up -> Left
        | Left -> Down
        | Down -> Right
    directions
    |> Seq.filter (fun x -> x.name = name)
    |> Seq.exactlyOne

let add point direction = 
    let (x, y) = point
    (x + direction.dx, y + direction.dy)

let getNextPoint lastPoint direction (points : Map<int * int, int>) = 
    let nextPoint = add lastPoint direction
    let nextDirection = getNextDirection direction
    if points.ContainsKey(add nextPoint nextDirection) <> true then (nextPoint, nextDirection)
    else (nextPoint, direction)

let getNextNumber lastNumber nextPoint (points : Map<int * int, int>) = 
    let (x, y) = nextPoint
    Seq.allPairs [ -1..1 ] [ -1..1 ]
    |> Seq.sumBy (fun (dx, dy) -> 
        let number = points.TryFind((x + dx, y + dy))
        match number with
        | None -> 0
        | Some x -> x
    )

let rec getPoint targetNumber lastNumber lastPoint direction (points : Map<int * int, int>) = 
    printfn "%A -> %i" lastPoint lastNumber
    if lastNumber > targetNumber 
    then
        lastPoint
    else 
        let (nextPoint, nextDirection) = getNextPoint lastPoint direction points
        let nextNumber = getNextNumber lastNumber nextPoint points
        let nextPoints = points.Add(nextPoint, nextNumber)
        getPoint targetNumber nextNumber nextPoint nextDirection nextPoints

let getTargetPoint number = 
    let points = Map.ofSeq [ ((0, 0), 1) ]
    let initialDirection = Seq.head directions
    getPoint number 1 (0, 0) initialDirection points

let solveSpiralMemory (lines : seq<string>) = 
    let line = Seq.head lines
    let number = Int32.Parse line
    let (x, y) = getTargetPoint number
    let distance = Math.Abs x + Math.Abs y
    distance.ToString()
