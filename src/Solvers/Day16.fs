module Day16

open System
open System.Linq

type DanceMove =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

let parseSpin (line : string) =
    Spin (Int32.Parse (line.Substring(1)))

let parseExchange (line : string) =
    let tokens = line.Substring(1).Split('/')
    Exchange (Int32.Parse (tokens.[0]), Int32.Parse (tokens.[1]))

let parsePartner (line : string) =
    let tokens = line.Substring(1).Split('/')
    Partner (tokens.[0].[0], tokens.[1].[0])

let parseMove (line : string) =
    if line.StartsWith "s" then parseSpin line
    else if line.StartsWith "x" then parseExchange line
    else parsePartner line

let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

let dance initState moves = 
    let length = initState |> Seq.length
    
    moves
    |> Seq.fold (fun state move -> 
        match move with
        | Spin x -> 
            let first = state |> Seq.take (length - x)
            let second = state |> Seq.skip (length - x)
            Seq.append second first |> Seq.toArray
        | Exchange (a, b) -> 
            swap state a b
            state.ToArray()
        | Partner (a, b) ->
            let posa = state |> Seq.findIndex (fun x -> x = a)
            let posb = state |> Seq.findIndex (fun x -> x = b)
            swap state posa posb
            state.ToArray()
    ) initState

let findCycleLength initState moves = 
    let visited = Map.empty

    seq { 1 .. 1_000_000_000 }
    |> Seq.scan (fun (state: char[], visited: Map<string, int>, isCycle) i -> 
        let str = String.Concat state
        let old = visited.TryFind str
        
        if old.IsSome then
            (dance state moves, visited, true)
        else        
            let visited = visited.Add(str, i)
            (dance state moves, visited, false)
    ) (initState, visited, false)
    |> Seq.takeWhile (fun (_, _, isCycle) -> isCycle = false)
    |> Seq.length

let solvePermutationPromenade (lines : seq<string>) =
    let initState = lines |> Seq.head
    let moveStrings = (lines |> Seq.skip 1 |> Seq.head).Split(',')
    let moves = moveStrings |> Seq.map parseMove
    
    let cycleLength = findCycleLength (initState.ToArray()) moves
    
    let answer = 
        seq { 1 .. (1_000_000_000 % (cycleLength - 1)) }
        |> Seq.fold (fun state _ -> 
            dance state moves
        ) (initState.ToArray())

    String.Concat answer