module Day19

open System
open System.Linq

type vec = { x: int; y: int }

let findFirstCell (field: char[][]) = 
    let y = field.[0] |> Seq.findIndex (fun c -> c <> ' ')
    { x = 0; y = y }

type state = { cur: vec; prev: vec; stop: bool; path: string; steps: int }

let add (a: vec) (b: vec) = { x = a.x + b.x; y = a.y + b.y }
let sub (a: vec) (b: vec) = { x = a.x - b.x; y = a.y - b.y }

let isInField (v: vec) (field: char[][]) = 
    0 <= v.x && v.x < field.Length &&
    0 <= v.y && v.y < field.[0].Length

let getNeighbors (v: vec) (field: char[][]) = 
    [ 
        { x = -1; y = 0 };
        { x = +1; y = 0 };
        { x = 0; y = -1 };
        { x = 0; y = +1 }
    ]
    |> Seq.map (fun delta -> add v delta)
    |> Seq.filter (fun v -> (isInField v field) && field.[v.x].[v.y] <> ' ')

let move (prev: vec) (pos: vec) (field: char[][]) = 
    let delta = sub pos prev
    let ch = field.[pos.x].[pos.y]
    
    if ch <> '+' then 
        let next = add pos delta
        if field.[next.x].[next.y] = ' ' then None
        else if isInField next field then Some(next)
        else None
    else
        getNeighbors pos field
        |> Seq.filter (fun v -> v <> prev)
        |> Seq.tryHead

let getNextState (state: state) (field: char[][]) = 
    let next = move state.prev state.cur field

    if next.IsNone then { state with stop = true }
    else 
        let nextChar = field.[next.Value.x].[next.Value.y]
        let nextPath = 
            if Char.IsLetter nextChar then state.path + nextChar.ToString()
            else state.path
        
        {
            prev = state.cur;
            cur = next.Value;
            stop = false;
            path = nextPath;
            steps = state.steps + 1;
        }

let getPath (field: char[][]) (start: vec) = 

    let initState = { cur = start; prev = { start with x = -1}; stop = false; path = ""; steps = 1; }
    
    let endState = 
        Seq.initInfinite (fun _ -> ())
        |> Seq.scan (fun (state: state) _ -> getNextState state field) initState
        |> Seq.find (fun state -> state.stop)
        
    endState.steps

let solveSeriesOfTubes (lines : seq<string>) =
    let field = lines |> Seq.map (fun line -> line.ToArray()) |> Seq.toArray
    let start = findFirstCell field
    let answer = getPath field start
    answer.ToString()
