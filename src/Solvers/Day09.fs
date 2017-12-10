module Day9

open System
open System.Linq

type stream = string

type ignoreFilterState =
    | SkipNextChar
    | TakeNextChar

let markExclamations state char =
    match state with
    | SkipNextChar -> (TakeNextChar, false)
    | TakeNextChar ->
        match char with
        | '!' -> (SkipNextChar, false)
        | _ -> (TakeNextChar, true)

type garbageFilterState =
    | Garbage
    | NotGarbage

let markGarbage state char =
    match state with
    | Garbage ->
        match char with
        | '>' -> (NotGarbage, false)
        | _ -> (Garbage, false)
    | NotGarbage ->
        match char with
        | '<' -> (Garbage, false)
        | _ -> (NotGarbage, true)

let filterChars filter initialState stream =
    stream
    |> Seq.scan (fun (state, _, isValid) char ->
           let (state, isValid) = filter state char
           (state, char, isValid)
       ) (initialState, '@', false)
    |> Seq.filter (fun (_, char, isValid) -> isValid)
    |> Seq.map (fun (_, char, _) -> char)
    |> String.Concat

let filterExclamations stream = filterChars markExclamations TakeNextChar stream
let filterGarbage stream = filterChars markGarbage NotGarbage stream

let filterIgnoredChars stream =
    stream
    |> filterExclamations
    |> filterGarbage

let calculateGarbage stream =
    let stream = stream |> filterExclamations
    let (state, garbage) =
        stream
        |> Seq.fold (fun (state, garbage) char ->
            match state with
            | Garbage ->
                match char with
                | '>' -> (NotGarbage, garbage)
                | _ -> (Garbage, garbage + 1)
            | NotGarbage ->
                match char with
                | '<' -> (Garbage, garbage)
                | _ -> (NotGarbage, garbage)
        ) (NotGarbage, 0)
    garbage

let calculateScore line =
    let (balance, score) =
        line
        |> Seq.fold (fun (balance, score) char ->
            match char with
            | '{' -> (balance + 1, score + balance + 1)
            | '}' -> (balance - 1, score)
            | _ -> (balance, score)
        ) (0, 0)
    score

let solveStreamProcessing (lines : seq<string>) =
    for line in lines do
        let garbage = line |> calculateGarbage
        let score = line |> filterIgnoredChars |> calculateScore
        printfn "Garbage = %i" garbage
        printfn "Score = %i" score
    "That's all, folks!"
