module Day17

open System
open System.Linq

let generate stepCount = 
    let seq = [0] |> Seq.toArray
    
    [1 .. 2017]
    |> Seq.fold (fun (seq, pos) i -> 
        let length = i
        let index = (pos + stepCount) % length + 1
        let left = Seq.take index seq
        let middle = Seq.ofList [ i ]
        let right = Seq.skip index seq
        let seq = Seq.concat [left; middle; right ] |> Seq.toArray
        (seq, index)
    ) (seq, 0)

let generate2 stepCount = 
    let answer = -1
    
    seq { 1 .. 50_000_000 }
    |> Seq.fold (fun (answer, pos) i -> 
        let length = i
        let index = (pos + stepCount) % length + 1
        if index = 1 then (i, index)
        else (answer, index)
    ) (answer, 0)
    
let solveSpinlock (lines : seq<string>) =
    let stepCount = Int32.Parse (lines |> Seq.head)
    let (answer, index) = generate2 stepCount
    answer.ToString()