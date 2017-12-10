module Day6

open System
open System.Linq

let parseIntArray (line : string) =
    (line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse).ToArray()

type memoryState = { banks: int[] }

let getIndexWithMostBlocks memoryState =
    let max = Seq.max memoryState.banks
    Seq.findIndex (fun x -> x = max) memoryState.banks

let reallocate memoryState =
    let reallocateIndex = getIndexWithMostBlocks memoryState
    let reallocatedMemory = memoryState.banks.ElementAt reallocateIndex
    let memoryLength = memoryState.banks.Length

    let banks =
        [ 0 .. memoryLength - 1 ]
        |> Seq.map (fun i ->
            let distance = (i - reallocateIndex - 1 + memoryLength) % memoryLength
            let newMemory = reallocatedMemory / memoryLength
            let additionalMemory = if distance < reallocatedMemory % memoryLength then 1 else 0

            if i = reallocateIndex
            then
                newMemory + additionalMemory
            else
                let oldMemory = memoryState.banks.ElementAt i
                oldMemory + newMemory + additionalMemory
        )

    { banks = banks.ToArray() }

let infinity = [ while true do yield () ]

type reallocationState = { current: memoryState; visited: Map<memoryState, int>; cycledIndex: option<int>; currentIndex: int }

let countReallocationCycles memoryState =
    let cycledState =
        Seq.initInfinite (fun i -> i)
        |> Seq.scan (fun state i ->
            let newState = reallocate state.current
            {
                current = newState;
                visited = state.visited.Add(newState, state.currentIndex + 1);
                cycledIndex = state.visited.TryFind newState;
                currentIndex = state.currentIndex + 1
            }
        ) { current = memoryState; visited = Map.ofArray [| memoryState, 0 |]; cycledIndex = None; currentIndex = 0 }
        |> Seq.skipWhile (fun state -> state.cycledIndex.IsNone)
        |> Seq.head
    cycledState.currentIndex - cycledState.cycledIndex.Value

let solveMemoryReallocation (lines : seq<string>) =
    let memoryState = { banks = (Seq.head lines |> parseIntArray) }
    let answer = countReallocationCycles memoryState
    answer.ToString()
