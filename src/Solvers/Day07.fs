module Day7

open System
open System.Linq

type node = { name: string; weight: int; childs: string[] }
type nodeMap = Map<string, node>
type weightMap = Map<string, int>

let parseNode (line: string) =
    let separators = [| " "; "("; ")"; "->"; "," |]
    let tokens = line.Split(separators, StringSplitOptions.RemoveEmptyEntries)
    {
        name = tokens.ElementAt 0;
        weight = Int32.Parse (tokens.ElementAt 1);
        childs = (tokens |> Seq.skip 2).ToArray()
    }

let buildNodeMap nodes =
    nodes
    |> Seq.map (fun x -> (x.name, x))
    |> Map.ofSeq

let findRoot (nodes: seq<node>) (nodeMap: nodeMap) =
    let childs = nodes |> Seq.collect (fun x -> x.childs) |> Seq.distinct
    let rootName =
        nodes
        |> Seq.map (fun x -> x.name)
        |> Seq.except childs
        |> Seq.exactlyOne
    (nodeMap.TryFind rootName).Value

let selectChilds root (nodeMap: nodeMap) =
    root.childs |> Seq.map (fun child -> (nodeMap.TryFind child).Value)

type dfsResult = { answer: option<int>; weightSum: int; rootWeight: int }

let allSame items = (items |> Seq.distinct |> Seq.length) <= 1

let tryFindAnswer childResults =
    let weights = childResults |> Seq.map (fun childResult -> childResult.weightSum)

    if allSame weights then None
    else
        let wrongWeight =
            weights
            |> Seq.groupBy (fun x -> x)
            |> Seq.filter (fun (key, values) -> (Seq.length values) = 1)
            |> Seq.map (fun (key, values) -> key)
            |> Seq.head

        let rightWeight =
            weights.Except [| wrongWeight |]
            |> Seq.head

        let wrongChild = childResults |> Seq.find (fun child -> child.weightSum = wrongWeight)
        Some (wrongChild.rootWeight + (rightWeight - wrongWeight))


let rec dfs root (nodeMap: nodeMap) =
    let childs = selectChilds root nodeMap
    let childResults = childs |> Seq.map (fun child -> dfs child nodeMap)

    let weights = childResults |> Seq.map (fun childResult -> childResult.weightSum)
    let weightsSum = Seq.sum weights

    let childAnswer = childResults |> Seq.tryFind (fun childResult -> childResult.answer.IsSome)

    let answer =
        if childAnswer.IsSome then childAnswer.Value.answer
        else tryFindAnswer childResults

    {
        answer = answer;
        weightSum = root.weight + weightsSum;
        rootWeight = root.weight
    }

let solveRecursiveCircus (lines : seq<string>) =
    let nodes = lines |> Seq.map parseNode
    let nodeMap = buildNodeMap nodes
    let root = findRoot nodes nodeMap
    let answer = dfs root nodeMap
    answer.answer.Value.ToString()