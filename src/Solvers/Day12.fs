module Day12

open System
open System.Linq

let parseEdges (line : string) =
    let tokens = line.Split([| "<->"; ","; " " |], StringSplitOptions.RemoveEmptyEntries)
    let sourceNode = Int32.Parse (tokens.ElementAt 0)
    let targetNodes = (tokens |> Seq.skip 1 |> Seq.map Int32.Parse).ToArray()
    (sourceNode, targetNodes)

let rec dfs node (edgesByNode: Map<int, int[]>) (visited: Set<int>) =
    let connectedNodes = edgesByNode.Item node
    let visited = visited.Add node

    let (visited, count) =
        connectedNodes
        |> Seq.fold (fun (visited: Set<int>, count) node ->
            let isVisited = visited.Contains node
            match isVisited with
            | true -> (visited, count)
            | false -> let (visited, newCount) = dfs node edgesByNode visited
                       (visited, newCount + count)
        ) (visited, 0)

    (visited, count + 1)

let solveDigitalPlumber (lines : seq<string>) =
    let edgesByNode = lines |> Seq.map parseEdges |> Map.ofSeq
    let nodes = edgesByNode |> Seq.map (fun x -> x.Key)
    let (visited, count) =
        nodes
        |> Seq.fold (fun (visited: Set<int>, componentsCount) node ->
            let isVisited = visited.Contains node
            match isVisited with
            | true -> (visited, componentsCount)
            | false -> let (visited, _) = dfs node edgesByNode visited
                       (visited, componentsCount + 1)
        ) (Set.empty, 0)

    count.ToString()