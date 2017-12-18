﻿module Program

open Day16
open System
open System.IO
open System.Linq

let notEmpty str = str <> String.Empty

let readLines filePath =
    File.ReadAllLines(filePath)
    |> Seq.filter notEmpty

let getInputFiles problem =
    Directory.GetFiles("Tests\\" + problem, "*.in")

let getOutput problem id =
    let filename = "Tests\\" + problem + "\\" + id + ".out"
    if File.Exists(filename)
        then readLines filename
    else
        Seq.empty

let solve (lines: seq<string>) = solvePermutationPromenade lines

[<EntryPoint>]
let main argv =
    let problem = "day_16"
    let inputFiles = getInputFiles problem
    for inputFile in inputFiles do
        let input = readLines inputFile
        let output = solve input

//todo: pattern matching for checking
        printfn "Result for test %s" inputFile
        printfn "%s" output

    0
