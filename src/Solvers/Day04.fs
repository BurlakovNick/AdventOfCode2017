module Day4

open System
open System.Linq

let parseStringArray (line : string) =
    line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries).ToArray()

let isValidPassword (password: string[]) =
    let uniqueCount = password |> Seq.map (fun p -> String.Concat (Seq.sort p)) |> Seq.distinct |> Seq.length
    uniqueCount = password.Length

let countValidPasswords passwords =
    passwords |> Seq.filter isValidPassword |> Seq.length

let solveHighEntropyPassphrases (lines : seq<string>) =
    let passwords = Seq.map parseStringArray lines
    let answer = countValidPasswords passwords
    answer.ToString()
