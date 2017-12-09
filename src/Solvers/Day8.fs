module Day8

open System
open System.Linq

type memory = Map<string, int>

type command = {
    register: string;
    incValue: int;
    compareRegister: string;
    comparePredicate: int -> bool
}

let parseCommand (line: string) =
    let separators = [| " "; "\t" |]
    let tokens = line.Split(separators, StringSplitOptions.RemoveEmptyEntries)

    let value = Int32.Parse (tokens.ElementAt 2)
    let incValue =
        match tokens.ElementAt 1 with
        | "inc" -> value
        | "dec" -> -value
        | _ -> 0

    let compareArgument = Int32.Parse (tokens.ElementAt 6)
    let comparePredicate x =
        match tokens.ElementAt 5 with
        | "<" -> x < compareArgument
        | "<=" -> x <= compareArgument
        | ">" -> x > compareArgument
        | ">=" -> x >= compareArgument
        | "==" -> x = compareArgument
        | "!=" -> x <> compareArgument
        | _ -> true

    {
        register = tokens.ElementAt 0;
        incValue = incValue;
        compareRegister = tokens.ElementAt 4;
        comparePredicate = comparePredicate
    }

let getRegisterValue (memory: memory) register =
    match memory.TryFind register with
    | None -> 0
    | Some x -> x

let setRegisterValue (memory: memory) register value =
    memory.Add(register, value)

let executeCommand (memory: memory) (command: command) =
    let compareRegisterValue = getRegisterValue memory command.compareRegister
    match command.comparePredicate compareRegisterValue with
    | false -> memory
    | true ->
        let oldValue = getRegisterValue memory command.register
        let newValue = oldValue + command.incValue
        setRegisterValue memory command.register newValue

let getMaximumValue (memory: memory) =
    match memory.IsEmpty with
    | true -> 0
    | false -> memory |> Seq.map (fun x -> x.Value) |> Seq.max

let executeCommands commands =
    let initialMemory = Map.ofArray [| |]
    commands
    |> Seq.fold (fun (memory, max) command ->
        let memory = executeCommand memory command
        let max = Math.Max((getMaximumValue memory), max)
        (memory, max)
    ) (initialMemory, 0)

let solveILikeRegisters (lines : seq<string>) =
    let commands = lines |> Seq.map parseCommand
    let (memory, maxValue) = executeCommands commands
    maxValue.ToString()