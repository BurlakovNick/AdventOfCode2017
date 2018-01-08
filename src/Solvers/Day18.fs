module Day18

open System
open System.Linq

type Register = char
type Integer = int
type Argument = Register of Register | Integer of Integer

type Command = 
    | Set of Register * Argument
    | Add of Register * Argument
    | Mul of Register * Argument
    | Mod of Register * Argument
    | Snd of Register
    | Rcv of Register
    | Jgz of Register * Argument
    | Nop

type State = {
    lastPlayed: Integer;
    recovered: Integer;
    registers: Map<Register, Integer>;
    commands: Command[];
    currentCommand: int;
}
    
let parseArg (line: string) = 
    if Char.IsLetter line.[0] then Register line.[0]
    else Integer (Int32.Parse line)

let parseCommand (line: string) = 
    let tokens = line.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)
    let reg = tokens.[1].[0]
    let arg = if tokens.Length > 2 then Some (parseArg tokens.[2]) else None
    
    match tokens.[0] with
    | "set" -> Set(reg, arg.Value)
    | "add" -> Add(reg, arg.Value)
    | "mul" -> Mul(reg, arg.Value)
    | "mod" -> Mod(reg, arg.Value)
    | "snd" -> Snd(reg)
    | "rcv" -> Rcv(reg)
    | "jgz" -> Jgz(reg, arg.Value)
    | _ -> Nop 

let getRegister state reg = 
    if state.registers.ContainsKey reg then state.registers.Item reg
    else 0

let getValue state argument = 
    match argument with
    | Integer i -> i
    | Register reg -> getRegister state reg

let modifyRegister (state: State) reg arg operation = 
    let regValue = getRegister state reg
    let argValue = getValue state arg
    { 
        state with registers = state.registers.Add (reg, operation regValue argValue);
    }

let execute (state: State) = 
    let command = state.commands.[state.currentCommand]
    let modify = modifyRegister state

    let state = 
        match command with
        | Set (reg, arg) -> 
            modify reg arg (fun reg arg -> arg)
        
        | Add (reg, arg) -> 
            modify reg arg (fun reg arg -> reg + arg)
        
        | Mul (reg, arg) -> 
            modify reg arg (fun reg arg -> reg * arg)
            
        | Mod (reg, arg) ->
            modify reg arg (fun reg arg -> reg % arg)
        
        | Snd (reg) ->
            { 
                state with lastPlayed = getRegister state reg;
            }
        
        | Rcv (reg) ->
            let regValue = getRegister state reg
            if regValue <> 0 then { state with recovered = state.lastPlayed; }
            else state
            
        | Jgz (reg, arg) ->
            let regValue = getRegister state reg
            let argValue = getValue state arg
            if regValue > 0 then { state with currentCommand = state.currentCommand + argValue }
            else { state with currentCommand = state.currentCommand + 1 }
        
        | Nop -> state
    
    match command with
    | Jgz (_, _) -> state
    | _ -> { state with currentCommand = state.currentCommand + 1 }

let executeAll commands = 
    let initState = { lastPlayed = 0; recovered = 0; registers = Map.empty; commands = commands; currentCommand = 0; }
    
    Seq.initInfinite (fun _ -> ())
    |> Seq.scan (fun state _ -> execute state) initState
    |> Seq.skipWhile (fun state -> state.recovered = 0)
    |> Seq.head

let solveDuet (lines : seq<string>) =
    //old: 862
    let commands = lines |> Seq.map parseCommand |> Seq.toArray
    let state = executeAll commands
    state.recovered.ToString()
