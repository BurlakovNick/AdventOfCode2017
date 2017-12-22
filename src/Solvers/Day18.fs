module Day18

open System
open System.Linq

type Register = char
type Integer = int64
type Argument = Register of Register | Integer of Integer

type Command = 
    | Set of Register * Argument
    | Add of Register * Argument
    | Mul of Register * Argument
    | Mod of Register * Argument
    | Snd of Argument
    | Rcv of Argument
    | Jgz of Argument * Argument
    | Nop

type State = {
    registers: Map<Register, Integer>;
    commands: Command[];
    currentCommand: int;
    programId: int;
    waiting: bool;
    finished: bool;
    sentCount: int;
    sent: List<Integer>;
    received: List<Integer>;
}
    
let parseArg (line: string) = 
    if Char.IsLetter line.[0] then Register line.[0]
    else Integer (Int64.Parse line)

let toReg arg = 
    match arg with
    | Register reg -> Some reg
    | Integer i -> None

let parseCommand (line: string) = 
    let tokens = line.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)
    let arg1 = parseArg tokens.[1]
    let reg = toReg arg1
    let arg2 = if tokens.Length > 2 then Some (parseArg tokens.[2]) else None
    
    match tokens.[0] with
    | "set" -> Set(reg.Value, arg2.Value)
    | "add" -> Add(reg.Value, arg2.Value)
    | "mul" -> Mul(reg.Value, arg2.Value)
    | "mod" -> Mod(reg.Value, arg2.Value)
    | "snd" -> Snd(arg1)
    | "rcv" -> Rcv(arg1)
    | "jgz" -> Jgz(arg1, arg2.Value)
    | _ -> Nop 

let getRegister state reg = 
    if state.registers.ContainsKey reg then state.registers.Item reg
    else 0L

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
    if state.currentCommand = state.commands.Length then { state with finished = true; }
    else
    
    let command = state.commands.[state.currentCommand]
    let modify = modifyRegister state

    let state = 
        match command with
        | Set (reg, arg) -> 
            modify reg arg (fun reg arg -> arg)
        
        | Add (reg, arg) -> 
            modify reg arg (+)
        
        | Mul (reg, arg) -> 
            modify reg arg (*)
            
        | Mod (reg, arg) ->
            modify reg arg (%)
        
        | Snd (arg) ->
            { 
                state with 
                    sent = Seq.append state.sent [ getValue state arg ] |> Seq.toList;
                    sentCount = state.sentCount + 1
            }
        
        | Rcv (reg) ->
            if state.received.IsEmpty then { state with waiting = true; }
            else 
                { 
                    state with 
                        received = Seq.skip 1 state.received |> Seq.toList;
                        waiting = false;
                        currentCommand = state.currentCommand + 1;
                }
            
        | Jgz (arg1, arg2) ->
            let regValue = getValue state arg1
            let argValue = getValue state arg2
            if regValue > 0L then { state with currentCommand = state.currentCommand + int argValue }
            else { state with currentCommand = state.currentCommand + 1 }
        
        | Nop -> state

    //printfn "%A" command
    //printfn "%A" state
    //printfn ""

    match command with
    | Jgz _ -> state
    | Rcv _ -> state
    | _ -> { state with currentCommand = state.currentCommand + 1 }

let createInitState (programId: int) commands = 
    { 
        registers = Map.ofList [ ('p', (int64) programId) ];
        commands = commands;
        currentCommand = 0;
        programId = programId;
        sent = List.empty;
        sentCount = 0;
        received = List.empty;
        waiting = false;
        finished = false;
    }
    
let canExecute state = 
    state.waiting = false && state.finished = false

let tryExecuteOneStep (state0, state1) = 
    if canExecute state1 then
        let state1 = execute state1
        let state0 = 
            {
                state0 with
                    waiting = if state0.waiting then state1.sent.IsEmpty = false else false;
                    received = state1.sent;
            }
        (state0, state1)
    else
    if canExecute state0 then
        let state0 = execute state0
        let state1 = 
            {
                state1 with
                    waiting = if state1.waiting then state0.sent.IsEmpty = false else false;
                    received = state0.sent;
            }
        (state0, state1)
    else
        (state0, state1)

let executeBothPrograms commands = 
    let state0 = createInitState 0 commands
    let state1 = createInitState 1 commands
    
    let (state0, state1) = 
        Seq.initInfinite (fun _ -> ())
        |> Seq.scan (fun (x, y) _ -> tryExecuteOneStep (x, y)) (state0, state1)
        |> Seq.skipWhile (fun (x, y) -> canExecute x || canExecute y)
        |> Seq.head
        
    state1.sentCount

let solveDuet (lines : seq<string>) =
    //old: 127 - to low
    let commands = lines |> Seq.map parseCommand |> Seq.toArray
    let answer = executeBothPrograms commands
    answer.ToString()
