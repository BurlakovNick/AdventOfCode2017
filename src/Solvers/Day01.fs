module Day1

open System
open System.Linq

let inline charToInt (c : char) = Convert.ToInt32(c) - int '0'

type circularBuffer =
    { values : int [] }

let getNext buffer position =
    let position = (position + buffer.values.Length / 2) % buffer.values.Length
    buffer.values.ElementAt position

let aggregateSum digits seed position =
    let current = digits.values.ElementAt position
    let next = getNext digits position
    if current = next then seed + current
    else seed

let calculateSum (digits : circularBuffer) =
    [ 0..digits.values.Length - 1 ]
    |> Seq.fold (fun sum position -> aggregateSum digits sum position) 0

let solveInverseCaptcha lines =
    let captcha = Seq.head lines
    let digits = { values = (captcha |> Seq.map charToInt).ToArray() }
    let answer = calculateSum digits
    answer.ToString()
