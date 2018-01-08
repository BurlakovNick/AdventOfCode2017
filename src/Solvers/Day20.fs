module Day20

open System
open System.Linq

type Vec = {
    x: int64;
    y: int64;
    z: int64;
} with
    member this.getDistToZero = (Math.Abs this.x) + (Math.Abs this.y) + (Math.Abs this.z)
    static member (*) (v : Vec, k) = { x = v.x * k; y = v.y * k; z = v.z * k }
    static member (+) (a: Vec, b: Vec) = { x = a.x + b.x; y = a.y + b.y; z = a.z + b.z }
    static member (-) (a: Vec, b: Vec) = { x = a.x - b.x; y = a.y - b.y; z = a.z - b.z }

type particle = { p: Vec; v: Vec; a: Vec; }

let parseVec (line: string) = 
    let coords = line.Split([| "=<"; ","; ">" |], StringSplitOptions.RemoveEmptyEntries)
    let x = Int64.Parse coords.[1]
    let y = Int64.Parse coords.[2]
    let z = Int64.Parse coords.[3]
    { x = x; y = y; z = z; }

let parse (line: string) = 
    let tokens = line.Split([| ", " |], StringSplitOptions.RemoveEmptyEntries);
    let p = parseVec tokens.[0]
    let v = parseVec tokens.[1]
    let a = parseVec tokens.[2]
    { p = p; v = v; a = a; }

let timeTravel (time: int64) particle = 
    {
        p = particle.p + particle.v * time + particle.a * ((time + 1L) * time / 2L);
        v = particle.v + particle.a * time;
        a = particle.a; 
    }
    
let move = timeTravel 1L

let findClosestToZeroIndex particles = 
    let (index, _) = 
        particles
        |> Seq.map (timeTravel 1_000_000L)
        |> Seq.mapi (fun i p -> (i, p))
        |> Seq.sortBy (fun (_, particle) -> particle.p.getDistToZero)
        |> Seq.head
    index

let eliminateCollisions particles = 
    particles
    |> Seq.groupBy (fun particle -> particle.p)
    |> Seq.filter (fun (_, group) -> (group |> Seq.length) = 1)
    |> Seq.collect (fun (_, group) -> group)
    |> Seq.toArray
    
let moveAll particles = particles |> Seq.map move |> Seq.toArray

let countAlive particles =
    [0 .. 1000]
    |> Seq.fold (fun particles _ -> particles |> moveAll |> eliminateCollisions) particles
    |> Seq.length

let solveParticleSwarm (lines : seq<string>) =
    let particles = lines |> Seq.map parse |> Seq.toArray
    let answer = countAlive particles
    answer.ToString()
