open System
open System.IO

type Direction = North | South | East | West

let readDirection = function
    | '^' -> Some North
    | 'v' -> Some South
    | '>' -> Some East
    | '<' -> Some West
    | _ -> None

type Position = { X : int; Y : int }
with
    static member Zero = { X = 0; Y = 0 }

let step pos direction =
    match direction with 
    | North -> { pos with Y = pos.Y + 1 }
    | South -> { pos with Y = pos.Y - 1 }
    | East -> { pos with X = pos.X + 1 }
    | West -> { pos with X = pos.X - 1 }

type World = { Santa : Position; RoboSanta : Position; Houses : Set<Position> }
with
    static member Start =
        {
            Santa = Position.Zero;
            RoboSanta = Position.Zero;
            Houses = Set.singleton Position.Zero
        }

type Visitor = Santa | RoboSanta

let stepWorld visitor world direction =
    let pos =
        match visitor with
        | Santa -> world.Santa
        | RoboSanta -> world.RoboSanta
    let nextPos = step pos direction
    let nextHouses = Set.add nextPos world.Houses
    match visitor with
    | Santa -> { world with Santa = nextPos; Houses = nextHouses }
    | RoboSanta -> { world with RoboSanta = nextPos; Houses = nextHouses }

let challenge1 directions =
    let stepSanta = stepWorld Santa
    let finalWorld = Seq.fold stepSanta World.Start directions
    finalWorld.Houses.Count

let challenge2 directions =
    let stepVisitor i =
        if i % 2 = 0 then stepWorld Santa
        else stepWorld RoboSanta
    let finalWorld =
        directions
        |> Seq.zip (Seq.initInfinite stepVisitor)
        |> Seq.fold (fun world (visit,dir) -> visit world dir) World.Start 
    finalWorld.Houses.Count

module Input =
    let directions =
        use sr = new StreamReader("input.txt")
        sr.ReadLine()
        |> Seq.map readDirection
        |> Seq.choose (fun x -> x)

[<EntryPoint>]
let main args =
    let directions = Input.directions
    challenge1 directions |> printfn "Santa visited %d house(s)"
    challenge2 directions |> printfn "Santa and Robo-Santa visited %d house(s)"
    0