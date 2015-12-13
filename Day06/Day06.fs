open System
open System.IO

type Opcode = TurnOn | TurnOff | Toggle
with
    member op.Transform =
        match op with
        | TurnOn -> (fun _ -> true)
        | TurnOff -> (fun _ -> false)
        | Toggle -> (fun b -> not b)

type Point = int * int

type Rectangle = { Corners : Point * Point }
with
    member r.Indexes =
        let (px,py),(qx,qy) = r.Corners
        seq { for x in px .. qx do for y in py .. qy do yield x, y }

type Grid = { Array : bool [,] }
with
    static member Start =
        { Array = Array2D.init 1000 1000 (fun _ _ -> false) }
    member g.CountSet =
        let mutable count = 0
        Array2D.iter (fun item -> if item then count <- count + 1) g.Array
        count

type Instruction = { Op : Opcode; Rect : Rectangle }

let challenge1 input =
    let grid = Grid.Start
    for instr in input do
        for (i,j) in instr.Rect.Indexes do
            grid.Array.[i,j] <- instr.Op.Transform grid.Array.[i,j]
    grid.CountSet

module Parse =
    let tryParse parse = parse >> function
        | true, v -> Some v
        | false, _ -> None
    
    let int = tryParse Int32.TryParse

module Input =
    let readPoint (s : string) =
        match s.Split(',') with
        | [| x; y |] ->
            match Parse.int x, Parse.int y with
            | Some x, Some y when 0 <= x && x < 1000 && 0 <= y && y < 1000 ->
                Some (x, y)
            | _ -> None
        | _ -> None

    let readInstruction (s : string) =
        let parsedInstr =
            match s.Split(' ') with
            | [| "turn"; "on"; p; "through"; q |] -> Some (TurnOn, readPoint p, readPoint q) 
            | [| "turn"; "off"; p; "through"; q |] -> Some (TurnOff, readPoint p, readPoint q)
            | [| "toggle"; p; "through"; q |] -> Some (Toggle, readPoint p, readPoint q)
            | _ -> None
        match parsedInstr with
        | Some (op, Some p, Some q) -> Some { Op = op; Rect = { Corners = p, q } }
        | _ -> None

    let readInput =
        seq { use r = new StreamReader("input.txt")
            while not r.EndOfStream do yield r.ReadLine() }
        |> Seq.map readInstruction
        |> Seq.choose (fun x -> x)

[<EntryPoint>]
let main args =
    let input = Input.readInput
    challenge1 input |> printfn "There are %d lights lit"
    0