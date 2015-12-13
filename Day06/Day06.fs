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
        let (p1,p2),(q1,q2) = r.Corners
        seq { for p in p1 .. q1 do for q in p2 .. q2 do yield p, q }

type Grid = { Array : bool [,] }
with
    static member Start =
        { Array = Array2D.init 1000 1000 (fun _ _ -> false) }
    member g.CountSet =
        let mutable count = 0
        Array2D.iter (fun item -> if item then count <- count + 1) g.Array
        count

type Instruction = { Op : Opcode; Rect : Rectangle }

let private execute grid instr =
    let arr = grid.Array
    Seq.iter (fun (i,j) -> arr.[i,j] <- instr.Op.Transform arr.[i,j]) instr.Rect.Indexes
    { Array = arr }

let challenge1 input =
    let finalGrid = input |> Seq.fold (fun g instr -> execute g instr) Grid.Start
    finalGrid.CountSet

module Parse =
    let tryParse parse = parse >> function
        | true, v -> Some v
        | false, _ -> None
    
    let int = tryParse Int32.TryParse

module Input =
    let readPoint (s : string) =
        match s.Split(',') with
        | [| p1; p2 |] ->
            match Parse.int p1, Parse.int p2 with
            | Some p1, Some p2 when 0 <= p1 && p1 < 1000 && 0 <= p2 && p2 < 1000 ->
                Some (p1, p2)
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