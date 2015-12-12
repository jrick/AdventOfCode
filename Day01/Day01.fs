open System
open System.IO

let private step ch =
    match ch with
    | '(' -> 1
    | ')' -> -1
    | _ -> 0
        
let challenge1 input =
    Seq.sumBy step input |> sprintf "Ends at floor %d"

let challenge2 input =
    let floors = Seq.scan (fun floor direction -> floor + step direction) 0 input
    match Seq.tryFindIndex (fun floor -> floor = -1) floors with
    | Some pos -> sprintf "Reaches basement at position %d" pos
    | None -> sprintf "Never reaches the basement"

module Input =
    let readInput =
        use sr = new StreamReader("input.txt")
        sr.ReadLine() 

[<EntryPoint>]
let main args =
    let input = Input.readInput
    challenge1 input |> printfn "%s"
    challenge2 input |> printfn "%s"
    0