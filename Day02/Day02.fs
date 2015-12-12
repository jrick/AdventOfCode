open System
open System.IO

module Parse =
    let tryParse parse = parse >> function
        | true, v -> Some v
        | false, _ -> None
    
    let int = tryParse Int32.TryParse

type Box =
    { Length: int; Width: int; Height: int }
    member this.SurfaceArea = 2 * (this.Length + this.Width + this.Height)
    member this.Volume = this.Length * this.Width * this.Height

let requiredWrappingPaper box =
    let face1 = box.Length * box.Width
    let face2 = box.Width * box.Height
    let face3 = box.Height * box.Length
    let smallestFace = min box.Length box.Width |> min box.Height
    box.SurfaceArea + smallestFace

let requiredRibbon box =
    let largestDimension = max box.Length box.Width |> max box.Height 
    let shortestPerimiter = 2 * (box.Length + box.Width + box.Height - largestDimension)
    let bowLength = box.Volume
    shortestPerimiter + bowLength

let readBox input =
    let splitStrings = (input: String).Split('x')
    let splitInts = Array.map Parse.int splitStrings
    match splitInts with
    | [| Some l; Some w; Some h |] -> Some { Length = l; Width = w; Height = h }
    | _ -> None

let readInput fileName =
    seq { use r = new StreamReader(fileName: String)
        while not r.EndOfStream do yield r.ReadLine() }
    |> Seq.map readBox
    |> Seq.choose (fun box -> box)
    
let challenge1 boxes =
    Seq.sumBy requiredWrappingPaper boxes

let challenge2 boxes =
    Seq.sumBy requiredRibbon boxes

[<EntryPoint>]
let main args =
    let boxes = readInput "input.txt"
    challenge1 boxes |> printfn "Require %d square feet of wrapping paper"
    challenge2 boxes |> printfn "Require %d feet of ribbon"
    0