open System.IO

module Challenge1 =
    type Requirements = { Vowels : int; Duplicate : bool }
    with
        static member Initial =
            {
                Vowels = 0;
                Duplicate = false;
            }
        member r.Nice =
            r.Vowels >= 3 && r.Duplicate

    let isVowel = function
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false

    let naughtyChars = function
        | 'a','b' | 'c','d' | 'p','q' | 'x','y' -> true
        | _ -> false

    let rec niceString prevChar (requirements : Requirements) input =
        match input with
        | "" -> requirements.Nice
        | s ->
            let head = s.[0]
            let tail = s.Substring(1)
            let vowelCount =
                if isVowel head then requirements.Vowels + 1
                else requirements.Vowels
            match prevChar with
            | Some ch when naughtyChars (ch,head) -> false
            | Some ch ->
                let newRequirements =
                    { requirements with
                        Vowels = vowelCount;
                        Duplicate = requirements.Duplicate || ch = head
                    }
                niceString (Some head) newRequirements tail
            | None ->
                niceString (Some head) { requirements with Vowels = vowelCount } tail

    let challenge1 input =
        input
        |> Seq.filter (fun s -> niceString None Requirements.Initial s)
        |> Seq.length

module Challenge2 =
    let removeOverlappingPairs (s : string) =
        seq {
            let mutable prevPair = None
            for pair in s |> Seq.pairwise do
                match prevPair with
                | Some pp ->
                    if pp = pair then prevPair <- None
                    else
                        yield pair
                        prevPair <- Some pair
                | None ->
                    yield pair
                    prevPair <- Some pair
        }

    let hasNonOverlappingPair (s : string) =
        let noOverlaps = s |> removeOverlappingPairs |> Seq.toList
        let unique = Seq.distinct noOverlaps
        List.length noOverlaps > Seq.length unique

    let isOutsideRepeat = function
        | [| a; _; b |] -> a = b
        | _ -> false

    let hasOutsideRepeat s =
        s |> Seq.windowed 3 |> Seq.exists isOutsideRepeat

    let isNice s =
        hasNonOverlappingPair s && hasOutsideRepeat s

    let challenge2 input =
        input
        |> Seq.filter isNice
        |> Seq.length

module Input =
    let allStrings =
        seq { use r = new StreamReader("input.txt")
            while not r.EndOfStream do yield r.ReadLine() }

[<EntryPoint>]
let main args =
    let input = Input.allStrings
    Challenge1.challenge1 input |> printfn "There are %d nice strings using the original requirements"
    Challenge2.challenge2 input |> printfn "There are %d nice strings using the revised requirements"
    0
