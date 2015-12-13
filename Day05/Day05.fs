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

module Input =
    let allStrings =
        seq { use r = new StreamReader("input.txt")
            while not r.EndOfStream do yield r.ReadLine() }

[<EntryPoint>]
let main args =
    let input = Input.allStrings
    Challenge1.challenge1 input |> printfn "There are %d nice strings"
    0
