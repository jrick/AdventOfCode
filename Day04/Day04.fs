open System.Security.Cryptography
open System.Text

module Input =
    let key = "bgvyzdsv"

module MD5 =
    let hash (md5 : MD5) (input : byte array) =
        md5.ComputeHash(input)

let doHash md5 key nonce =
    let nonceBytes = Encoding.UTF8.GetBytes(sprintf "%d" nonce)
    let input = Array.append key nonceBytes
    MD5.hash md5 input

let has5Zeros (arr : byte array) =
    let sum = int arr.[0] + int arr.[1] + int (arr.[2] >>> 4)
    sum = 0

let has6Zeros (arr : byte array) =
    let sum = int arr.[0] + int arr.[1] + int arr.[2]
    sum = 0

let challenge isAnswer key =
    use md5 = MD5.Create()
    let nonces = Seq.initInfinite (fun i -> i)
    let answer, _ =
        nonces
        |> Seq.map (fun nonce -> nonce, doHash md5 key nonce)
        |> Seq.find (fun (nonce, hash) -> isAnswer hash)
    answer

let challenge1 key = challenge has5Zeros key

let challenge2 key = challenge has6Zeros key

[<EntryPoint>]
let main args =
    let key = Encoding.UTF8.GetBytes(Input.key)
    challenge1 key |> printfn "Smallest 5-zero answer is %d"
    challenge2 key |> printfn "Smallest 6-zero answer is %d"
    0