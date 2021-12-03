#r "nuget: FSharp.Data"
open FSharp.Data

[<Literal>]
let sample =
    """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

let data =
    sprintf "%s/day03.csv" __SOURCE_DIRECTORY__
    |> Some

type Bits = CsvProvider<sample, HasHeaders=false, Schema="Bits (string)">

let get_bits =
    function
    | None -> Bits.GetSample().Rows |> Seq.map (fun s -> s.Bits)
    | Some (file: string) -> Bits.Load(file).Rows |> Seq.map (fun s -> s.Bits)

// Part A

type BinaryCounter = { Zero: int; One: int }

let update_char ((counts: BinaryCounter), c) =
    match c with
    | '0' -> { counts with Zero = counts.Zero + 1 }
    | '1' -> { counts with One = counts.One + 1 }
    | v -> sprintf "Unknown binary digit: %c" v |> failwith

let create_counts width entries =
    let update (counts: BinaryCounter array) (data: string) =
        data.ToCharArray()
        |> Array.zip counts
        |> Array.map update_char

    Seq.fold update (Array.create width { Zero = 0; One = 0 }) entries

let get_power_consumption width counts =
    Seq.mapi
        (fun i c ->
            let position_value = int (2.0 ** (float (width - i - 1)))

            if c.Zero > c.One then
                (0, position_value)
            else
                (position_value, 0))
        counts
    |> Seq.fold (fun (s_most, s_least) (most, least) -> (s_most + most, s_least + least)) (0, 0)
    |> (fun (m, l) -> m * l)

let bits_to_int (bits: string) =
    let width = bits.Length

    bits.ToCharArray()
    |> Seq.mapi (fun i c ->
        match c with
        | '0' -> 0
        | '1' -> int (2.0 ** (float (width - i - 1)))
        | v -> sprintf "Odd binary digit: %c" v |> failwith)
    |> Seq.sum

let sample_bits = get_bits None
let sample_length = Seq.head sample_bits |> String.length
let sample_counts = create_counts sample_length sample_bits

get_power_consumption sample_length sample_counts
|> printfn "Test 198 = %A"


let data_bits = get_bits data
let data_length = Seq.head data_bits |> String.length
let data_counts = create_counts data_length data_bits

get_power_consumption data_length data_counts
|> printfn "A = %A"

// Part B

let rec shrink (data: seq<string>) operator position =
    if (Seq.length data) = 1 then
        Seq.head data
    else
        let zeroes =
            Seq.filter (fun (e: string) -> e.Chars position = '0') data

        let ones =
            Seq.filter (fun (e: string) -> e.Chars position = '1') data

        if operator (Seq.length zeroes) (Seq.length ones) then
            shrink zeroes operator (position + 1)
        else
            shrink ones operator (position + 1)

shrink sample_bits (fun z o -> z > o) 0
|> bits_to_int
|> printfn "Test Oxygen 23= %A"

shrink sample_bits (fun z o -> z <= o) 0
|> bits_to_int
|> printfn "Test CO2 10= %A"

let oxygen =
    shrink data_bits (fun z o -> z > o) 0
    |> bits_to_int

let co2 =
    shrink data_bits (fun z o -> z <= o) 0
    |> bits_to_int

printfn "Support rating: %d" (oxygen * co2)
