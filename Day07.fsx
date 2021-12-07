#r "nuget: FSharp.Stats"

open FSharp.Stats

let test_input = """16,1,2,0,4,2,7,1,2,14"""

let get_crabs (input: string) =
    input.Split(',', System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map int

let align_cost (at, crabs) =
    Seq.map (fun crab -> abs (crab - at)) crabs
    |> Seq.sum

let test_crabs = get_crabs test_input

Seq.median test_crabs |> printfn "Test: 2 = %d"

[ 2, 37; 1, 41; 3, 39; 10, 71 ]
|> Seq.iter (fun (pos, expected) ->
    let actual = align_cost (pos, test_crabs)

    if expected = actual then
        printfn "Test: Align at %d costs %d" pos expected
    else
        printfn "Test: Align at %d costs %d but should cost %d" pos actual expected)

// Part A

System.IO.File.ReadAllText "day07.data"
|> fun s -> s.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
|> Seq.head
|> get_crabs
|> fun cs -> Seq.median cs, cs
|> align_cost
|> printfn "Part A: %d"

// Part B

let rec p n = if n < 2 then n else n + p (n - 1)

let align_cost_b (at, crabs) =
    Seq.map (fun crab -> p (abs (crab - at))) crabs
    |> Seq.sum

[ 2, 206; 5, 168 ]
|> Seq.iter (fun (pos, expected) ->
    let actual = align_cost_b (pos, test_crabs)

    if expected = actual then
        printfn "Test: Align at %d costs %d" pos expected
    else
        printfn "Test: Align at %d costs %d but should cost %d" pos actual expected)

System.IO.File.ReadAllText "day07.data"
|> fun s -> s.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
|> Seq.head
|> get_crabs
|> fun cs ->
    [ 0 .. Seq.length cs ]
    |> Seq.map (fun pos -> align_cost_b (pos, cs), pos)
|> Seq.minBy fst
|> printfn "Part B: %A"
