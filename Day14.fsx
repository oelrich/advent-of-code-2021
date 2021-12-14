let test_input =
    """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""

let load_template (input: string) =
    input.ToCharArray()
    |> Seq.pairwise
    |> Seq.groupBy id
    |> Seq.map (fun (f, s) -> f, uint64 (Seq.length s))
    |> Map.ofSeq

let load (input: string) =
    let parts =
        input.Split(
            System.Environment.NewLine
            + System.Environment.NewLine,
            System.StringSplitOptions.RemoveEmptyEntries
        )

    load_template parts.[0],
    parts.[1]
        .Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun s ->
        match s.ToCharArray() with
        | [| b; b'; ' '; '-'; '>'; ' '; c |] -> (b, b'), ((b, c), (c, b'))
        | sa -> sprintf "error in: %A" sa |> failwith)
    |> Map.ofSeq

let react (polymer, reactions) =
    Map.toSeq polymer
    |> Seq.map (fun (p: char * char, c: uint64) ->
        let (r1: char * char), r2 = Map.find p reactions
        [ r1, c; r2, c ])
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (p, cs) -> p, Seq.sumBy snd cs)
    |> Map.ofSeq,
    reactions

let react_in_array (polymer, reactions) =

    let next_length = 2 * Array.length polymer - 1
    let mutable next_polymer = Array.zeroCreate next_length
    next_polymer.[next_length - 1] <- Array.last polymer
    let last_polymer = Array.length polymer - 1

    Array.Parallel.iteri
        (fun idx b ->
            if idx <> last_polymer then
                let b' = Array.get polymer (idx + 1)
                let reaction = Map.find (b, b') reactions
                let new_idx = idx * 2
                next_polymer.[new_idx] <- b
                next_polymer.[new_idx + 1] <- reaction)
        polymer

    next_polymer, reactions

let max_min_min (polymer, _reactions) =
    let poly_parts =
        Map.toSeq polymer
        |> Seq.map (fun ((b, b'), c) -> [ b, c; b', c ])
        |> Seq.concat
        |> Seq.groupBy fst
        |> Seq.map (fun (_b, cs) -> Seq.sumBy snd cs)

    let min = Seq.min poly_parts
    let max = Seq.max poly_parts
    let biggy = max - min

    (biggy + biggy % 2UL) / 2UL

let rec react_n count reaction =
    if count > 0 then
        let next_reaction = react reaction
        react_n (count - 1) next_reaction
    else

        reaction

// Part A

let expected =
    load_template "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"

let actual = test_input |> load |> react_n 4
printfn "Test A: %A" (expected = fst actual)

test_input
|> load
|> react_n 10
|> max_min_min
|> printfn "Test A: 1588=%d"

System.IO.File.ReadAllText "day14.data"
|> load
|> react_n 10
|> max_min_min
|> printfn "Part A: %d"

// Part A

test_input
|> load
|> react_n 40
|> max_min_min
|> printfn "Test B: 2188189693529=%d"

System.IO.File.ReadAllText "day14.data"
|> load
|> react_n 40
|> max_min_min
|> printfn "Part B: %d"
