let sample_input =
    """2199943210
3987894921
9856789892
8767896789
9899965678"""

let read_map (data: string) =
    data.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.mapi (fun row row_data ->
        row_data.ToCharArray()
        |> Seq.mapi (fun col height -> (col, row), sprintf "%c" height |> int))
    |> Seq.concat
    |> Map.ofSeq

let neighbours map (x, y) =
    [ (x - 1, y)
      (x + 1, y)
      (x, y - 1)
      (x, y + 1) ]
    |> Seq.choose (fun p -> Map.tryFind p map |> Option.map (fun v -> p, v))

let local_minimum map p =
    let value: int = Map.find p map

    neighbours map p
    |> Seq.forall (fun v -> snd v > value)

let risk_level map p = 1 + Map.find p map

// Part A

let sample_map = read_map sample_input

Map.keys sample_map
|> Seq.filter (local_minimum sample_map)
|> Seq.map (risk_level sample_map)
|> Seq.sum
|> printfn "Test A: 15=%d"

System.IO.File.ReadAllText "day09.data"
|> read_map
|> fun map ->
    Map.keys map
    |> Seq.filter (local_minimum map)
    |> Seq.map (risk_level map)
    |> Seq.sum
|> printfn "Part A: %d"

// Part B

let three_big_ones data =
    let one = Set.maxElement data
    let two = Set.maxElement (Set.remove one data)

    let three =
        Set.maxElement (Set.remove two (Set.remove one data))

    [ one; two; three ]

let rec basin map visit finished =
    if Set.isEmpty visit then
        finished
    else
        let p = Set.minElement visit

        match Map.find p map with
        | 9 -> basin map (Set.remove p visit) finished
        | _ ->
            let finished = Set.add p finished

            let neighbours =
                neighbours map p
                |> Seq.choose (fun (p, v) -> if v = 9 then None else Some p)
                |> Set.ofSeq
                |> Set.union visit
                |> fun s -> Set.difference s finished

            basin map neighbours finished

Map.keys sample_map
|> Seq.filter (local_minimum sample_map)
|> Seq.map (fun lm -> basin sample_map (Set.singleton lm) Set.empty)
|> Seq.map Set.count
|> Seq.sortDescending
|> Seq.take 3
|> Seq.fold (*) 1
|> printfn "Test B: 1134=%A"

System.IO.File.ReadAllText "day09.data"
|> read_map
|> fun map ->
    Map.keys map
    |> Seq.filter (local_minimum map)
    |> Seq.map (fun lm -> basin map (Set.singleton lm) Set.empty)
    |> Seq.map Set.count
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (*) 1
|> printfn "Part A: %d"
