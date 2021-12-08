let sample_test_input =
    """acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"""

let test_input =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""

type Segment =
    | Top
    | Middle
    | Bottom
    | UpperLeft
    | UpperRight
    | LowerLeft
    | LowerRight


type DisplayData =
    { Sample: Map<int, seq<Set<char>>>
      Display: seq<Set<char>> }

let read (data: string) =
    let data = data.Split('|')

    let display =
        data.[1]
            .Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s -> s.ToCharArray() |> Set.ofArray)

    let sample =
        data.[0]
            .Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s -> s.ToCharArray() |> Set.ofArray)
        |> Seq.groupBy (fun s -> s.Count)
        |> Map.ofSeq

    { Sample = sample; Display = display }

let get (s: Set<char>) : char =
    if Set.count s = 1 then
        Set.minElement s
    else
        sprintf "Not a single thing: %A" s |> failwith

let get_single =
    Seq.choose (fun s -> if Set.count s = 1 then Some s else None)
    //    |> Seq.concat
    >> Set.unionMany
    >> get

let derive (data: Map<int, seq<Set<char>>>) =
    let one = Seq.head (Map.find 2 data)
    let seven = Seq.head (Map.find 3 data)
    let four = Seq.head (Map.find 4 data)
    let eight = Seq.head (Map.find 7 data)

    let five_two_three = Map.find 5 data
    let six_nine_zero = Map.find 6 data

    let top = Set.difference seven one |> get

    let upper_left: char =
        Seq.map (fun s -> Set.difference (Set.difference four one) s) five_two_three
        |> get_single

    let middle =
        Set.difference four one
        |> Set.remove upper_left
        |> get

    let bottom =
        Seq.map (fun s -> Set.difference s (Set.union four seven)) five_two_three
        |> get_single

    let lower_left =
        Set.difference
            eight
            (Set.add upper_left seven
             |> Set.add middle
             |> Set.add bottom)
        |> get

    let upper_right =
        Seq.map
            (fun s ->
                Set.difference
                    s
                    (Set.ofList [ top
                                  middle
                                  bottom
                                  lower_left ]))
            five_two_three
        |> get_single

    let lower_right = Set.remove upper_right one |> get

    [ (top, Top)
      (middle, Middle)
      (bottom, Bottom)
      (upper_left, UpperLeft)
      (lower_left, LowerLeft)
      (upper_right, UpperRight)
      (lower_right, LowerRight) ]
    |> Map.ofList


let values =
    [ Set.ofList [ Top
                   UpperLeft
                   UpperRight
                   LowerLeft
                   LowerRight
                   Bottom ],
      0
      Set.ofList [ UpperRight; LowerRight ], 1
      Set.ofList [ Top
                   UpperRight
                   Middle
                   LowerLeft
                   Bottom ],
      2
      Set.ofList [ Top
                   UpperRight
                   Middle
                   LowerRight
                   Bottom ],
      3
      Set.ofList [ UpperLeft
                   UpperRight
                   Middle
                   LowerRight ],
      4
      Set.ofList [ Top
                   UpperLeft
                   Middle
                   LowerRight
                   Bottom ],
      5
      Set.ofList [ Top
                   UpperLeft
                   Middle
                   LowerLeft
                   LowerRight
                   Bottom ],
      6
      Set.ofList [ Top
                   UpperRight
                   LowerRight ],
      7
      Set.ofList [ Top
                   UpperLeft
                   UpperRight
                   Middle
                   LowerLeft
                   LowerRight
                   Bottom ],
      8
      Set.ofList [ Top
                   UpperLeft
                   UpperRight
                   Middle
                   LowerRight
                   Bottom ],
      9 ]
    |> Map.ofList



let decode (dd: DisplayData) =
    let decoder = derive dd.Sample

    dd.Display
    |> Seq.map (fun s ->
        Set.map
            (fun c ->
                match Map.tryFind c decoder with
                | Some s -> s
                | None -> failwith "baaaad")
            s)
    |> Seq.map (fun s ->
        match Map.tryFind s values with
        | Some v -> v
        | None -> sprintf "%A" s |> failwith)
    |> Seq.fold (sprintf "%s%d") ""
    |> int

let sample_data = read sample_test_input

printfn "%d" <| decode sample_data

(*
  |2| -> 1
  |3| -> 7
  |4| -> 4
  |5| -> 5, 2, 3
  |6| -> 6, 9, 0
  |7| -> 8

  |3| - |2| -> Top
  |4| - |2| -> {UpperLeft, Middle}
  |7| - |3| - |4| -> {LowerLeft, Bottom}

  8 - 9 -> LowerLeft
  8 - 0 -> Middle
  4 - 2 - Middle -> UpperLeft
*)

let test_data =
    test_input.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map read

let count_data =
    System.IO.File.ReadAllText "day08.data"
    |> fun s -> s.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map read

// Part A

let counter (dd: DisplayData) =
    dd.Display
    |> Seq.sumBy (fun s ->
        match Set.count s with
        | 2
        | 3
        | 4
        | 7 -> 1
        | _ -> 0)

printfn "Test A: 26=%d"
<| Seq.sumBy counter test_data

printfn "Part A: %d"
<| Seq.sumBy counter count_data

// Part B

test_data
|> Seq.map decode
|> Seq.sum
|> fun d -> printfn "Test B: 61299=%d" d

count_data
|> Seq.map decode
|> Seq.sum
|> fun d -> printfn "Part B: %d" d
