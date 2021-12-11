let test_input =
    """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""

let actual_input =
    """5723573158
3154748563
4783514878
3848142375
3637724151
8583172484
7747444184
1613367882
6228614227
4732225334"""


let octopi (input: string) =
    input.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.mapi (fun row s ->
        s.ToCharArray()
        |> Seq.map (sprintf "%c")
        |> Seq.mapi (fun col c -> (col, row), int c))
    |> Seq.concat

let triggered (x, y) =
    [ (x - 1, y - 1)
      (x, y - 1)
      (x + 1, y - 1)
      (x - 1, y)
      (x + 1, y)
      (x - 1, y + 1)
      (x, y + 1)
      (x + 1, y + 1) ]

let get_charged spent (p, v) =
    if v > 9 then
        if Set.contains p spent then
            None
        else
            Some p
    else
        None


let rec trigger spent octopussies =
    let charged =
        Seq.choose (get_charged spent) octopussies

    if Seq.isEmpty charged then
        octopussies
    else
        let spent =
            Seq.fold (fun s t -> Set.add t s) spent charged

        let triggered = Seq.map triggered charged |> Seq.concat


        let octopussies =
            Seq.fold (fun s o -> Seq.map (fun (p, v) -> if p = o then p, v + 1 else p, v) s) octopussies triggered

        trigger spent octopussies

let step octopi =
    let flashing =
        Seq.map (fun (p, v) -> p, v + 1) octopi
        |> trigger Set.empty

    let flash_count =
        Seq.sumBy (fun (p, v) -> if v > 9 then 1 else 0) flashing

    flash_count, Seq.map (fun (p, v) -> if v > 9 then p, 0 else p, v) flashing

let rec steps count flash_count octopi =
    if count = 0 then
        flash_count, octopi
    else
        let octopi = Array.ofSeq octopi
        //        printfn "iter: %d" count
        let fc, octopi = step octopi
        steps (count - 1) (flash_count + fc) octopi

let rec get_sync count octopi =
    let octopi = Array.ofSeq octopi
    let fc, octopi = step octopi
    let count = count + 1

    if fc = 100 then
        count, octopi
    else
        get_sync count octopi


let carto data =
    let colours =
        [ (0, System.ConsoleColor.DarkBlue)
          (1, System.ConsoleColor.Blue)
          (2, System.ConsoleColor.DarkGreen)
          (3, System.ConsoleColor.Green)
          (4, System.ConsoleColor.DarkGray)
          (5, System.ConsoleColor.Gray)
          (6, System.ConsoleColor.DarkRed)
          (7, System.ConsoleColor.Red)
          (8, System.ConsoleColor.DarkYellow)
          (9, System.ConsoleColor.Yellow) ]
        |> Map.ofSeq

    let p =
        function
        | None ->
            System.Console.ForegroundColor <- System.ConsoleColor.Black
            printf "."
            System.Console.ResetColor()
        | Some v ->
            let colour = Map.find v colours
            System.Console.ForegroundColor <- colour
            printf "%d" v
            System.Console.ResetColor()

    let map = Map.ofSeq data
    let max_x = map.Keys |> Seq.map fst |> Seq.max
    let max_y = map.Keys |> Seq.map snd |> Seq.max

    [ 0 .. max_y ]
    |> Seq.iter (fun row ->
        [ 0 .. max_x ]
        |> Seq.iter (fun col -> Map.tryFind (col, row) map |> p)

        printfn "")

    data

let test_data =
    (System.IO.File.ReadAllText "day11.test")
        .Split(
            System.Environment.NewLine
            + System.Environment.NewLine,
            System.StringSplitOptions.RemoveEmptyEntries
        )
    |> Seq.map (fun s ->
        let parts =
            s.Split(':', System.StringSplitOptions.RemoveEmptyEntries)

        int parts.[0], octopi parts.[1] |> Map.ofSeq)

let space v =
    printfn ""
    v

let test_all start values =
    Seq.iter
        (fun (iter, result) ->
            let flash_count, stop = steps iter 0 start
            let stop = stop |> Map.ofSeq

            printfn "%d: %d %b" iter flash_count (stop = result))
        values

//test_all (test_input |> octopi) test_data

"""0397666866
0749766918
0053976933
0004297822
0004229892
0053222877
0532222966
9322228966
7922286866
6789998766"""
|> octopi
|> fun expected ->
    let fc, stop = steps 100 0 (test_input |> octopi)
    let result = Map.ofSeq expected = Map.ofSeq stop
    printfn "Test A: (%b) 1656=%d" result fc

"""5723573158
3154748563
4783514878
3848142375
3637724151
8583172484
7747444184
1613367882
6228614227
4732225334"""
|> octopi
|> fun data ->
    let fc, _stop = steps 100 0 data
    printfn "Part A: %d" fc


// Part B
let fc, _stop = get_sync 0 (test_input |> octopi)
printfn "Test A: 195=%d" fc


"""5723573158
3154748563
4783514878
3848142375
3637724151
8583172484
7747444184
1613367882
6228614227
4732225334"""
|> octopi
|> fun data ->
    let iter, _stop = get_sync 0 data
    printfn "Part B: %d" iter
