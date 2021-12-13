let test_input =
    """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"""

type Fold =
    | X of int
    | Y of int

let load (input: string) =
    let data =
        input.Split(
            System.Environment.NewLine
            + System.Environment.NewLine,
            System.StringSplitOptions.RemoveEmptyEntries
        )

    let points =
        data.[0]
            .Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s ->
            let p = s.Split(',')
            int p.[0], int p.[1])
        |> Set.ofSeq

    let instructions =
        data.[1]
            .Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s ->
            match s.Split('=') |> List.ofSeq with
            | "fold along y" :: num :: [] -> int num |> Y
            | "fold along x" :: num :: [] -> int num |> X
            | err -> sprintf "Not an instruction: %A" err |> failwith)
        |> List.ofSeq

    (points, instructions)

let print (points, instructions) =
    let max_x = Seq.map fst points |> Seq.max
    let max_y = Seq.map snd points |> Seq.max

    [ 0 .. max_y ]
    |> Seq.iter (fun y ->
        [ 0 .. max_x ]
        |> Seq.iter (fun x ->
            if (Set.contains (x, y) points) then
                printf "#"
            else
                printf ".")

        printfn "")

    instructions
    |> Seq.iter (function
        | X value -> printfn "Fold along x=%d" value
        | Y value -> printfn "Fold along y=%d" value)

    (points, instructions)

let empty_line data =
    printfn ""
    data

let fold (points, instructions) =
    match instructions with
    | [] -> points, instructions
    | X value :: rest -> Set.map (fun (x, y) -> value - abs (value - x), y) points, rest
    | Y value :: rest -> Set.map (fun (x, y) -> x, value - abs (value - y)) points, rest

// Part A

let count (points, _instr) = Set.count points

test_input
|> load
|> print
|> empty_line
|> fold
|> print
|> empty_line
|> count
|> printfn "Test A: 17=%d"

System.IO.File.ReadAllText("day13.data")
|> load
|> fold
|> count
|> printfn "Part A: %d"

// Part B

let rec fold_all (points, instructions) =
    match instructions with
    | [] -> points, []
    | _ -> fold (points, instructions) |> fold_all

test_input
|> load
|> print
|> empty_line
|> fold_all
|> print
|> ignore

System.IO.File.ReadAllText("day13.data")
|> load
|> fold_all
|> print
|> ignore
