let sample_data =
    """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

let fwm message source =
    sprintf "%s%A" message source |> failwith

type Point = { X: int; Y: int }

type Line = { A: Point; B: Point }

type VentMap = Map<(int * int), int>

let build_map (points: Point seq) : VentMap =
    points
    |> Seq.map (fun p -> p.X, p.Y)
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> k, Seq.length v)
    |> Map.ofSeq

let carto (map: VentMap) =
    let p =
        function
        | None ->
            System.Console.ForegroundColor <- System.ConsoleColor.DarkGray
            printf "."
            System.Console.ResetColor()
        | Some 1 ->
            System.Console.ForegroundColor <- System.ConsoleColor.Cyan
            printf "1"
            System.Console.ResetColor()
        | Some 2 ->
            System.Console.ForegroundColor <- System.ConsoleColor.Red
            printf "2"
            System.Console.ResetColor()
        | Some v ->
            System.Console.ForegroundColor <- System.ConsoleColor.DarkRed
            printf "%d" v
            System.Console.ResetColor()

    let max_x = map.Keys |> Seq.map fst |> Seq.max
    let max_y = map.Keys |> Seq.map snd |> Seq.max

    [ 0 .. max_y ]
    |> Seq.iter (fun row ->
        [ 0 .. max_x ]
        |> Seq.iter (fun col -> Map.tryFind (col, row) map |> p)

        printfn "")

    map

let vent_line (input: string) =
    input.Split(" -> ")
    |> Array.map (fun p ->
        match p.Split(',') with
        | [| x; y |] -> { X = int x; Y = int y }
        | f -> fwm "Not a valid point: " f)
    |> fun points ->
        match points with
        | [| a; b |] -> { A = a; B = b }
        | f -> fwm "Strange number of points: " f

let vent_lines (input: string) =
    input.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map vent_line

let mix a b = if a < b then a, b else b, a

// Part A

let ortho_points (line: Line) =
    match line.A, line.B with
    | p1, p2 when p1.X = p2.X ->
        let (min, max) = mix p1.Y p2.Y

        [ min .. max ]
        |> Seq.map (fun y -> { X = p1.X; Y = y })
    | p1, p2 when p1.Y = p2.Y ->
        let (min, max) = mix p1.X p2.X

        [ min .. max ]
        |> Seq.map (fun x -> { X = x; Y = p1.Y })
    | _ -> Seq.empty

vent_lines sample_data
|> fun d ->
    printfn "%d" (Seq.length d)
    d
|> Seq.map ortho_points
|> Seq.concat
|> build_map
|> carto
|> Map.values
|> Seq.filter (fun v -> v > 1)
|> Seq.length
|> printfn "Test: 5=%d"

System.IO.File.ReadAllText "day05.data"
|> vent_lines
|> fun d ->
    printfn "%d" (Seq.length d)
    d
|> Seq.map ortho_points
|> Seq.concat
|> build_map
//|> carto
|> Map.values
|> Seq.filter (fun v -> v > 1)
|> Seq.length
|> printfn "Part A: %d"

// Part B

let diagonal (line: Line) =
    let delta_x = line.B.X - line.A.X
    let delta_y = line.B.Y - line.A.Y

    if abs delta_x = abs delta_y then
        Some(delta_x / abs delta_x, delta_y / abs delta_y)
    else
        None

let rec get_points (dx, dy) p p_end =
    if p = p_end then
        [ p ]
    else
        p
        :: get_points (dx, dy) { X = p.X + dx; Y = p.Y + dy } p_end

let points (line: Line) =
    match line.A, line.B with
    | p1, p2 when p1.X = p2.X ->
        let (min, max) = mix p1.Y p2.Y

        [ min .. max ]
        |> Seq.map (fun y -> { X = p1.X; Y = y })
    | p1, p2 when p1.Y = p2.Y ->
        let (min, max) = mix p1.X p2.X

        [ min .. max ]
        |> Seq.map (fun x -> { X = x; Y = p1.Y })
    | p1, p2 ->
        match diagonal line with
        | None -> Seq.empty
        | Some step -> get_points step p1 p2

vent_lines sample_data
|> fun d ->
    printfn "%d" (Seq.length d)
    d
|> Seq.map points
|> Seq.concat
|> build_map
|> carto
|> Map.values
|> Seq.filter (fun v -> v > 1)
|> Seq.length
|> printfn "Test: 12=%d"

System.IO.File.ReadAllText "day05.data"
|> vent_lines
|> fun d ->
    printfn "%d" (Seq.length d)
    d
|> Seq.map points
|> Seq.concat
|> build_map
//|> carto
|> Map.values
|> Seq.filter (fun v -> v > 1)
|> Seq.length
|> printfn "Part B: %d"
