type Cave =
    | Start
    | End
    | Big of string
    | Small of string

let cave_of_string =
    function
    | "start" -> Start
    | "end" -> End
    | cave ->
        if Seq.forall (fun c -> System.Char.IsUpper c) (cave.ToCharArray()) then
            Big cave
        else
            Small cave

type CaveMap = Map<Cave, Cave list>

let load (input: string) : CaveMap =
    input.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun s ->
        s.Split('-', System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map cave_of_string
        |> fun s ->
            if Seq.length s = 2 then
                [ Seq.item 0 s, Seq.item 1 s
                  Seq.item 1 s, Seq.item 0 s ]
            else
                sprintf "%A" s |> failwith)
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (c, s) -> c, Seq.map snd s |> Seq.toList)
    |> Map.ofSeq

let print map =
    printfn "%A" map
    map

// Part A

let get_and_reduce caves location =
    Map.tryFind location caves,
    match location with
    | Big cn -> caves
    | _ -> Map.remove location caves

let rec paths (caves: CaveMap) path location =
    match location with
    | End -> End :: path |> List.rev |> Seq.singleton
    | _ ->
        let possible, new_caves = get_and_reduce caves location

        match possible with
        | None -> Seq.singleton []
        | Some possible ->
            Seq.map (fun next_location -> paths new_caves (location :: path) next_location) possible
            |> Seq.concat

let get_paths caves =
    paths caves [] Start
    |> Seq.filter (List.isEmpty >> not)

"""start-A
start-b
A-c
A-b
b-d
A-end
b-end"""
|> load
|> get_paths
|> Seq.length
|> printfn "Test A: 10=%A"

"""dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""
|> load
|> get_paths
|> Seq.length
|> printfn "Test A: 19=%A"

"""fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""
|> load
|> get_paths
|> Seq.length
|> printfn "Test A: 226=%A"

System.IO.File.ReadAllText "day12.data"
|> load
|> get_paths
|> Seq.length
|> printfn "Part A: %A"

// Part B

type Caverns =
    | Strict of CaveMap
    | Revisit of CaveMap

let get_and_reduce_b caves location =
    match caves, location with
    | Strict caves, Big _ -> Map.tryFind location caves, Strict caves, Strict caves
    | Strict caves, Small _ ->
        let shrunk = Map.remove location caves |> Strict
        Map.tryFind location caves, shrunk, shrunk
    | Revisit caves, Big _ -> Map.tryFind location caves, Revisit caves, Revisit caves
    | Revisit caves, Small _ -> Map.tryFind location caves, Strict caves, Map.remove location caves |> Revisit
    | _, Start ->
        let caves =
            match caves with
            | Strict cm -> cm
            | Revisit cm -> cm

        let start = Map.remove location caves |> Revisit
        Map.tryFind location caves, start, start
    | _, End -> None, Strict Map.empty, Strict Map.empty

let rec paths_b (caves: Caverns) path location =
    match location with
    | End -> End :: path |> List.rev |> Seq.singleton
    | _ ->
        let possible, new_caves, other_new_caves = get_and_reduce_b caves location

        match possible with
        | None -> Seq.singleton []
        | Some possible ->
            if new_caves = other_new_caves then
                Seq.map (fun next_location -> paths_b new_caves (location :: path) next_location) possible
                |> Seq.concat
            else
                let strict =
                    Seq.map (fun next_location -> paths_b new_caves (location :: path) next_location) possible
                    |> Seq.concat

                let brideshead =
                    Seq.map (fun next_location -> paths_b other_new_caves (location :: path) next_location) possible
                    |> Seq.concat

                Seq.append strict brideshead
                |> Seq.toList
                |> Seq.ofList

let get_paths_b caves =
    paths_b (Revisit caves) [] Start
    |> Seq.distinct
    |> Seq.filter (List.isEmpty >> not)

"""start-A
start-b
A-c
A-b
b-d
A-end
b-end"""
|> load
|> get_paths_b
|> Seq.length
|> printfn "Test B: 36=%A"

"""dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""
|> load
|> get_paths_b
|> Seq.length
|> printfn "Test B: 103=%A"

"""fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""
|> load
|> get_paths_b
|> Seq.length
|> printfn "Test B: 3509=%A"

System.IO.File.ReadAllText "day12.data"
|> load
|> get_paths_b
|> Seq.length
|> printfn "Part B: %A"
