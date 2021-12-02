#r "nuget: FSharp.Data"
open FSharp.Data

type Move =
    | Forward of int
    | Down of int
    | Up of int

[<Literal>]
let sample =
    """forward 5
down 5
forward 8
up 3
down 8
forward 2"""

let data =
    sprintf "%s/day02.csv" __SOURCE_DIRECTORY__
    |> Some

type Moves = CsvProvider<sample, Separators=" ", HasHeaders=false, Schema="Direction (string), Value (int)">

let move_from (it: Moves.Row) =
    match it.Direction with
    | "forward" -> Forward it.Value
    | "up" -> Up it.Value
    | "down" -> Down it.Value
    | error -> failwith <| sprintf "ERROR: %s" error

let get_moves =
    function
    | None -> Moves.GetSample().Rows |> Seq.map move_from
    | Some (file: string) -> Moves.Load(file).Rows |> Seq.map move_from

// Part A

type Position = { Horizontal: int; Depth: int }


let move (position: Position) (move: Move) =
    match move with
    | Forward value -> { position with Horizontal = position.Horizontal + value }
    | Up value -> { position with Depth = position.Depth - value }
    | Down value -> { position with Depth = position.Depth + value }

get_moves None
|> Seq.fold move { Horizontal = 0; Depth = 0 }
|> fun p -> p.Depth * p.Horizontal |> printfn "Test: 150 = %d"

get_moves data
|> Seq.fold move { Horizontal = 0; Depth = 0 }
|> fun p -> p.Depth * p.Horizontal |> printfn "A = %d"

// Part B

type AimedPosition =
    { Aim: int
      Horizontal: int
      Depth: int }

let b_move (position: AimedPosition) (move: Move) =
    match move with
    | Forward value ->
        { position with
            Horizontal = position.Horizontal + value
            Depth = position.Depth + value * position.Aim }
    | Up value -> { position with Aim = position.Aim - value }
    | Down value -> { position with Aim = position.Aim + value }

get_moves None
|> Seq.fold b_move { Aim = 0; Horizontal = 0; Depth = 0 }
|> fun p -> p.Depth * p.Horizontal |> printfn "Test: 900 = %d"

get_moves data
|> Seq.fold b_move { Aim = 0; Horizontal = 0; Depth = 0 }
|> fun p -> p.Depth * p.Horizontal |> printfn "B = %d"
