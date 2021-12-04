let bingo_sample =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

type Calls = int list

type Value =
    | Called of int
    | NotCalled of int

type Board = Map<(int * int), Value>

type Game =
    { Calls: int list
      Called: int list
      Boards: Board list }

let read_calls (line: string) =
    line.Split(",") |> Seq.map int |> List.ofSeq

let read_board (line: string) =
    line.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.mapi (fun row data ->
        data.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.mapi (fun col value -> (row, col), NotCalled(int value)))
    |> Seq.concat
    |> Map.ofSeq

let read_data (lines: string) =
    let input =
        lines.Split(
            System.Environment.NewLine
            + System.Environment.NewLine,
            System.StringSplitOptions.RemoveEmptyEntries
        )

    let calls = read_calls (Seq.head input)

    let boards =
        input
        |> Seq.tail
        |> Seq.map read_board
        |> List.ofSeq

    { Calls = calls
      Called = []
      Boards = boards }

let update number (board: Board) =
    Map.toSeq board
    |> Seq.map (fun (p, v) ->
        match v with
        | NotCalled v' when v' = number -> p, Called v'
        | m -> p, m)
    |> Map.ofSeq

let run (game: Game) =
    if List.isEmpty game.Calls
       || List.isEmpty game.Boards then
        None
    else
        let number = List.head game.Calls
        let boards = List.map (update number) game.Boards

        Some
            { game with
                Calls = List.tail game.Calls
                Called = number :: game.Called
                Boards = boards }

let bingo_row (board: Board) row =
    Map.toSeq board
    |> Seq.forall (fun ((x, y), c) ->
        if y = row then
            match c with
            | Called _ -> true
            | _ -> false
        else
            true)

let bingo_column (board: Board) column =
    Map.toSeq board
    |> Seq.forall (fun ((x, y), c) ->
        if x = column then
            match c with
            | Called _ -> true
            | _ -> false
        else
            true)

let bingo (board: Board) =
    let (max_x, max_y) = Map.keys board |> Seq.max

    ([ 0 .. max_y ] |> Seq.exists (bingo_row board))
    || ([ 0 .. max_x ] |> Seq.exists (bingo_column board))

let winner (boards: Board list) = Seq.tryFindIndex bingo boards

let rec get_winner (game: Game) =
    match run game with
    | None -> failwith "Game not running"
    | Some game ->
        match winner game.Boards with
        | None -> get_winner game
        | Some idx -> List.item idx game.Boards, List.head game.Called

// Part A

let score ((board: Board), winning_call) =
    let uncalled =
        Map.values board
        |> Seq.map (fun c ->
            match c with
            | Called _ -> 0
            | NotCalled v -> v)
        |> Seq.sum

    uncalled * winning_call

read_data bingo_sample
|> get_winner
|> score
|> printfn "Test: 4512=%A"

System.IO.File.ReadAllText "day04.data"
|> read_data
|> get_winner
|> score
|> printfn "Part A: %d"

// Part B

let winners (boards: Board list) = Seq.filter bingo boards

let rec get_loser (last_winner: (Board * int) option) (game: Game) =
    match run game with
    | None ->
        match last_winner with
        | None -> failwith "Only way to win is not to play ..."
        | Some (board, called_by) -> board, called_by
    | Some game ->
        let winners = Seq.filter bingo game.Boards
        let losers = Seq.except winners game.Boards

        match Seq.tryLast winners with
        | None ->
            if Seq.isEmpty losers then
                match last_winner with
                | None -> failwith "Only way to win is not to play ..."
                | Some (board, called_by) -> board, called_by
            else
                get_loser last_winner { game with Boards = Seq.toList losers }
        | Some board ->
            if Seq.isEmpty losers then
                board, List.head game.Called
            else
                get_loser (Some(board, List.head game.Called)) { game with Boards = Seq.toList losers }

read_data bingo_sample
|> get_loser None
|> score
|> printfn "Test: 1924=%A"

System.IO.File.ReadAllText "day04.data"
|> read_data
|> get_loser None
|> score
|> printfn "Part B: %d"
