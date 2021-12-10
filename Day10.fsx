let test_input =
    """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""

let read (input: string) =
    input.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun s -> s.ToCharArray())

type ParenType =
    | Paren
    | Brace
    | Curly
    | Neq

let (|Open|_|) c =
    match c with
    | '<' -> Some Neq
    | '(' -> Some Paren
    | '{' -> Some Curly
    | '[' -> Some Brace
    | _ -> None

let (|Close|_|) c =
    match c with
    | '>' -> Some Neq
    | ')' -> Some Paren
    | '}' -> Some Curly
    | ']' -> Some Brace
    | _ -> None

type ParseResult =
    | Ok
    | Partial of ParenType list
    | SyntaxError of ParenType * (ParenType list)

let rec parse state data =
    if Seq.isEmpty data then
        if Seq.isEmpty state then
            Ok
        else
            Partial state
    else
        match Seq.head data with
        | Open p -> parse (p :: state) (Seq.tail data)
        | Close p ->
            if Seq.head state = p then
                parse (List.tail state) (Seq.tail data)
            else
                SyntaxError(p, state)
        | e ->
            sprintf "Found unexpected character: %c" e
            |> failwith

// Part A

let error_score =
    function
    | Curly -> 1197
    | Paren -> 3
    | Brace -> 57
    | Neq -> 25137

test_input
|> read
|> Seq.map (parse List.empty)
|> Seq.choose (fun p ->
    match p with
    | SyntaxError (p, _s) -> Some p
    | _ -> None)
|> Seq.map error_score
|> Seq.sum
|> printfn "Test A: 26397=%d"

System.IO.File.ReadAllText "day10.data"
|> read
|> Seq.map (parse List.empty)
|> Seq.choose (fun p ->
    match p with
    | SyntaxError (p, _s) -> Some p
    | _ -> None)
|> Seq.map error_score
|> Seq.sum
|> printfn "Part A: %d"

// Part B

let complete_score =
    function
    | Curly -> 3UL
    | Paren -> 1UL
    | Brace -> 2UL
    | Neq -> 4UL

let rec auto_score score state =
    match state with
    | [] -> score
    | p :: lst -> auto_score (score * 5UL + (complete_score p)) lst

test_input
|> read
|> Seq.map (parse List.empty)
|> Seq.choose (fun p ->
    match p with
    | Partial s -> Some(auto_score 0UL s)
    | _ -> None)
|> fun scores ->
    let mid = (Seq.length scores) / 2
    Seq.sort scores |> Seq.item mid
|> printfn "Test B: 288957=%d"

System.IO.File.ReadAllText "day10.data"
|> read
|> Seq.map (parse List.empty)
|> Seq.choose (fun p ->
    match p with
    | Partial s -> Some(auto_score 0UL s)
    | _ -> None)
|> fun scores ->
    let mid = (Seq.length scores) / 2
    Seq.sort scores |> Seq.item mid
|> printfn "Part A: %d"
