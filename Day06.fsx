let state (school: string) =
    school.Split(',')
    |> Seq.map int
    |> Seq.fold
        (fun count_by_age age ->
            let count = 1UL + Array.get count_by_age age
            Array.updateAt age count count_by_age)
        (Array.zeroCreate 9)

let run (school: uint64 array) =
    let reproduce = Array.head school
    let new_sixes = reproduce + Array.get school (6 + 1)

    Array.append (Array.tail school) [| reproduce |]
    |> Array.updateAt 6 new_sixes

let test_days =
    [ 1, state "2,3,2,0,1"
      2, state "1,2,1,6,0,8"
      3, state "0,1,0,5,6,7,8"
      4, state "6,0,6,4,5,6,7,8,8"
      5, state "5,6,5,3,4,5,6,7,7,8"
      6, state "4,5,4,2,3,4,5,6,6,7"
      7, state "3,4,3,1,2,3,4,5,5,6"
      8, state "2,3,2,0,1,2,3,4,4,5"
      9, state "1,2,1,6,0,1,2,3,3,4,8"
      10, state "0,1,0,5,6,0,1,2,2,3,7,8"
      11, state "6,0,6,4,5,6,0,1,1,2,6,7,8,8,8"
      12, state "5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8"
      13, state "4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8"
      14, state "3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8"
      15, state "2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7"
      16, state "1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8"
      17, state "0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8"
      18, state "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8" ]

let rec test school =
    function
    | [] -> true
    | (_d, s) :: rest ->
        let next_school = run school
        (s = next_school) && test next_school rest

test (state "3,4,3,1,2") test_days

let rec run_for days school =
    if days = 0 then
        school
    else
        run_for (days - 1) (run school)

// Part A

printfn "Test 18; 26=%d" (Array.sum <| run_for 18 (state "3,4,3,1,2"))

printfn "Test 80; 5934=%d" (Array.sum <| run_for 80 (state "3,4,3,1,2"))

System.IO.File.ReadAllText "day06.data"
|> fun s -> s.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
|> Array.head
|> state
|> run_for 80
|> Array.sum
|> printfn "Part A: %d"

// Part B

printfn "Test 256; 26984457539=%d" (Array.sum <| run_for 256 (state "3,4,3,1,2"))

System.IO.File.ReadAllText "day06.data"
|> fun s -> s.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
|> Array.head
|> state
|> run_for 256
|> Array.sum
|> printfn "Part B: %d"
