#r "nuget: FSharp.Data"
open FSharp.Data

let sonar_sweep_test_7 =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]

[<Literal>]
let ResolutionFolder = __SOURCE_DIRECTORY__

type SonarSweep = CsvProvider<"./day01.csv", HasHeaders=false, Schema="Depth (int)", ResolutionFolder=ResolutionFolder>

let sonar_sweep =
    let csv = SonarSweep.GetSample()
    csv.Rows |> Seq.map (fun r -> r.Depth)

let growing values =
    Seq.fold
        (fun (prev, count) value ->
            if value > prev then
                (value, count + 1)
            else
                (value, count))
        (Seq.head values, 0)
        (Seq.tail values)
    |> snd

printfn "7 = %d" <| growing sonar_sweep_test_7
printfn "A: %d" <| growing sonar_sweep

let three_little_sums (values: seq<int>) =
    Seq.windowed 3 values |> Seq.map Array.sum

let test =
    three_little_sums sonar_sweep_test_7 |> growing

printfn "5 = %d" test

let part_deux = three_little_sums sonar_sweep |> growing

printfn "B = %d" part_deux
