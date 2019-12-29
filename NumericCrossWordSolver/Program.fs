// Learn more about F# at http://fsharp.org

open System
open Types

let squares num direction (xs: int list) =
    let location = (num, direction) |> locationToString
    if xs.Length = 0 then
        failwith "input was empty"
    elif xs.Length = 1 then
        sprintf "%s: In order, the squares of %d" location xs.Head
    else
        let allButTail = xs.[0..xs.Length-2]
        let last = xs.[xs.Length-1]
        sprintf "%s: In order, the squares of %s and %d" location (allButTail |> List.map (fun x -> x.ToString()) |> String.concat ", ") last

let locationMinusLocation num dir a b =
    sprintf "%s: %s minus %s" 
        (locationToString (num, dir))
        (locationToString a)
        (locationToString b)

let formatSolution solution =
    let location = solution |> Solution.location
    let answer = (solution |> Solution.answer).Value
    sprintf "%s: %d" (location |> locationToString) answer

[<EntryPoint>]
let main argv =
    let statements = [
        locationMinusLocation 7 Across (5, Across) (6, Down)
        squares 1 Across [4; 5; 6]
        squares 4 Down [10; 2; 3;]
        squares 2 Down [7; 6;]
        squares 3 Across [22]
        locationMinusLocation 5 Across (1, Across) (4, Down)
        locationMinusLocation 6 Down (3, Across) (4, Down)
        "8 across: One-quarter of 5 across"
        "9 down: first 2 digits of 4 down plus last 3"
     ]

    printfn "Questions ===="
    statements
    |> List.iter (printfn "%s")

    Console.WriteLine()
    Console.WriteLine()

    let challengeOpts = 
        statements
        |> List.map QuestionParser.parseQuestionText

    match (challengeOpts |> List.exists (fun x -> x |> Option.isNone)) with
    | true -> 
        printfn "Unable to parse all statements"
        Console.ReadLine() |> ignore
        1
    | false -> 
        let challenges =
            challengeOpts
            |> List.map (fun x -> x.Value)

        let result = Solver.solveAll Solvers.solverDispatcher challenges

        match result with
        | Error(s) -> 
            printfn "Error'd solving: %s" s
            Console.ReadLine() |> ignore
            1
        | Ok(solutions) ->
            printfn "Answers ==="

            solutions
            |> List.map formatSolution
            |> List.iter (printfn "%s")
            
            Console.ReadLine() |> ignore

            0
