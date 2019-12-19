module Solvers

open Types
open System

type SolverResult = Result<Solutions, string>
let inline (>=>) fn x = Result.bind fn x
let apply fn x =
    match x with 
    | Ok(y) -> fn y
    | Error(z) -> Error(z)

let inline (<!>) x fn  = apply fn x
let inline (>>*) fn x = Ok(fn x)

let unwrap fn x =
    match x with
    | Ok(y) -> fn y
    | Error(s) -> failwith s

let inline (!>) x fn = unwrap fn x

type SolverDispatcher = Location->SolverResult->SolverResult

let private badQuestionTypeMsg solverName question =
        sprintf "%s cannot solve a question of type %s" solverName (question.GetType().FullName)

let private setSolutionAnswer challenge solutions answer =
    solutions
    <!> Solutions.set (challenge, answer)

let private solveDependents solverDispatcher solverResult dependentLocations =
    let folder result elem =
        result
        |> solverDispatcher elem

    dependentLocations
    |> List.fold folder solverResult
    

let squareSequence solverResult (challenge: Challenge) =
    let (_, q) = challenge
    
    match q with
    | SquaresSequence ints -> 
        let concattedInts =
            ints
            |> List.map (fun x -> x*x)
            |> List.map (fun x -> x.ToString())
            |> String.concat ""
            |> Convert.ToInt32

        solverResult
        <!> Solutions.set (challenge, Some(concattedInts))
        
      
    | _ -> Error (badQuestionTypeMsg "squareSequence" q)

let locationMinusLocation (solverDispatcher: SolverDispatcher) solverResult (challenge: Challenge) =
    match challenge with
    | _, LocationMinusLocation(a, b) ->
        let extractChallengeTuple solutions =
            let solutionA::solutionB::_ = solutions
            Ok((solutionA, solutionB))

        let subtractAFromB challengeTuple =
            let (a, b) = challengeTuple
            let a' = a |> Solution.answer
            let b' = b |> Solution.answer

            match (a', b') with 
            | (Some(x), Some(y)) -> Ok(Some(x-y))
            | _ -> Error("either a or b was empty")

        let solutionsAfterSolvingDependents = 
            [a;b]
            |> solveDependents solverDispatcher solverResult 

        solutionsAfterSolvingDependents
        <!> (Solutions.findSolutions [a;b]) 
        <!> extractChallengeTuple
        <!> subtractAFromB
        <!> setSolutionAnswer challenge solutionsAfterSolvingDependents

    | _, q -> Error (badQuestionTypeMsg "locationMinusLocation" q)

let oneQuarterOfLocation (solverDispatcher: SolverDispatcher) solverResult challenge =
    match challenge with
    | (_, OneQuarterOfLocation(location)) ->
        let divideAnswerByFour solution =
            let answer = (solution |> Solution.answer).Value
            Ok(Some((answer/4)))



        let solverAfterSolvingDependents = 
            [location]
            |> solveDependents solverDispatcher solverResult
 
        solverAfterSolvingDependents
        <!> (Solutions.findSolutionTo location)
        <!> divideAnswerByFour
        <!> setSolutionAnswer challenge solverAfterSolvingDependents


    | (_, q) -> Error(badQuestionTypeMsg "oneQuarterOfLocation" q)

let rec solverDispatcher location solverResult : SolverResult =   
    solverResult
    <!> fun(solutions) ->
        let maybeSolution = solutions |> Solutions.tryFindSolutionTo location
        match maybeSolution with
        | Error(str) -> Error(str)
        | Ok(solution) ->
            let isSolved = solution |> Solution.answer |> Option.isSome

            if isSolved = true then
                solverResult
            else
                let challenge = solution |> Solution.challenge
                match challenge with
                | _, (SquaresSequence _)            -> squareSequence solverResult challenge
                | _, (LocationMinusLocation (_, _)) -> locationMinusLocation (solverDispatcher) solverResult challenge
                | _, (OneQuarterOfLocation(_))      -> oneQuarterOfLocation (solverDispatcher) solverResult challenge
                | _, (Unknown)                      -> Error("unknown challenge")