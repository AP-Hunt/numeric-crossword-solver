module Solver

open Types
open Solvers

let rec solveAllRec (dispatcher: SolverDispatcher) (solverResult: SolverResult) (index: int) =
    match solverResult with 
    | Error(s) -> Error(s)
    | Ok(solutions) -> 
        if index >= solutions.Length then
            solverResult
        else
            let isSolved solution =
                solution
                |> Solution.answer
                |> Option.isSome

            let s = solutions.[index]

            if isSolved s = true then
                solveAllRec dispatcher solverResult (index+1)
            else
                let location = s |> Solution.location
           
                solveAllRec dispatcher (dispatcher location solverResult) (index+1)

let solveAll (dispatcher: Solvers.SolverDispatcher) challenges =
    let solutions: Solution list =
        challenges
        |> List.map(fun challenge -> (challenge, None))

    let initialResult: SolverResult = Ok(solutions)

    solveAllRec dispatcher initialResult 0