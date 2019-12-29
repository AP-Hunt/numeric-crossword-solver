namespace Solver.Test

open FsUnit
open Types
open Solvers

module Helpers =    
    let testResult fn result =
        match result with 
        | Ok(x) -> fn x
        | Error(s) -> failwith "expected result to be OK, but got error"

        ()

    let inline (<&>) result fn = testResult fn result

    let expectedAnswer location answer solutions =
        solutions
        <!> Solutions.findSolutionTo location
        <&> fun solution ->
                solution
                |> Solution.answer
                |> should equal answer


    let newFakeDispatcher solutionFixtures =
        let fn (location: Location) solverResult =      
            let answerFixture = solutionFixtures |> Map.find location
            
            solverResult
            <!> Solutions.set ((location, Unknown), Some(answerFixture))
            
        fn