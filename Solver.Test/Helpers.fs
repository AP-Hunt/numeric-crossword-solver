namespace Solver.Test

open Types
open Solvers

module Helpers =    
    let testResult fn result =
        match result with 
        | Ok(x) -> fn x
        | Error(s) -> failwith "expected result to be OK, but got error"

        ()


    let newFakeDispatcher solutionFixtures =
        let fn (location: Location) solverResult =      
            let answerFixture = solutionFixtures |> Map.find location
            
            solverResult
            <!> Solutions.set ((location, Unknown), Some(answerFixture))
            

        fn