namespace Solver.Test

open FsUnit
open NUnit.Framework
open Solvers
open Helpers
open Types

[<TestFixture>]
module OneQuarterSolvingTests =
    [<Test>]
    let ``divides the answer at the location by 4`` () =
        let fixutres = [
            (2, Across), 100;
        ]
        
        let fakeDispatcher = Helpers.newFakeDispatcher (fixutres |> Map.ofList)

        let challenge = OneQuarterOfLocation((2, Across))

        let solutions: SolverResult = Ok([
            ((1, Across), challenge), None;
            ((2, Across), Unknown), None;
        ])

        ((1, Across), challenge)
        |> Solvers.oneQuarterOfLocation fakeDispatcher solutions
        |> testResult (
            fun solutions ->
                solutions.[0]
                |> Solution.answer
                |> should equal (Some(25))
        )