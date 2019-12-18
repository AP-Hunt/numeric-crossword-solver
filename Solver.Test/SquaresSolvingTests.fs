namespace Solver.Test

open Types
open FsUnit
open NUnit.Framework
open Solvers
open Helpers

[<TestFixture>]
module SquaresSolvingTests =

    let toChallenge question =
        (1, Across), question

    let solverResult: SolverResult = Ok([])

    [<Test>]
    let ``squares a single input``() =

        SquaresSequence([2])
        |> toChallenge
        |> Solvers.squareSequence solverResult
        <!> Solutions.findSolutionTo (1, Across)
        |> testResult (
            fun solution -> 
                solution 
                |> Solution.answer
                |> should equal (Some(4))
            )

    [<Test>]
    let ``concatenates characters in the squares of the inputs, and converts back to a number`` () =
        SquaresSequence([9; 9])
        |> toChallenge
        |> Solvers.squareSequence solverResult
        <!> Solutions.findSolutionTo (1, Across)
        |> testResult (
            fun solution -> 
                solution 
                |> Solution.answer
                |> should equal (Some(8181))
            )
