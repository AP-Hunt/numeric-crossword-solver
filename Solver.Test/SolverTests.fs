namespace Solver.Test

open Types
open Solver
open FsUnit
open NUnit.Framework
open Helpers

[<TestFixture>]
module SolverTests =
    
    let unwrapSome x =
        match x with
        | Some y -> y
        | _ -> failwith "expected option to be Some, got None"

    [<Test>]
    let ``a single challenge with no dependencies can be solved`` () =
        let challenge = {Location = (1, Across); Question = SquaresSequence([1;2])}
        
        solveAll Solvers.solverDispatcher [challenge]
        |> testResult(
            fun solutions ->
                solutions.Length |> should equal 1
        
                solutions.[0]
                |> Solution.answer
                |> Option.isNone
                |> should not' (be True)
        )

    [<Test>]
    let ``a challenge with dependencies can be solved along with its dependencies`` () =
        let oneAcross = (1, Across)
        let twoDown = (2, Down)
        let threeAcross = (3, Across)

        let challenges = [
            { Location = oneAcross; Question = SquaresSequence([2; 2])};
            { Location = twoDown; Question = SquaresSequence([9])};
            { Location = threeAcross; Question = LocationMinusLocation((2, Down), (1, Across))};
        ]
        
        solveAll Solvers.solverDispatcher challenges
        |> testResult (
            fun solutions -> 
                solutions.Length |> should equal 3

                [oneAcross; twoDown; threeAcross]
                |> List.iter (fun x ->
                    solutions 
                    |> Solutions.tryFindSolutionTo x 
                    |> testResult(fun s -> 
                        s
                        |> Solution.answer
                        |> Option.isSome
                        |> should be True
                    )
                )

                solutions
                |> Solutions.tryFindSolutionTo oneAcross
                |> testResult(
                    fun solution ->
                        solution
                        |> Solution.answer 
                        |> should equal (Some 44)
                )
        

                solutions
                |> Solutions.tryFindSolutionTo twoDown
                |> testResult(
                    fun solution ->
                        solution
                        |> Solution.answer 
                        |> should equal (Some 81)
                )

                solutions
                |> Solutions.tryFindSolutionTo threeAcross
                |> testResult(
                    fun solution ->
                        solution
                        |> Solution.answer 
                        |> should equal (Some 37)
                )
        )