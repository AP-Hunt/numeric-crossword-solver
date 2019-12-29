namespace Solver.Test

open FsUnit
open NUnit.Framework
open Solvers
open Helpers
open Types

[<TestFixture>]
module NMDigitsOfLocationSumSolvingTests =
    [<Test>]
    let ``sums the first N digits plus the last M digits of the answer at the loation`` () =
        let fixtures = [
            (2, Across), 2244;
        ]
        
        let fakeDispatcher = Helpers.newFakeDispatcher (fixtures |> Map.ofList)

        let n = 2
        let m = 3
        let expected = (2+2) + (2+4+4) // Explicit sum here to make it clear how the sum works
        let challenge = NMDigitsOfLocationSum(n, m, (2, Across))

        let solutions: SolverResult = Ok([
            ((1, Across), challenge), None;
            ((2, Across), Unknown), None;
        ])

        ((1, Across), challenge)
        |> Solvers.nmDigitsOfLocationSum fakeDispatcher solutions
        |> expectedAnswer (1, Across) (Some expected)

    [<Test>]
    let ``returns an error if N is greater than the length of the answer at the location`` () =
        let fixtures = [
            (2, Across), 1;
        ]
        
        let fakeDispatcher = Helpers.newFakeDispatcher (fixtures |> Map.ofList)

        let challenge = NMDigitsOfLocationSum(5, 1, (2, Across))

        let solutions: SolverResult = Ok([
            ((1, Across), challenge), None;
            ((2, Across), Unknown), None;
        ])

        ((1, Across), challenge)
        |> Solvers.nmDigitsOfLocationSum fakeDispatcher solutions
        |> (fun r ->
                match r with
                | Ok(_) -> false
                | Error(_) -> true)
        |> should be True

    [<Test>]
    let ``returns an error if M is greater than the length of the answer at the location`` () =
        let fixtures = [
            (2, Across), 1;
        ]
    
        let fakeDispatcher = Helpers.newFakeDispatcher (fixtures |> Map.ofList)

        let challenge = NMDigitsOfLocationSum(1, 5, (2, Across))

        let solutions: SolverResult = Ok([
            ((1, Across), challenge), None;
            ((2, Across), Unknown), None;
        ])

        ((1, Across), challenge)
        |> Solvers.nmDigitsOfLocationSum fakeDispatcher solutions
        |> (fun r ->
                match r with
                | Ok(_) -> false
                | Error(_) -> true)
        |> should be True