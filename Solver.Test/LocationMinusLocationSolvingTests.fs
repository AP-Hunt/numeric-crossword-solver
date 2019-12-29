namespace Solver.Test

open Types
open FsUnit
open NUnit.Framework
open Solvers
open Helpers

[<TestFixture>]
module LocationMinusLocationSolvingTests =

    let toChallenge question =
        { Location = (1, Across); Question = question}

    [<Test>]
    let ``subtracts the solution at location A from the solution at location B``() =
        let challenge = LocationMinusLocation((4, Across), (5, Across)) |> toChallenge
        let solutions: SolverResult = Ok([
            {Challenge = challenge; Answer = None};
            {Challenge = {Location = (4, Across); Question = Unknown}; Answer = None};
            {Challenge = {Location = (5, Across); Question = Unknown}; Answer = None};
        ])

        let fixtures = [
            (4, Across), 4;
            (5, Across), 2;
        ]

        let fakeDispatcher = newFakeDispatcher (fixtures |> Map.ofList)

        challenge
        |> Solvers.locationMinusLocation fakeDispatcher solutions
        |> expectedAnswer (1, Across) ((Some(2)))
         


