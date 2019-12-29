namespace Solver.Test

open Types
open FsUnit
open NUnit.Framework
open Solvers
open Helpers

[<TestFixture>]
module LocationMinusLocationSolvingTests =

    let toChallenge question =
        (1, Across), question

    [<Test>]
    let ``subtracts the solution at location A from the solution at location B``() =
        let challenge = LocationMinusLocation((4, Across), (5, Across)) |> toChallenge
        let solutions: SolverResult = Ok([
            challenge, None;
            ((4, Across), Unknown), None;
            ((5, Across), Unknown), None
        ])

        let fakeDispatcher (location: Location) solverResult =
            match location with
            | (4, Across) -> solverResult <!> Solutions.set ((location, Unknown), Some(4))
            | (5, Across) -> solverResult <!> Solutions.set ((location, Unknown), Some(2))
            | _ -> Error("unexpected location")

        challenge
        |> Solvers.locationMinusLocation fakeDispatcher solutions
        |> expectedAnswer (1, Across) ((Some(2)))
         


