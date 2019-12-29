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
        let fixtures = [
            (2, Across), 100;
        ]
        
        let fakeDispatcher = Helpers.newFakeDispatcher (fixtures |> Map.ofList)

        let question = OneQuarterOfLocation((2, Across))

        let solutions: SolverResult = Ok([
            {Challenge = {Location = (1, Across); Question = question}; Answer = None};
            {Challenge = {Location = (2, Across); Question = Unknown}; Answer = None };
        ])

        { Location = (1, Across); Question = question }
        |> Solvers.oneQuarterOfLocation fakeDispatcher solutions
        |> expectedAnswer (1, Across) (Some(25))
        