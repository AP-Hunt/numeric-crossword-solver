namespace Solver.Test

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Types
open QuestionParser

[<TestFixture>]
module LocationParsingProperties =

    [<Property>]
    let ``extracts a number and direction from a location`` () =
        let formatQuestion (location: Location) =
            let num, dir = location
            sprintf "%d %s: In order, the squares of 1" num (directionToString dir)

        Arb.generate<Location>
        |> Arb.fromGen
        |> Prop.forAll <| fun loc ->
            let questionText = formatQuestion loc
            let challenge = parseQuestionText questionText

            match challenge with
            | Some (location, _) -> location = loc
            | None -> false