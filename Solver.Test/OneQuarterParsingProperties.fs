namespace Solver.Test

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Types
open QuestionParser

[<TestFixture>]
module OneQuarterParsingProperties =

    [<Property>]
    let ``extracts the location from the text`` () =
        let formatQuestion location =
            sprintf "1 across: One-quarter of %s" (location |> locationToString)

        Arb.generate<Location>
        |> Arb.fromGen
        |> Prop.forAll <| fun(input) ->
            let questionText = formatQuestion input
            let challenge = parseQuestionText questionText

            match challenge with
            | Some(_, OneQuarterOfLocation(loc)) -> loc = input
            | _ -> false
