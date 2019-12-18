namespace Solver.Test

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Types
open QuestionParser

[<TestFixture>]
module LocationMinusLocationParsingProperties =
    [<Property>]
    let ``extracts the two locations from the text`` () =

        let formatQuestion (locationA: Location) (locationB: Location) =
            let locANum, locADir = locationA
            let locBNum, locBDir = locationB
            sprintf "1 across: %d %A minus %d %A" locANum locADir locBNum locBDir
        
        Arb.generate<Location>
        |> Gen.two
        |> Arb.fromGen
        |> Prop.forAll <| fun(a, b) ->
            let questionText = formatQuestion a b
            let challenge = parseQuestionText questionText

            match challenge with
            | Some (_, LocationMinusLocation (locA, locB)) -> (locA = a) && (locB = b)
            | Some _ -> false
            | None -> false

