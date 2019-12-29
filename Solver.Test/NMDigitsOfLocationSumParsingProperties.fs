namespace Solver.Test

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Types
open QuestionParser

[<TestFixture>]
module NMDigitsOfLocationSumParsingProperties =
    let formatQuestion n m location =
        sprintf "1 across: first %d digits of %s plus last %d" n (locationToString location) m
    

    [<Property>]
    let ``extracts the location from the text`` () =
        Arb.generate<(int * int * Location)>
        |> Arb.fromGen
        |> Prop.forAll <| fun(n, m, loc) ->
            let questionText = formatQuestion n m loc
            let challenge = parseQuestionText questionText

            match challenge with
            | Some (_, NMDigitsOfLocationSum (_, _, loc')) -> (loc' = loc)
            | Some _ -> false
            | None -> false

    [<Property>]
    let ``extracts N value from the text`` ()=
        Arb.generate<(int * int * Location)>
        |> Arb.fromGen
        |> Prop.forAll <| fun(n, m, loc) ->
            let questionText = formatQuestion n m loc
            let challenge = parseQuestionText questionText

            match challenge with
            | Some (_, NMDigitsOfLocationSum (n', _, _)) -> (n' = n)
            | Some _ -> false
            | None -> false

    [<Property>]
    let ``extracts M value from the text`` ()=
        Arb.generate<(int * int * Location)>
        |> Arb.fromGen
        |> Prop.forAll <| fun(n, m, loc) ->
            let questionText = formatQuestion n m loc
            let challenge = parseQuestionText questionText

            match challenge with
            | Some (_, NMDigitsOfLocationSum (_, m', _)) -> (m' = m)
            | Some _ -> false
            | None -> false