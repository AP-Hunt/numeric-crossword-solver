namespace Solver.Test

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Types
open QuestionParser

[<TestFixture>]
module SquaresQuestionParsingProperties =

    [<Property>]
    let ``extracts a list of ints from the text`` () =
        let formatQuestion (ints: int list) =
            if ints.Length = 0 then
                failwith "input was empty"
            elif ints.Length = 1 then
                sprintf "1 across: In order, the squares of %d" ints.Head
            else
                let allButTail = ints.[0..ints.Length-2]
                let last = ints.[ints.Length-1]
                sprintf "1 across: In order, the squares of %s and %d" (allButTail |> List.map (fun x -> x.ToString()) |> String.concat ", ") last

        let intListComparator = fun elem1 elem2 ->
                if elem1 > elem2 then 1
                elif elem1 < elem2 then -1
                else 0

        Arb.generate<DoNotSize<int32>>
        |> Gen.map (fun (DoNotSize x) -> x)
        |> Gen.four
        |> Arb.fromGen
        |> Prop.forAll <| fun (a, b, c, d) ->
            let ints = [a;b;c;d]
            let questionText = formatQuestion ints
            let challenge = parseQuestionText questionText

            match challenge with
            | Some (_, SquaresSequence xs) -> (List.compareWith intListComparator ints xs) = 0
            | Some _ -> false
            | None -> false