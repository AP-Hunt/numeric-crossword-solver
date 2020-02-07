namespace Solver.Test

open NUnit.Framework
open FsUnit
open Helpers


[<TestFixture>]
module GridTests =

    [<TestFixture>]
    module parse =
        [<Test>]
        let ``each line in the input represents one row in the grid`` () =
            let input = [| "0"; "0"; |]

            input 
            |> Grid.parse
            |> extractOK
            |> Array2D.length1
            |> should equal 2

        [<Test>]
        let ``each number in a line is converted to an int`` () = 
            let grid = 
                [|
                    "10";
                    "20"
                |]
                |> Grid.parse
                |> extractOK
        
            grid.[0, 0] 
            |> should equal (Some 10)

            grid.[1, 0]
            |> should equal (Some 20)

        [<Test>]
        let ``eaech number is separated by a pipe`` () =
            let grid =
                [|
                    "10|20";
                    "30|40";
                |]
                |> Grid.parse
                |> extractOK

            grid.[0, 0] |> should equal (Some 10)
            grid.[0, 1] |> should equal (Some 20)
            grid.[1, 0] |> should equal (Some 30)
            grid.[1, 1] |> should equal (Some 40)

        [<Test>]
        let ``empty spaces are converted to None`` () =
            let grid =
                [|
                    "10||20";
                    "30|40|50";
                |]
                |> Grid.parse
                |> extractOK

            grid.[0,0] |> should equal (Some 10)
            grid.[0,1] |> should equal (None)
            grid.[0,2] |> should equal (Some 20)
            grid.[1,0] |> should equal (Some 30)
            grid.[1,1] |> should equal (Some 40)
            grid.[1,2] |> should equal (Some 50)

        [<Test>]
        let ``grids rows must be equally wide`` () =
            let grid = 
                [|
                    "10";
                    "20|30";
                |]
                |> Grid.parse

            match grid with
            | Error(s) -> s |> should not' (be Empty)
            | Ok(_) -> failwith "should have been an error type"
        