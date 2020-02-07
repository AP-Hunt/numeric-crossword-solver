module Grid

open System

type Grid = int[][]

let parse gridLines =
    //Array2D.create<int> (gridLines |> Array.length) 0 0
    let splitLines (line:string) =
        line.Split '|'
        |> Array.map (fun x -> x.Trim())
    
    let parseNumber (str:string) =
        match str with
        | "" -> None
        | " " -> None
        | x ->
            Some(System.Convert.ToInt32 x)

    let parseLine (line:string[]) =
        line
        |> Array.map parseNumber

    let validateRectangularArray array =
        let longest = array |> Array.map (Array.length) |> Array.max

        array
        |> Array.forall (fun line -> (line.Length) = longest)
   
    let numericLine =
        gridLines
        |> Array.map splitLines
        |> Array.map parseLine

    match (validateRectangularArray numericLine) with
    | true -> Ok(array2D numericLine)
    | false -> Error("grid is not a rectangular array")