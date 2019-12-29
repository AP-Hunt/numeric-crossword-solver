module Solutions

open Solution

type Solutions = Solution list

let tryFindSolutionTo location solutions =
    let maybeSolution =
        solutions 
        |> List.tryFind (fun sol -> (sol |> Solution.location) = location)
    
    match maybeSolution with
    | Some(s) -> Ok(s)
    | None -> Error("unable to find solution")

let findSolutionTo location solutions =
    Ok(
        solutions
        |> List.find (fun sol -> (sol |> Solution.location) = location)
    )

let tryFindChallengeFor location solutions =
    let maybeSolution = 
        solutions
        |> List.tryFind (fun sol -> (sol |> Solution.location) = location)
    
    match maybeSolution with
    | Some(solution) -> Ok(solution |> Solution.challenge)
    | None -> Error("unable to find solution")

let findChallengeFor location solutions =
    solutions
    |> List.find (fun sol -> (sol |> Solution.location) = location)
    |> Solution.challenge

let set solution solutions =
    let index = 
        solutions
        |> List.tryFindIndex (fun sol -> 
            (sol |> Solution.location) = (solution |> Solution.location)
        )

    match index with
    | Some(i) -> Ok(solutions.[0..(i-1)] @ [solution] @ solutions.[(i+1)..])
    | None -> Ok(solutions @ [solution])

let findSolutions locations solutions =
    Ok(
        locations 
        |> List.map (fun x -> 
            solutions
            |> List.find (fun sol -> (sol |> Solution.location) = x)
        )
    )
