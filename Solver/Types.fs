module Types

type Direction =
    | Down
    | Across

let directionToString direction =
    match direction with
    | Down -> "down"
    | Across -> "across"

let locationToString location =
    let (num, dir) = location
    sprintf "%d %s" num (directionToString dir)

type Location = (int * Direction)

type Question =
    | SquaresSequence of List<int>
    | LocationMinusLocation of (Location * Location)
    | OneQuarterOfLocation of Location
    | Unknown

type Challenge = (Location * Question)

module Challenge =
    let location challenge =
        let (loc, _) = challenge
        loc

    let question challenge = 
        let (_, q) = challenge
        q

type Solution = (Challenge * int option)
type Solutions = Solution list

module Solution =
    let answer (sol: Solution) =
        let (_, answer) = sol
        answer

    let location (sol: Solution) =
        let ((loc, _), _) = sol
        loc

    let challenge (sol: Solution) =
        let (c, _) = sol
        c

module Solutions =
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
