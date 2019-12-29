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