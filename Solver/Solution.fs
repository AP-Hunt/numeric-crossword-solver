module Solution

open Types

type Solution = (Challenge * int option)

let answer (sol: Solution) =
    let (_, answer) = sol
    answer

let location (sol: Solution) =
    let ((loc, _), _) = sol
    loc

let challenge (sol: Solution) =
    let (c, _) = sol
    c

