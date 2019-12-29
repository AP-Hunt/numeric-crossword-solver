module Solution

open Types

type Solution = 
    {
        Challenge: Challenge
        Answer: int option
    }

let answer (sol: Solution) = sol.Answer
let location (sol: Solution) = sol.Challenge.Location
let challenge (sol: Solution) = sol.Challenge

