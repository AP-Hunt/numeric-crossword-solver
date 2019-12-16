module ChallengeGraph

open AST

type ChallengeGraph =
    | Branch of (Location * Challenge * Location list)
    | Leaf of (Location * Challenge)