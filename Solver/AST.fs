module AST

type Direction =
    | Down
    | Across

type Location = (int * Direction)

type Challenge =
    | SquaresSequence of List<int>
    | LocationMinusLocation of (Location * Location)
    | Unknown