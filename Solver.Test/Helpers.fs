namespace Solver.Test

open NUnit.Framework.Constraints

module Helpers =
    open FsUnit.TopLevelOperators
    open FsUnit.Common
    
    let testResult fn result =
        match result with 
        | Ok(x) -> fn x
        | Error(s) -> failwith "expected result to be OK, but got error"

        ()