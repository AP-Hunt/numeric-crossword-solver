namespace Solver.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =
    
    [<TestMethod>]
    member this.OneIsOne() =
        Assert.AreEqual(1, 1)
