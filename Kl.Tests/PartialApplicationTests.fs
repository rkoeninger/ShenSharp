namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Startup
open TestCommon

[<TestFixture>]
type PartialApplicationTests() =

    [<Test>]
    member this.``primitive functions should be partially applicable``() =
        let env = baseEnv()
        runInEnv env "(defun add4 (A B C D) (+ A (+ B (+ C D))))" |> ignore
        assertEq
            (Int 10)
            (runInEnv env "(let X (add4 1) (let Y (X 2 3) (Y 4)))")
