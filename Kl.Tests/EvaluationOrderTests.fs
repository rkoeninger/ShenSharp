namespace Kl.Tests

open NUnit.Framework
open TestCommon
open Kl
open Kl.Startup

[<TestFixture>]
type EvaluationOrderTests() =
    
    [<Test>]
    member this.``argument expressions in applications should be evaluated left to right``() =
        let env = baseEnv()
        runIn env "(defun do (X Y) Y)" |> ignore
        runIn env "(set foo \"a\")" |> ignore
        runIn env "(do (set foo (cn (value foo) \"b\")) (set foo (cn (value foo) \"c\")))" |> ignore
        assertEq (Str "abc") (runIn env "(value foo)")

    [<Test>]
    member this.``operator expression should be evaluated before arguments``() =
        let env = baseEnv()
        runIn env "(defun do (X Y) Y)" |> ignore
        runIn env "(set foo \"a\")" |> ignore
        runIn env "(defun app (X) (set foo (cn (value foo) X)))" |> ignore
        assertEq (Int 8) (runIn env "((do (app \"b\") (lambda X (lambda Y (+ X Y)))) (do (app \"c\") 3) (do (app \"d\") 5))")
        assertEq (Str "abcd") (runIn env "(value foo)")

    [<Test>]
    member this.``if operator expression eval's to a symbol, that symbol must resolve to a function, which will be applied``() =
        assertEq (Int 3) (run "((if true + -) 1 2)")
