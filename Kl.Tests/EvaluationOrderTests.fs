namespace Kl.Tests

open NUnit.Framework
open TestCommon
open Kl
open Kl.Startup

[<TestFixture>]
type EvaluationOrderTests() =

    [<Test>]
    member this.``argument expressions in applications should be evaluated left to right``() =
        assertEq (Str "abc") <| runAll
            "(set foo \"a\")
             (do (set foo (cn (value foo) \"b\")) (set foo (cn (value foo) \"c\")))
             (value foo)"

    [<Test>]
    member this.``operator expression should be evaluated before arguments``() =
        assertEach [
            (Str "a"),    "(set foo \"a\")"
            (Sym "app"),  "(defun app (X) (set foo (cn (value foo) X)))"
            (Int 8),      "((do (app \"b\") (lambda X (lambda Y (+ X Y)))) (do (app \"c\") 3) (do (app \"d\") 5))"
            (Str "abcd"), "(value foo)"]

    [<Test>]
    member this.``if operator expression eval's to a symbol, that symbol must resolve to a function, which will be applied``() =
        assertEq (Int 3) (run "((if true + -) 1 2)")
