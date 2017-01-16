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
            "(set state \"a\")
             (defun order (X Body) (do (set state (cn (value state) (str X))) Body))
             (do (order b ()) (order c ()))
             (value state)"

    [<Test>]
    member this.``operator expression should be evaluated before arguments``() =
        assertEq (Str "abcd") <| runAll
            "(set state \"a\")
             (defun order (X Body) (do (set state (cn (value state) (str X))) Body))
             ((order b (lambda X (lambda Y (+ X Y)))) (order c 3) (order d 5))
             (value state)"

    [<Test>]
    member this.``if operator expression eval's to a symbol, that symbol must resolve to a function, which will be applied``() =
        assertEq (Int 3) (run "((if true + -) 1 2)")
        assertEq (Int -1) (run "((if false + -) 1 2)")
