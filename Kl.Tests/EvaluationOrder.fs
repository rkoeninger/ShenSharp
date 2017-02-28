[<NUnit.Framework.TestFixture>]
module Kl.Tests.``Evaluation Order``

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Startup
open Assertions

[<Test>]
let ``argument expressions in applications should be evaluated left to right``() =
    assertEq (Str "abc") <| runAll
        "(set state \"a\")
         (defun order (X Body) (do (set state (cn (value state) (str X))) Body))
         (do (order b ()) (order c ()))
         (value state)"

[<Test>]
let ``operator expression should be evaluated before arguments``() =
    assertEq (Str "abcd") <| runAll
        "(set state \"a\")
         (defun order (X Body) (do (set state (cn (value state) (str X))) Body))
         ((order b (lambda X (lambda Y (+ X Y)))) (order c 3) (order d 5))
         (value state)"
