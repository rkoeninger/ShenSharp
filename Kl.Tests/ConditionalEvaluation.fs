module Kl.Tests.``Conditional Evaluation``

open NUnit.Framework
open Kl
open Kl.Extensions
open Kl.Values
open Kl.Startup
open Assertions

let assertEffect eff syntax =
    let globals = baseGlobals()
    runIn globals "(defun effect (X) (do (set *effect* true) X))" |> ignore
    runIn globals syntax |> ignore
    match getValueOption (intern globals "*effect*") with
    | None -> if eff then Assert.Fail "Effect did not occurr" else ()
    | _ -> if not eff then Assert.Fail "Effect should not have occurred" else ()

[<Test>]
let ``and expression evals to true if both argument expressions eval to true``() =
    assertEq True (run "(and true true)")
    assertEq False (run "(and true false)")
    assertEq False (run "(and false true)")
    assertEq False (run "(and false false)")

[<Test>]
let ``and expression requires conditionals eval to boolean granted they are evaluated``() =
    assertError "(and true ())"
    assertFalse "(and false ())"

[<Test>]
let ``and expression should only eval second conditional if first eval'd to true``() =
    assertEffect true "(and true (effect true))"
    assertEffect false "(and false (effect true))"

[<Test>]
let ``and expression can be partially applied``() =
    assertFalse "((and false) false)"
    assertFalse "((and false) true)"
    assertFalse "((and true) false)"
    assertTrue "((and true) true)"
    assertTrue "((and) true true)"

[<Test>]
let ``and expression does not do short-circuit evaluation when partially applied``() =
    assertEffect true "((and true) (effect true))"
    assertEffect true "((and false) (effect true))"
    assertEffect true "((and) true (effect true))"

[<Test>]
let ``or expression evals to true if one or both argument expressions eval to true``() =
    assertEq True (run "(or true true)")
    assertEq True (run "(or true false)")
    assertEq True (run "(or false true)")
    assertEq False (run "(or false false)")

[<Test>]
let ``or expression requires conditionals eval to boolean granted they are evaluated``() =
    assertError "(or false ())"
    assertNoError "(or true ())"

[<Test>]
let ``or expression should only eval second conditional if first eval'd to false``() =
    assertEffect true "(or false (effect true))"
    assertEffect false "(or true (effect true))"

[<Test>]
let ``or expression can be partially applied``() =
    assertFalse "((or false) false)"
    assertTrue "((or false) true)"
    assertTrue "((or true) false)"
    assertTrue "((or true) true)"
    assertTrue "((or) true true)"

[<Test>]
let ``or expression does not do short-circuit evaluation when partially applied``() =
    assertEffect true "((or true) (effect true))"
    assertEffect true "((or false) (effect true))"
    assertEffect true "((or) false (effect true))"

[<Test>]
let ``if expression evals consequent or alternative depending on what condition evals to``() =
    assertEq (Int 1) (run "(if true 1 2)")
    assertEq (Int 2) (run "(if false 1 2)")

[<Test>]
let ``if expression requires condition eval to boolean``() =
    assertError "(if 1 a b)"
    assertError "(if () a b)"
    assertError "(if \"a\" a b)"

[<Test>]
let ``if expression should always eval its condition``() =
    assertEffect true "(if (effect true) 1 2)"

[<Test>]
let ``if expression should only eval consequent when condition eval's to true``() =
    assertEffect true "(if true (effect true) 0)"
    assertEffect false "(if false (effect true) 0)"

[<Test>]
let ``if expression should only eval alternative when condition eval's to false``() =
    assertEffect true "(if false 0 (effect true))"
    assertEffect false "(if true 0 (effect true))"

[<Test>]
let ``if expression can be partially applied``() =
    assertEq (Int 1) (run "((if true) 1 2)")
    assertEq (Int 1) (run "((if true 1) 2)")
    assertEq (Int 2) (run "((if false) 1 2)")
    assertEq (Int 2) (run "(((if false) 1) 2)")

[<Test>]
let ``if expression does not do short-circuit evaluation when partially applied``() =
    assertEffect true "((if true) 0 (effect 0))"
    assertEffect true "((if false) (effect 0) 0)"
    assertEffect true "((if) false (effect 0) 0)"
    assertEffect true "((if) true 0 (effect 0))"

[<Test>]
let ``cond expression evaluates consequent in clause where condition evals to true``() =
    assertEq (Int 1) (run "(cond (true 1) (true 2) (false 3))")
    assertEq (Int 2) (run "(cond (false 1) (true 2) (false 3))")
    assertEq (Int 3) (run "(cond (false 1) (false 2) (true 3))")

[<Test>]
let ``cond expression raises an error when no conditions are true``() =
    assertError "(cond (false 1) (false 2) (false 3))"
    assertError "(cond)"

[<Test>]
let ``cond expression requires conditionals eval to boolean granted they are evaluates``() =
    assertError "(cond (0 1) (true 2))"
    assertNoError "(cond (true 1) (() 2))"

[<Test>]
let ``cond expressions should only eval consequent in clause where condition was true``() =
    assertEffect true "(cond ((= 0 1) 0) ((= 1 1) (effect 0)) (false false))"
    assertEffect false "(cond (false (effect 0)) (true 0))"
    assertEffect false "(cond (false 1) ((= 1 2) (effect 0)) (true 2))"
