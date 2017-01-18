namespace Kl.Tests

open NUnit.Framework
open TestCommon
open Kl
open Kl.Extensions
open Kl.Values
open Kl.Startup

[<TestFixture>]
type ConditionalEvalutationTests() =

    let assertEffect eff syntax =
        let env = baseEnv()
        runIn env "(defun effect (X) (do (set *effect* true) X))" |> ignore
        runIn env syntax |> ignore
        match env.Globals.Symbols.GetMaybe "*effect*" with
        | None -> if eff then Assert.Fail "Effect did not occurr" else ()
        | _ -> if not eff then Assert.Fail "Effect should not have occurred" else ()

    [<Test>]
    member this.``and expression evals to true if both argument expressions eval to true``() =
        assertEq truev (run "(and true true)")
        assertEq falsev (run "(and true false)")
        assertEq falsev (run "(and false true)")
        assertEq falsev (run "(and false false)")

    [<Test>]
    member this.``and expression requires conditionals eval to boolean granted they are evaluated``() =
        assertError "(and true ())"
        assertFalse "(and false ())"

    [<Test>]
    member this.``and expression should only eval second conditional if first eval'd to true``() =
        assertEffect true "(and true (effect true))"
        assertEffect false "(and false (effect true))"

    [<Test>]
    member this.``or expression evals to true if one or both argument expressions eval to true``() =
        assertEq truev (run "(or true true)")
        assertEq truev (run "(or true false)")
        assertEq truev (run "(or false true)")
        assertEq falsev (run "(or false false)")

    [<Test>]
    member this.``or expression requires conditionals eval to boolean granted they are evaluated``() =
        assertError "(or false ())"
        assertNoError "(or true ())"

    [<Test>]
    member this.``or expression should only eval second conditional if first eval'd to false``() =
        assertEffect true "(or false (effect true))"
        assertEffect false "(or true (effect true))"

    [<Test>]
    member this.``if expression evals consequent or alternative depending on what condition evals to``() =
        assertEq (Int 1) (run "(if true 1 2)")
        assertEq (Int 2) (run "(if false 1 2)")

    [<Test>]
    member this.``if expression requires condition eval to boolean``() =
        assertError "(if 1 a b)"
        assertError "(if () a b)"
        assertError "(if \"a\" a b)"

    [<Test>]
    member this.``if expression should always eval its condition``() =
        assertEffect true "(if (effect true) 1 2)"

    [<Test>]
    member this.``if expression should only eval consequent when condition eval's to true``() =
        assertEffect true "(if true (effect true) 0)"
        assertEffect false "(if false (effect true) 0)"

    [<Test>]
    member this.``if expression should only eval alternative when condition eval's to false``() =
        assertEffect true "(if false 0 (effect true))"
        assertEffect false "(if true 0 (effect true))"

    [<Test>]
    member this.``cond expression evaluates consequent in clause where condition evals to true``() =
        assertEq (Int 1) (run "(cond (true 1) (true 2) (false 3))")
        assertEq (Int 2) (run "(cond (false 1) (true 2) (false 3))")
        assertEq (Int 3) (run "(cond (false 1) (false 2) (true 3))")

    [<Test>]
    member this.``cond expression evaluates to Empty when no conditions are true``() =
        assertEq Empty (run "(cond (false 1) (false 2) (false 3))")
        assertEq Empty (run "(cond)")

    [<Test>]
    member this.``cond expression requires conditionals eval to boolean granted they are evaluates``() =
        assertError "(cond (0 1) (true 2))"
        assertNoError "(cond (true 1) (() 2))"

    [<Test>]
    member this.``cond expressions should only eval consequent in clause where condition was true``() =
        assertEffect true "(cond ((= 0 1) 0) ((= 1 1) (effect 0)) (false false))"
        assertEffect false "(cond (false (effect 0)) (true 0))"
        assertEffect false "(cond (false 1) ((= 1 2) (effect 0)) (true 2))"
