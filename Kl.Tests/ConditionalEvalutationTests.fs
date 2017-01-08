namespace Kl.Tests

open NUnit.Framework
open TestCommon
open Kl
open Kl.Values

[<TestFixture>]
type ConditionalEvalutationTests() =

    [<Test>]
    member this.``and expression``() =
        assertEq truev (run "(and true true)")
        assertEq falsev (run "(and true false)")
        assertEq falsev (run "(and false true)")
        assertEq falsev (run "(and false false)")

    [<Test>]
    member this.``or expression``() =
        assertEq truev (run "(or true true)")
        assertEq truev (run "(or true false)")
        assertEq truev (run "(or false true)")
        assertEq falsev (run "(or false false)")

    [<Test>]
    member this.``if expression``() =
        assertEq (Int 1) (run "(if true 1 2)")
        assertEq (Int 2) (run "(if false 1 2)")

    [<Test>]
    member this.``cond expression``() =
        assertEq (Int 1) (run "(cond (true 1) (true 2) (false 3))")
        assertEq (Int 2) (run "(cond (false 1) (true 2) (false 3))")
        assertEq (Int 3) (run "(cond (false 1) (false 2) (true 3))")
        assertError "(cond (false 1) (false 2) (false 3))"

    [<Test>]
    member this.``and expression should only eval second conditional if first eval'd to true``() =
        assertEffect true "(and true (effect true))"
        assertEffect false "(and false (effect true))"

    [<Test>]
    member this.``or expression should only eval second conditional if first eval'd to false``() =
        assertEffect true "(or false (effect true))"
        assertEffect false "(or true (effect true))"

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
    member this.``cond expressions should only eval consequent in clause where condition was true``() =
        assertEffect true "(cond ((= 0 1) 0) ((= 1 1) (effect 0)) (false false))"
        assertEffect false "(cond (false (effect 0)) (true 0))"
        assertEffect false "(cond (false 1) ((= 1 2) (effect 0)) (true 2))"
