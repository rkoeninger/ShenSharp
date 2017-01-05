namespace Kl.Tests

open NUnit.Framework
open TestCommon

[<TestFixture>]
type ConditionalEvalutationTests() =

    [<Test>]
    member this.``and expression should only eval second conditional if first eval'd to true``() =
        assertEffect true "(and true (effect))"
        assertEffect false "(and false (effect))"

    [<Test>]
    member this.``or expression should only eval second conditional if first eval'd to false``() =
        assertEffect true "(or false (effect))"
        assertEffect false "(or true (effect))"

    [<Test>]
    member this.``if expression should always eval its condition``() =
        assertEffect true "(if (do (effect) true) 1 2)"

    [<Test>]
    member this.``if expression should only eval consequent when condition eval's to true``() =
        assertEffect true "(if true (effect) 0)"
        assertEffect false "(if false (effect) 0)"

    [<Test>]
    member this.``if expression should only eval alternative when condition eval's to false``() =
        assertEffect true "(if false 0 (effect))"
        assertEffect false "(if true 0 (effect))"

    [<Test>]
    member this.``cond expressions should only eval consequent in clause where condition was true``() =
        assertEffect true "(cond ((= 0 1) 0) ((= 1 1) (effect)) (false false))"
        assertEffect false "(cond (false (effect)) (true 0))"
        assertEffect false "(cond (false 1) ((= 1 2) (effect)) (true 2))"
