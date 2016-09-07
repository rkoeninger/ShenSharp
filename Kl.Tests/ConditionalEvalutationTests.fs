namespace Kl.Tests

open NUnit.Framework
open TestCommon

[<TestFixture>]
type ConditionalEvalutationTests() =

    [<Test>]
    member this.``and expression should not eval second argument expression if first eval'd to false``() =
        assertEffect false "(and false (effect))"

    [<Test>]
    member this.``and expression should always eval second argument expression if first eval'd to true``() =
        assertEffect true "(and true (effect))"

    [<Test>]
    member this.``or expression should not eval second argument expression if first eval'd to true``() =
        assertEffect false "(or true (effect))"

    [<Test>]
    member this.``or expression should always eval second argument expression if first eval'd to false``() =
        assertEffect true "(or false (effect))"

    [<Test>]
    member this.``if expression should always eval its condition``() =
        assertEffect true "(if (do (effect) true) 1 2)"

    [<Test>]
    member this.``if expression should always eval consequent when condition eval's to true``() =
        assertEffect true "(if true (effect) 0)"

    [<Test>]
    member this.``if expressions should not eval consequent when condition eval's to false``() =
        assertEffect false "(if false (effect) 0)"
        
    [<Test>]
    member this.``if expression should not eval alternative when condition eval's to true``() =
        assertEffect false "(if true 0 (effect))"

    [<Test>]
    member this.``if expressions should always eval alternative when condition eval's to false``() =
        assertEffect true "(if false 0 (effect))"
