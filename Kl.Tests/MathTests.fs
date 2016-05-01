namespace Kl.Tests

open NUnit.Framework
open Kl
open TestCommon

[<TestFixture>]
type MathTests() =

    [<Test>]
    member this.``adding two integers gives integer``() =
        assertInt(run "(+ 5 3)")

    [<Test>]
    member this.``adding decimal and integer gives decimal``() =
        assertDec(run "(+ 1.1 2)")
        assertDec(run "(+ 11 -2.4)")
    
    [<Test>]
    member this.``adding two decimals gives decimal``() =
        assertDec(run "(+ 1.1 2.4)")

    [<Test>]
    member this.``subtracting two integers gives integer``() =
        assertInt(run "(- 1 2)")

    [<Test>]
    member this.``subtracting integer from decimal gives decimal``() =
        assertDec(run "(- 1.1 2)")

    [<Test>]
    member this.``subtracting decimal from integer gives decimal``() =
        assertDec(run "(- 11 -2.4)")

    [<Test>]
    member this.``subtracting two decimals gives decimal``() =
        assertDec(run "(- 1.1 2.4)")

    [<Test>]
    member this.``multiplying two integers gives integer``() =
        assertInt(run "(* 1 2)")

    [<Test>]
    member this.``multiplying integer and decimal gives decimal``() =
        assertDec(run "(* 1.1 2)")
        assertDec(run "(* 11 -2.4)")

    [<Test>]
    member this.``multiplying two decimals gives decimal``() =
        assertDec(run "(* 1.1 2.4)")

    [<Test>]
    member this.``dividing any combination of two decimals/integers gives decimal``() =
        assertDec(run "(/ 1 2)")
        assertDec(run "(/ 2 1)")
        assertDec(run "(/ 1.1 2)")
        assertDec(run "(/ 11 -2.4)")
        assertDec(run "(/ 1.1 2.4)")

    [<Test>]
    member this.``dividing two integers does not truncate decimal``() =
        match run "(/ 3 2)" with
        | Dec d -> assertEq 1.5m d
        | _ -> Assert.Fail "decimal expected"
