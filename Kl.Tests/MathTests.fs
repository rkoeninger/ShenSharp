namespace Kl.Tests

open NUnit.Framework
open Kl
open TestCommon

[<TestFixture>]
type MathTests() =

    [<Test>]
    member this.``adding two integers gives integer``() =
        assertInt(runIt "(+ 5 3)")

    [<Test>]
    member this.``adding decimal and integer gives decimal``() =
        assertDec(runIt "(+ 1.1 2)")
        assertDec(runIt "(+ 11 -2.4)")
    
    [<Test>]
    member this.``adding two decimals gives decimal``() =
        assertDec(runIt "(+ 1.1 2.4)")

    [<Test>]
    member this.``subtracting two integers gives integer``() =
        assertInt(runIt "(- 1 2)")

    [<Test>]
    member this.``subtracting integer from decimal gives decimal``() =
        assertDec(runIt "(- 1.1 2)")

    [<Test>]
    member this.``subtracting decimal from integer gives decimal``() =
        assertDec(runIt "(- 11 -2.4)")

    [<Test>]
    member this.``subtracting two decimals gives decimal``() =
        assertDec(runIt "(- 1.1 2.4)")

    [<Test>]
    member this.``multiplying two integers gives integer``() =
        assertInt(runIt "(* 1 2)")

    [<Test>]
    member this.``multiplying integer and decimal gives decimal``() =
        assertDec(runIt "(* 1.1 2)")
        assertDec(runIt "(* 11 -2.4)")

    [<Test>]
    member this.``multiplying two decimals gives decimal``() =
        assertDec(runIt "(* 1.1 2.4)")

    [<Test>]
    member this.``dividing any combination of two decimals/integers gives decimal``() =
        assertDec(runIt "(/ 1 2)")
        assertDec(runIt "(/ 2 1)")
        assertDec(runIt "(/ 1.1 2)")
        assertDec(runIt "(/ 11 -2.4)")
        assertDec(runIt "(/ 1.1 2.4)")

    [<Test>]
    member this.``dividing two integers does not truncate decimal``() =
        match runIt "(/ 3 2)" with
        | Dec d -> assertEq 1.5m d
        | _ -> Assert.Fail "decimal expected"
