namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open TestCommon

[<TestFixture>]
type MathTests() =

    let assertDec expr =
        match run expr with
        | Num _ -> ()
        | _ -> Assert.Fail "Decimal expected"

    let assertInt expr =
        match run expr with
        | Int _ -> ()
        | _ -> Assert.Fail "Int expected"

    [<Test>]
    member this.``adding two integers gives integer``() =
        assertInt "(+ 5 3)"

    [<Test>]
    member this.``adding decimal and integer gives decimal``() =
        assertDec "(+ 1.1 2)"
        assertDec "(+ 11 -2.4)"
    
    [<Test>]
    member this.``adding two decimals gives decimal``() =
        assertDec "(+ 1.1 2.4)"

    [<Test>]
    member this.``subtracting two integers gives integer``() =
        assertInt "(- 1 2)"

    [<Test>]
    member this.``subtracting integer from decimal gives decimal``() =
        assertDec "(- 1.1 2)"

    [<Test>]
    member this.``subtracting decimal from integer gives decimal``() =
        assertDec "(- 11 -2.4)"

    [<Test>]
    member this.``subtracting two decimals gives decimal``() =
        assertDec "(- 1.1 2.4)"

    [<Test>]
    member this.``multiplying two integers gives integer``() =
        assertInt "(* 1 2)"

    [<Test>]
    member this.``multiplying integer and decimal gives decimal``() =
        assertDec "(* 1.1 2)"
        assertDec "(* 11 -2.4)"

    [<Test>]
    member this.``multiplying two decimals gives decimal``() =
        assertDec "(* 1.1 2.4)"

    [<Test>]
    member this.``dividing any combination of two decimals/integers gives decimal``() =
        assertDec "(/ 1 2)"
        assertDec "(/ 2 1)"
        assertDec "(/ 1.1 2)"
        assertDec "(/ 11 -2.4)"
        assertDec "(/ 1.1 2.4)"

    [<Test>]
    member this.``division by decimal/integer zero raises an error``() =
        assertError "(/ 1 0)"
        assertError "(/ 1 0.0)"
        assertError "(/ 10 (5 - 5.0))"

    [<Test>]
    member this.``dividing two integers does not truncate decimal``() =
        assertEq (Num 1.5m) (run "(/ 3 2)")
