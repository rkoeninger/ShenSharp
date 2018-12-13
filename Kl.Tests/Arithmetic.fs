module Kl.Tests.Arithmetic

open NUnit.Framework
open Kl
open Assertions

[<Test>]
let ``return an integer when adding two integers``() =
    assertInt "(+ 5 3)"

[<Test>]
let ``return a decimal when adding a decimal and an integer``() =
    assertDec "(+ 1.1 2)"
    assertDec "(+ 11 -2.4)"
    
[<Test>]
let ``return a decimal when adding two decimals``() =
    assertDec "(+ 1.1 2.4)"

[<Test>]
let ``return an integer when subtracting two integers``() =
    assertInt "(- 1 2)"

[<Test>]
let ``return a decimal when subtracting an integer from a decimal``() =
    assertDec "(- 1.1 2)"

[<Test>]
let ``return a decimal when subtracting a decimal from an integer``() =
    assertDec "(- 11 -2.4)"

[<Test>]
let ``return a decimal when subtracting two decimals``() =
    assertDec "(- 1.1 2.4)"

[<Test>]
let ``return an integer when multiplying two integers``() =
    assertInt "(* 1 2)"

[<Test>]
let ``return a decimal when multiplying an integer and a decimal``() =
    assertDec "(* 1.1 2)"
    assertDec "(* 11 -2.4)"

[<Test>]
let ``return a decimal when multiplying two decimals``() =
    assertDec "(* 1.1 2.4)"

[<Test>]
let ``return a decimal when dividing any combination of decimals and integers``() =
    assertDec "(/ 1 2)"
    assertDec "(/ 2 1)"
    assertDec "(/ 1.1 2)"
    assertDec "(/ 11 -2.4)"
    assertDec "(/ 1.1 2.4)"

[<Test>]
let ``raise an error when dividing by zero in any combination of decimals and integers``() =
    assertError "(/ 1 0)"
    assertError "(/ 1 0.0)"
    assertError "(/ 10 (5 - 5.0))"

[<Test>]
let ``do not truncate decimal when dividing two integers that are not divisible``() =
    assertEq (Num 1.5m) (run "(/ 3 2)")
