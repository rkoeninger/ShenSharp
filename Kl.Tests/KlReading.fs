module Kl.Tests.``KL Reading``

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Make.Reader
open Assertions

[<Test>]
let ``does not handle extra spaces inside of parens``() =
    assertError "( 1 )"

[<Test>]
let ``handles extra space in between expressions``() =
    assertEq
        (toCons
            [Sym "A"
             toCons [Sym "B"; Sym "C"]
             toCons [Sym "X"; toCons [Sym "Y"; Sym "Z"]]
             toCons [Sym "U"; Sym "V"]])
        (read "(A (B    C) (X  (Y  Z))  (U    V))")

[<Test>]
let ``supports full range of characters in symbols``() =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ=-*/+_?$!@~.><&%'#`;:{}"
    for ch in chars do
        let s = string ch
        assertEq (Sym s) (read s)
    assertEq [Sym chars] (readAll chars)

[<Test>]
let ``parses lists into Cons chains``() =
    assertEq (Cons(Int 0, Cons(Int 0, Cons(Int 0, Cons(Int 0, Empty))))) (read "(0 0 0 0)")

[<Test>]
let ``parses empty parens as the Empty list``() =
    assertEq Empty (read "()")

[<Test>]
let ``reads string literals including line breaks as single tokens``() =
    let testString = "\"long copyright string\nwith line breaks\n\""
    assertEq [Str "long copyright string\nwith line breaks\n"] (readAll testString)

[<Test>]
let ``normalizes newline sequences in string literals to \n``() =
    let testString = "\"long copyright string\n\rwith line breaks\r\n\""
    assertEq (Str "long copyright string\n\nwith line breaks\n") (read testString)

[<Test>]
let ``supports single quotes in string literals``() =
    // NOTE: string literals in KL cannot contain double quotes as there is no way to escape them
    assertEq (Str "'") (read "\"'\"")

[<Test>]
let ``supports positive and negative integers in a 32-bit range``() =
    assertEq (Int 45) (read "45")
    assertEq (Int -1439) (read "-1439")
    assertEq (Int 0) (read "0")

[<Test>]
let ``supports decimal literals, including in exponential notation``() =
    assertEq (Num 4.25m) (read "4.25")
    assertEq (Num -345256.6465m) (read "-345256.6465")
    assertEq (Num 0m) (read "-0.0e0")

[<Test>]
let ``does not capture "inf" or similiar when parsing decimals``() =
    assertEq (Sym "Inf") (read "Inf")
    assertEq (Sym "-Inf") (read "-Inf")
    assertEq (Sym "+Inf") (read "+Inf")
    assertEq (Sym "Infinity") (read "Infinity")
    assertEq (Sym "-Infinity") (read "-Infinity")
    assertEq (Sym "+Infinity") (read "+Infinity")
    assertEq (Sym "NaN") (read "NaN")
    assertEq (Sym "+NaN") (read "+NaN")
    assertEq (Sym "-NaN") (read "-NaN")
