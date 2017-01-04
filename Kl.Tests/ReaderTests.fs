﻿namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Reader
open Kl.Values
open TestCommon

[<TestFixture>]
type ReaderTests() =

    [<Test>]
    member this.``tokenizer does not handle extra spaces inside of parens``() =
        assertError "( 1 )"

    [<Test>]
    member this.``tokenizer handles extra space inbetween tokens``() =
        assertEq
            (toCons
                [Sym "A"
                 toCons [Sym "B"; Sym "C"]
                 toCons [Sym "X"; toCons [Sym "Y"; Sym "Z"]]
                 toCons [Sym "U"; Sym "V"]])
            (read "(A (B    C) (X  (Y  Z))  (U    V))")

    [<Test>]
    member this.``string literals including line breaks should be read as single token``() =
        let t = readAll "\"long copyright string\n\rwith line breaks\r\n\""
        assertEq 1 t.Length

    [<Test>]
    member this.``string literals can contain single quotes``() =
        // NOTE: string literals in KL cannot contain double quotes as there is no way to escape them
        assertEq (Str "'") (read "\"'\"")

    [<Test>]
    member this.``integers are parsable``() =
        assertEq (Int 45) (read "45")
        assertEq (Int -1439) (read "-1439")
        assertEq (Int 0) (read "0")

    [<Test>]
    member this.``decimals are parsable``() =
        assertEq (Dec 4.25m) (read "4.25")
        assertEq (Dec -345256.6465m) (read "-345256.6465")
        assertEq (Dec 0m) (read "-0.0e0")

    [<Test>]
    member this.``decimal parsing does not capture 'Inf' or similiar``() =
        assertEq (Sym "Inf") (read "Inf")
        assertEq (Sym "-Inf") (read "-Inf")
        assertEq (Sym "+Inf") (read "+Inf")
        assertEq (Sym "Infinity") (read "Infinity")
        assertEq (Sym "-Infinity") (read "-Infinity")
        assertEq (Sym "+Infinity") (read "+Infinity")
        assertEq (Sym "NaN") (read "NaN")
        assertEq (Sym "+NaN") (read "+NaN")
        assertEq (Sym "-NaN") (read "-NaN")