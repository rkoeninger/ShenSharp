namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Reader
open Kl.Values
open TestCommon

[<TestFixture>]
type ReaderTests() =

    [<Test>]
    member this.``tokenizer does not handle extra spaces inside of parens``() =
        try
            read "( 1 )" |> ignore // should be Cons(Int 1, Empty)
            Assert.Fail "Expected tokenizer fail"
        with
            x -> ()

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
        // NB: string literals in KL cannot contain double quotes as there is no way to escape them
        assertEq (Str "'") (read "\"'\"")

    [<Test>]
    member this.``integers are parsable``() =
        assertEq (Int 45) (read "45")

    [<Test>]
    member this.``decimals are parsable``() =
        assertEq (Dec 4.25m) (read "4.25")
