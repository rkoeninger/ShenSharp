namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Tokenizer
open TestCommon

[<TestFixture>]
type TokenizerTests() =

    [<Test>]
    member this.``tokenizer does not handle extra spaces inside of parens``() =
        try
            tokenize "( 1 )" |> ignore // should be (ComboToken [IntToken 1])
            Assert.Fail "Expected tokenizer fail"
        with
            x -> ()

    [<Test>]
    member this.``tokenizer handles extra space inbetween tokens``() =
        assertEq
            (ComboToken
                [SymToken "A"
                 ComboToken [SymToken "B"; SymToken "C"]
                 ComboToken [SymToken "X"; ComboToken [SymToken "Y"; SymToken "Z"]]
                 ComboToken [SymToken "U"; SymToken "V"]])
            (tokenize "(A (B    C) (X  (Y  Z))  (U    V))")

    [<Test>]
    member this.``string literals including line breaks should be read as single token``() =
        let t = tokenizeAll "\"long copyright string\n\rwith line breaks\r\n\""
        assertEq 1 t.Length

    [<Test>]
    member this.``string literals can contain single quotes``() =
        // NB: string literals in KL cannot contain double quotes as there is no way to escape them
        assertEq (StrToken "'") (tokenize "\"'\"")
