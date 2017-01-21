namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Load.Reader
open Kl.Load.Translator

[<TestFixture>]
type TranslatorTests() =

    [<Test>]
    member this.``translates Empty values``() =
        printfn "%s" (encodeSymbol "x" (read "()"))

    [<Test>]
    member this.``translates Num values``() =
        printfn "%s" (encodeSymbol "x" (read "1.23"))

    [<Test>]
    member this.``translates Str values``() =
        printfn "%s" (encodeSymbol "x" (read "\"Hello, World!\""))

    [<Test>]
    member this.``translates Sym values``() =
        printfn "%s" (encodeSymbol "x" (read "hi"))

    [<Test>]
    member this.``translates Cons lists``() =
        printfn "%s" (encodeSymbol "x" (read "(1 (() 3 (\"4\" 5)) 6 (a 8) 9)"))

    [<Test>]
    member this.``translates Vec values``() =
        let array = [|
            Str "abc"
            Sym "shen.fail!"
            Empty
            Sym "shen.fail!"
            Num 3.42m
        |]
        printfn "%s" (encodeSymbol "x" (Vec array))
