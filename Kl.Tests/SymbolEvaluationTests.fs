namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Reader
open Kl.Startup
open TestCommon

[<TestFixture>]
type SymbolEvaluationTests() =

    [<Test>]
    member this.``symbols support full range of characters``() =
        let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ=-*/+_?$!@~.><&%'#`;:{}"
        for ch in chars do
            let s = string ch
            assertEq (Sym s) (read s)

    [<Test>]
    member this.``symbols not at head of application should be resolved using local scope or be idle``() =
        let env = baseEnv()
        runIn env "(defun inc (x) (+ x 1))" |> ignore
        runIn env "(defun inc' (X) (+ X 1))" |> ignore
        runIn env "(defun hi-sym () hi)" |> ignore
        assertEq (Int 5) (runIn env "(inc 4)")
        assertEq (Int 3) (runIn env "(inc' 2)")
        assertEq (Sym "hi") (runIn env "(hi-sym)")

    [<Test>]
    member this.``if a symbol originally idle ends up in application position, it will be resolved as a global function``() =
        let env = baseEnv()
        runIn env "(defun abc (X) (+ X 1))" |> ignore
        assertEq (Sym "abc") (runIn env "(if false 0 abc)")
        assertEq (Int 2) (runIn env "((if false 0 abc) 1)")

    [<Test>]
    member this.``result of interning a string is equal to symbol with name that is equal to that string``() =
        assertEq (Sym "hi") (run "(intern \"hi\")")

    [<Test>]
    member this.``interned symbols can contain any characters``() =
        assertEq (Sym "@!#$") (run "(intern \"@!#$\")")
        assertEq (Sym "(),[];{}") (run "(intern \"(),[];{}\")")
        assertEq (Sym "   ") (run "(intern \"   \")") // space space space
        
    [<Test>]
    member this.``both symbols starting with or with-out an uppercase letter or non-letter can be idle``() =
        assertEq
            (Cons(Sym "A", Cons(Sym "-->", Cons(Sym "boolean", Empty))))
            (run "(cons A (cons --> (cons boolean ())))")

    [<Test>]
    member this.``a lambda set on a global symbol will not be resolved in an application``() =
        let env = baseEnv()
        runIn env "(set inc (lambda X (+ X 1)))" |> ignore
        assertErrorInEnv env "(inc 5)"

    [<Test>]
    member this.``evaluating a defun results in the defun name as a symbol``() =
        match run "(defun inc (X) (+ 1 X))" with
        | Sym s -> assertEq s "inc"
        | _ -> Assert.Fail "Symbol expected"
