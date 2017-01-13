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
        assertEach [
            (Sym "inc"),    "(defun inc (x) (+ x 1))"
            (Sym "inc'"),   "(defun inc' (X) (+ X 1))"
            (Sym "hi-sym"), "(defun hi-sym () hi)"
            (Int 5),        "(inc 4)"
            (Int 3),        "(inc' 2)"
            (Sym "hi"),     "(hi-sym)"]

    [<Test>]
    member this.``if a symbol originally idle ends up in application position, it will be resolved as a global function``() =
        assertEach [
            (Sym "abc"), "(defun abc (X) (+ X 1))"
            (Sym "abc"), "(if false 0 abc)"
            (Int 2),     "((if false 0 abc) 1)"]

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
