namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Startup
open Assertions

[<TestFixture>]
type ``Symbol Resolution``() =

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
    member this.``symbols as the values of variables cannot be applied as funtions``() =
        assertError
            "(defun abc (X) (+ X 1))
             (let F abc (F 2))"

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
            (toCons [Sym "A"; Sym "-->"; Sym "boolean"])
            (run "(cons A (cons --> (cons boolean ())))")

    [<Test>]
    member this.``a lambda set on a global symbol will not be resolved in an application``() =
        assertError
            "(set inc (lambda X (+ X 1)))
             (inc 5)"

    [<Test>]
    member this.``evaluating a defun results in the defun name as a symbol``() =
        assertEq (Sym "inc") (run "(defun inc (X) (+ 1 X))")
