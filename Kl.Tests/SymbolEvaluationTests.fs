namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Startup
open TestCommon

[<TestFixture>]
type SymbolEvaluationTests() =

    [<Test>]
    member this.``symbols not starting with an uppercase letter and not at the head of an application are always idle``() =
        assertEq (Sym "abc") (runIt "(let Id (lambda X X) abc)")
        assertEq (Sym "if") (runIt "(let Id (lambda X X) if)")
        assertEq (Sym "+") (runIt "(let Id (lambda X X) +)")

    [<Test>]
    member this.``symbols starting with an uppercase letter not at the head of an application are idle if not in local scope``() =
        assertEq (Sym "ABC") (runIt "(let Id (lambda X X) ABC)")

    [<Test>]
    member this.``result of interning a string is equal to symbol with name that is equal to that string``() =
        assertEq (Sym "hi") (runIt "(intern \"hi\")")

    [<Test>]
    member this.``interned symbols can contain any characters``() =
        assertEq (Sym "@!#$") (runIt "(intern \"@!#$\")")
        assertEq (Sym "(),[];{}") (runIt "(intern \"(),[];{}\")")
        assertEq (Sym "   ") (runIt "(intern \"   \")") // space space space
        
    [<Test>]
    member this.``both symbols starting with or with-out an uppercase letter or non-letter can be idle``() =
        assertEq
            (Cons(Sym "A", Cons(Sym "-->", Cons(Sym "boolean", Empty))))
            (runIt "(cons A (cons --> (cons boolean ())))")
