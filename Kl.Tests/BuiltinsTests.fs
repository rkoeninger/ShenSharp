namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Evaluator
open Kl.Startup
open TestCommon

[<TestFixture>]
type BuiltinsTests() =

    [<Test>]
    member this.``string index out of bounds should cause uncaught error``() =
        assertError """(pos "" 0)"""
        assertError """(pos "hello" 5)"""

    [<Test>]
    member this.``vector index out of bounds should cause uncaught error``() =
        assertError "(<-address (absvector 0) 0)"

    [<Test>]
    member this.``concatenation with empty string should produce same string``() =
        assertEq (Str "abc") (runIt """(cn "" "abc")""")
        assertEq (Str "abc") (runIt """(cn "abc" "")""")

    [<Test>]
    member this.``string->n and n->string functions should be able to result in same character``() =
        assertEq (Str "Hello") (runIt """(cn (n->string (string->n "Hello")) (tlstr "Hello"))""")
        
    [<Test>]
    member this.``vectors should be pre-filled with the symbol 'fail!``() =
        assertEq (Vec [|Sym "fail!"|]) (runIt "(absvector 1)")

    [<Test>]
    member this.``empty values don't count as conses``() =
        assertFalse(runIt "(cons? ())")

    [<Test>]
    member this.``calling hd or tl on anything but a cons should fail``() =
        assertError "(hd 0)"
        assertError "(tl 0)"
        assertError "(hd ())"
        assertError "(tl ())"
        assertNoError "(hd (cons 0 ()))"
        assertNoError "(tl (cons 0 ()))"
    
    [<Test>]
    member this.``eval-kl evaluating a constant should equal that constant``() =
        assertEq (Int 5) (runIt "(eval-kl 5)")
        assertEq (Str "abc") (runIt """(eval-kl "abc")""")
        assertEq (Bool true) (runIt "(eval-kl true)")
        assertEq Empty (runIt "(eval-kl ())")
        assertEq (Sym "abc") (runIt "(eval-kl abc)")

    [<Test>]
    member this.``eval-kl should have the same effect as straight KL that ``() =
        assertEq
            (runIt "(+ 1 2)")
            (runIt "(eval-kl (cons + (cons 1 (cons 2 ()))))")
        assertEq
            (runIt "(if (< 4 5) a b)")
            (runIt "(eval-kl (cons if (cons (cons < (cons 4 (cons 5 ()))) (cons a (cons b ())))))")
