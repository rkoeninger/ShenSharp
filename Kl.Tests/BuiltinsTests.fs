namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Startup
open TestCommon

[<TestFixture>]
type BuiltinsTests() =

    [<Test>]
    member this.``string index out of bounds should raise error``() =
        assertError """(pos "" 0)"""
        assertError """(pos "hello" 5)"""

    [<Test>]
    member this.``vector index out of bounds should raise error``() =
        assertError "(<-address (absvector 0) 0)"
        assertError "(<-address (absvector 3) 5)"

    [<Test>]
    member this.``concatenation with empty string should produce same string``() =
        assertEq (Str "abc") (run """(cn "" "abc")""")
        assertEq (Str "abc") (run """(cn "abc" "")""")

    [<Test>]
    member this.``string->n and n->string functions should be able to result in same character``() =
        assertEq (Str "Hello") (run """(cn (n->string (string->n "Hello")) (tlstr "Hello"))""")

    [<Test>]
    member this.``vectors should be pre-filled with Empty``() =
        assertEq (Vec [|Empty; Empty; Empty|]) (run "(absvector 3)")

    [<Test>]
    member this.``empty values don't count as conses``() =
        assertFalse "(cons? ())"
        assertEq Empty (run "()")

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
        assertEq (Int 5) (run "(eval-kl 5)")
        assertEq (Str "abc") (run """(eval-kl "abc")""")
        assertTrue "(eval-kl true)"
        assertEq Empty (run "(eval-kl ())")
        assertEq (Sym "abc") (run "(eval-kl abc)")

    [<Test>]
    member this.``eval-kl should have the same effect as straight KL that does the same thing``() =
        assertEq
            (run "(+ 1 2)")
            (run "(eval-kl (cons + (cons 1 (cons 2 ()))))")
        assertEq
            (run "(if (< 4 5) a b)")
            (run "(eval-kl (cons if (cons (cons < (cons 4 (cons 5 ()))) (cons a (cons b ())))))")
