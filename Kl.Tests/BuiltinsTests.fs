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
        assertError "(pos \"\" 0)"
        assertError "(pos \"hello\" 5)"

    [<Test>]
    member this.``vector index out of bounds should cause uncaught error``() =
        assertError "(<-address (absvector 0) 0)"

    [<Test>]
    member this.``concatenation with empty string should produce same string``() =
        assertEq (Str "abc") (runIt "(cn \"\" \"abc\")")
        assertEq (Str "abc") (runIt "(cn \"abc\" \"\")")

    [<Test>]
    member this.``string->n and n->string functions should be able to result in same character``() =
        assertEq (Str "Hello") (runIt "(cn (n->string (string->n \"Hello\")) (tlstr \"Hello\"))")
        
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
        
    [<Test>]
    member this.EvalFunction() =
        assertEq
            (Int 3)
            (runIt "(eval-kl (cons + (cons 1 (cons 2 ()))))") // (+ 1 2)

        match runIt "(eval-kl (cons lambda (cons X (cons (cons + (cons 1 (cons X ()))) ()))))" with // (lambda X (+ 1 X))
        | Func f ->
            assertEq
                (Int 5)
                (Values.go(apply Head (baseEnv()).Globals [] f [Int 4]))
        | _ -> Assert.Fail "Function expected"
