namespace Kl.Tests

open NUnit.Framework
open FParsec
open Kl.Extensions
open Kl
open Kl.Tokenizer
open Kl.Parser
open Kl.Evaluator
open Kl.Builtins
open Kl.Startup
open System.Reflection
open System
open System.IO

[<TestFixture>]
type KlTests() =

    let runInEnv env syntax = rootEval env.Globals (rootParse(tokenize syntax))
    let runIt = runInEnv (baseEnv())
    let assertEq (expected: 'a) (actual: 'a) = Assert.AreEqual(expected, actual)
    let assertTrue = assertEq Values.truev
    let assertFalse = assertEq Values.falsev
    let assertDec value =
        match value with
        | Dec _ -> ()
        | _ -> Assert.Fail "Decimal expected"
    let assertInt value =
        match value with
        | Int _ -> ()
        | _ -> Assert.Fail "Int expected"
    let assertError syntax =
        try
            runIt syntax |> ignore
            Assert.Fail "Error expected"
        with
            _ -> ()
    let assertEffect eff syntax =
        let env = baseEnv()
        runInEnv env "(defun do (X Y) Y)" |> ignore
        runInEnv env "(defun effect () (set *effect* true))" |> ignore
        runInEnv env syntax |> ignore
        match env.Globals.Symbols.GetMaybe "*effect*" with
        | None -> if eff then Assert.Fail "Effect did not occurr" else ()
        | _ -> if not eff then Assert.Fail "Effect should not have occurred" else ()

    [<Test>]
    member this.TokenizerTest() =
        let tryit = tokenize >> ignore

        tryit "abc"
        tryit "123"
        tryit "()"
        tryit "(a)"
        tryit "(a b)"
        tryit "((a b) c (d e f) ((a) ()) c (d f))"
        tryit "(a     b)"
        tryit "(a   \" b c \"  d)"
        let all = tokenizeAll "\"long copyright string\n\rwith line breaks\r\n\"\r\n\r\n(defun form () (stuff 0))\r\n\r\n(defun form2 (a b) (+ a b))"
        Assert.AreEqual(3, all.Length)

    [<Test>]
    member this.``and expression should not eval second argument expression if first eval'd to false``() =
        assertEffect false "(and false (effect))"

    [<Test>]
    member this.``and expression should always eval second argument expression if first eval'd to true``() =
        assertEffect true "(and true (effect))"

    [<Test>]
    member this.``or expression should not eval second argument expression if first eval'd to true``() =
        assertEffect false "(or true (effect))"

    [<Test>]
    member this.``or expression should always eval second argument expression if first eval'd to false``() =
        assertEffect true "(or false (effect))"

    [<Test>]
    member this.``if expression should always eval its condition``() =
        assertEffect true "(if (do (effect) true) 1 2)"

    [<Test>]
    member this.``if expression should always eval consequent when condition eval's to true``() =
        assertEffect true "(if true (effect) 0)"

    [<Test>]
    member this.``if expressions should not eval consequent when condition eval's to false``() =
        assertEffect false "(if false (effect) 0)"
        
    [<Test>]
    member this.``if expression should not eval alternative when condition eval's to true``() =
        assertEffect false "(if true 0 (effect))"

    [<Test>]
    member this.``if expressions should always eval alternative when condition eval's to false``() =
        assertEffect true "(if false 0 (effect))"

    [<Test>]
    member this.DefunAndResolutionOfDefinedFunctions() =
        let env = Values.newEnv()
        runInEnv env "(defun not (X) (if X false true))" |> ignore
        runInEnv env "(defun xor (L R) (or (and L (not R)) (and (not L) R)))" |> ignore
        assertTrue(runInEnv env "(xor true false)")

    [<Test>]
    member this.SymbolResolution() =
        let klIsSymbol _ args =
            match args with
            | [Sym _] -> Values.truev
            | [_] -> Values.falsev
            | _ -> Values.err "symbol? only takes 1 argument"

        let klId _ args =
            match args with
            | [x] -> x
            | _ -> Values.err "id only takes 1 argument"

        let env = baseEnv()
        env.Globals.Functions.["symbol?"] <- Values.primitivev "symbol?" 1 klIsSymbol
        env.Globals.Functions.["id"] <- Values.primitivev "id" 1 klId
        assertTrue(runInEnv env "(symbol? run)")
        assertTrue(runInEnv env "(= run (id run))")

    [<Test>]
    member this.``result of interning a string is equal to symbol with name that is equal to that string``() =
        assertEq (Sym "hi") (runIt "(intern \"hi\")")

    [<Test>]
    member this.``primitive functions should be partially applicable``() =
        let env = baseEnv()
        runInEnv env "(defun add4 (A B C D) (+ A (+ B (+ C D))))" |> ignore
        assertEq
            (Int 10)
            (runInEnv env "(let X (add4 1) (let Y (X 2 3) (Y 4)))")

    [<Test>]
    member this.``adding two integers gives integer``() =
        assertInt(runIt "(+ 5 3)")

    [<Test>]
    member this.``adding decimal and integer gives decimal``() =
        assertDec(runIt "(+ 1.1 2)")
        assertDec(runIt "(+ 11 -2.4)")
    
    [<Test>]
    member this.``adding two decimals gives decimal``() =
        assertDec(runIt "(+ 1.1 2.4)")

    [<Test>]
    member this.``subtracting two integers gives integer``() =
        assertInt(runIt "(- 1 2)")

    [<Test>]
    member this.``subtracting integer from decimal gives decimal``() =
        assertDec(runIt "(- 1.1 2)")

    [<Test>]
    member this.``subtracting decimal from integer gives decimal``() =
        assertDec(runIt "(- 11 -2.4)")

    [<Test>]
    member this.``subtracting two decimals gives decimal``() =
        assertDec(runIt "(- 1.1 2.4)")

    [<Test>]
    member this.``multiplying two integers gives integer``() =
        assertInt(runIt "(* 1 2)")

    [<Test>]
    member this.``multiplying integer and decimal gives decimal``() =
        assertDec(runIt "(* 1.1 2)")
        assertDec(runIt "(* 11 -2.4)")

    [<Test>]
    member this.``multiplying two decimals gives decimal``() =
        assertDec(runIt "(* 1.1 2.4)")

    [<Test>]
    member this.``dividing any combination of two decimals/integers gives decimal``() =
        assertDec(runIt "(/ 1 2)")
        assertDec(runIt "(/ 2 1)")
        assertDec(runIt "(/ 1.1 2)")
        assertDec(runIt "(/ 11 -2.4)")
        assertDec(runIt "(/ 1.1 2.4)")

    [<Test>]
    member this.StringFunctions() =
        assertEq (Str "Hello, World!") (runIt "(cn \"Hello, \" \"World!\")")
        assertEq (Str "Hello") (runIt "(cn (n->string (string->n \"Hello\")) (tlstr \"Hello\"))")
        assertEq (Str "1") (runIt "(str 1)")
        
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

    [<Test>]
    member this.``idle symbols``() =
        assertEq
            (Cons(Sym "A", Cons(Sym "-->", Cons(Sym "boolean", Empty))))
            (runIt "(cons A (cons --> (cons boolean ())))")

    [<Test>]
    member this.``lambda expressions should capture local variables``() =
        assertEq (Int 1) (runIt "(let X 1 (let F (lambda Y X) (F 0)))")

    [<Test>]
    member this.``freeze expressions should capture local variables``() =
        assertEq (Int 1) (runIt "(let X 1 (let F (freeze X) (F)))")

    [<Test>]
    member this.``deep-running tail-recursive function does not stack overflow``() =
        let env = baseEnv()
        runInEnv env "(defun fill (Vec Start Stop Val) (if (= Stop Start) (address-> Vec Start Val) (fill (address-> Vec Start Val) (+ 1 Start) Stop Val)))" |> ignore
        runInEnv env "(fill (absvector 20000) 0 19999 0)" |> ignore
        ()
    
    [<Test>]
    member this.``deep-running mutually-recursive functions do not stack overflow``() =
        let env = baseEnv()
        runInEnv env "(defun odd? (X) (if (= 1 X) true (even? (- X 1))))" |> ignore
        runInEnv env "(defun even? (X) (if (= 1 X) false (odd? (- X 1))))" |> ignore
        assertEq Values.falsev (runInEnv env "(odd? 20000)")

    [<Test>]
    member this.``when if expr is in head position, conditional and branches should be in head``() =
        match parse Head (tokenize "(if (< 0 n) (* n 2) (- n 1))") with
        | IfExpr(AppExpr(Head, _, _), AppExpr(Head, _, _), AppExpr(Head, _, _)) -> ()
        | _ -> Assert.Fail "Head/Tail positions parsed incorrectly"

    [<Test>]
    member this.``when if expr is in tail position, conditional should be in head position, branches in tail``() =
        match parse Tail (tokenize "(if (< 0 n) (* n 2) (- n 1))") with
        | IfExpr(AppExpr(Head, _, _), AppExpr(Tail, _, _), AppExpr(Tail, _, _)) -> ()
        | _ -> Assert.Fail "Head/Tail positions parsed incorrectly"

    [<Test>]
    member this.``string index out of bounds should cause uncaught error``() =
        assertError "(pos \"\" 0)"
        assertError "(pos \"hello\" 5)"

    [<Test>]
    member this.``vector index out of bounds should cause uncaught error``() =
        assertError "(<-address (absvector 0) 0)"

    [<Test>]
    member this.``simple-error should cause uncaught error``() =
        try
            runIt "(simple-error \"whoops\")" |> ignore
            Assert.Fail "Exception expected"
        with
            SimpleError message -> Assert.AreEqual("whoops", message)

    [<Test>]
    member this.``simple-error should be caught by trap-error``() =
        assertEq Empty (runIt "(trap-error (simple-error \"whoops\") (lambda E ()))")

    [<Test>]
    member this.``trap-error should prevent uncaught error from propogating``() =
        assertEq Empty (runIt "(trap-error (pos \"\" 0) (lambda E ()))")

    [<Test>]
    member this.``trap-error should eval and apply second expression if eval of first results in uncaught error``() =
        let env = baseEnv()
        runInEnv env "(defun do (X Y) Y)" |> ignore
        assertEq
            Values.truev
            (runInEnv env "(trap-error (do (pos \"\" 0) false) (lambda E true))")

    [<Test>]
    member this.``error message should be preserved when error is caught and handled``() =
        assertEq
            (Str "hi")
            (runIt "(trap-error (simple-error \"hi\") (lambda E (error-to-string E)))")

    [<Test>]
    member this.``print platform information``() =
        let env = baseEnv()
        for pair in env.Globals.Symbols do
            printfn "%s = %s" pair.Key (Values.toStr pair.Value)
