namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Startup
open TestCommon

[<TestFixture>]
type PartialApplicationTests() =

    [<Test>]
    member this.``defuns should be partially applicable``() =
        assertEq (Int 10) <| runAll
            "(defun add4 (A B C D) (+ A (+ B (+ C D))))
             (let X (add4 1) (let Y (X 2 3) (Y 4)))"

    [<Test>]
    member this.``applying a defun to fewer arguments than it takes results in a partial``() =
        match runAll "(defun add3 (A B C) (+ A (+ B C))) (add3 1 2)" with
        | Func(Partial(Defun("add3", _, _), [Int 1; Int 2])) -> ()
        | _ -> Assert.Fail "Partial expected"

    [<Test>]
    member this.``applying a defun that takes 1 or more parameters to 0 arguments results in that same defun``() =
        match runAll "(defun inc (X) (+ X 1)) (inc)" with
        | Func(Defun("inc", _, _)) -> ()
        | _ -> Assert.Fail "Defun expected"

    [<Test>]
    member this.``applying a defun to more arguments than it takes causes an error``() =
        assertError
            "(defun add (A B) (+ A B))
             (add 5 3 4 6 7)"

    [<Test>]
    member this.``application of zero-param defuns should not be mistaken for partial application``() =
        assertEq (Int 8) (runAll "(defun const () 8) (const)")

    [<Test>]
    member this.``application of freezes should not be mistaken for partial application``() =
        assertEq (Int 8) (run "((freeze 8))")

    [<Test>]
    member this.``native should be partially applicable``() =
        assertEq (Int 7) (run "((+ 10) -3)")

    [<Test>]
    member this.``applying a native to fewer arguments than it takes results in a partial``() =
        match run "(+ 2)" with
        | Func(Partial(Native("+", _, _), [Int 2])) -> ()
        | _ -> Assert.Fail "Partial expected"

        assertEq (Int 3) (run "((+ 2) 1)")

    [<Test>]
    member this.``applying a native that takes 1 or more parameters to 0 arguments results in that same primitve``() =
        match run "(+)" with
        | Func(Native("+", _, _)) -> ()
        | x -> Assert.Fail "Native expected"

        assertEq (Int 3) (run "((+) 1 2)")

    [<Test>]
    member this.``applying a native to more arguments than it takes causes an error``() =
        assertError "(+ 5 3 4 6 7)"

    [<Test>]
    member this.``freezes can not be applied to any non-zero number of arguments``() =
        assertError "(let X 5 (let Y (freeze X) (Y 2)))"
        assertError "((freeze 0) 4 3 2)"

    [<Test>]
    member this.``lambdas can only be applied to a single argument``() =
        assertError "((lambda X X) 0 0 0)"
        assertEq (Int 4) (run "((lambda X X) 4)")

    [<Test>]
    member this.``applying a lambda to 0 arguments results in error``() =
        assertError "((lambda X X))"

    [<Test>]
    member this.``application of multiple arguments should work over curried lambdas``() =
        assertEq (Int 3) (run "(let F (lambda X (lambda Y (+ X Y))) (F 1 2))")

    [<Test>]
    member this.``excessive arguments applied by freeze results in error``() =
        assertError "(let F (freeze (lambda X (lambda Y (+ X Y)))) (F 1 2))"

    [<Test>]
    member this.``excessive arguments applied by defuns will not get passed on to returned function``() =
        assertEq (Int 3) <| runAll
            "(defun add (X) (lambda Y (+ X Y)))
             ((add 1) 2)"

        assertError
            "(defun add (X) (lambda Y (+ X Y)))
             (add 1 2)"

    [<Test>]
    member this.``excessive arguments are applied by function that symbol resolves to if symbol returned from freeze``() =
        assertEq (Int 3) (run "(((freeze +)) 1 2)")
        assertError "((freeze +) 1 2)"

    [<Test>]
    member this.``excessive arguments are applied by function that symbol resolves to if symbol returned from lambda``() =
        assertEq (Int 3) (run "((lambda _ +) 0 1 2)")
        assertEq (Int 3) (run "(((lambda _ +) 0) 1 2)")
        assertEq (Int 3) (run "((((lambda _ +) 0) 1) 2)")
        assertEq (Int 3) (run "((((lambda _ +) 0) 1) 2)")

    [<Test>]
    member this.``excessive arguments are applied by function that symbol resolves to if symbol returned from defun``() =
        assertEq (Int 3) <| runAll
            "(defun f () +)
             ((f) 1 2)"

        assertError
            "(defun f () +)
             (f 1 2)"

    [<Test>]
    member this.``excessive arguments are applied by function that symbol resolves to if symbol returned from native``() =
        assertEq (Int 3) (run "((intern \"+\") 1 2)")
        assertError "(intern \"+\" 1 2)"
