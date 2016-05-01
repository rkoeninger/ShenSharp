namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Startup
open TestCommon

[<TestFixture>]
type PartialApplicationTests() =

    [<Test>]
    member this.``defuns should be partially applicable``() =
        let env = baseEnv()
        runIn env "(defun add4 (A B C D) (+ A (+ B (+ C D))))" |> ignore
        assertEq
            (Int 10)
            (runIn env "(let X (add4 1) (let Y (X 2 3) (Y 4)))")

    [<Test>]
    member this.``applying a defun to fewer arguments than it takes results in a partial``() =
        let env = baseEnv()
        runIn env "(defun add3 (A B C) (+ A (+ B C)))" |> ignore
        match runIn env "(add3 1 2)" with
        | Func(Partial(Defun("add3", _, _), [Int 1; Int 2])) -> ()
        | _ -> Assert.Fail "Partial expected"

    [<Test>]
    member this.``applying a defun that takes 1 or more parameters to 0 arguments results in that same defun``() =
        let env = baseEnv()
        runIn env "(defun inc (X) (+ X 1))" |> ignore
        match runIn env "(inc)" with
        | Func(Defun("inc", _, _)) -> ()
        | _ -> Assert.Fail "Defun expected"

    [<Test>]
    member this.``applying a defun to more arguments than it takes causes an error``() =
        let env = baseEnv()
        runIn env "(defun add2 (A B) (+ A B))" |> ignore
        assertErrorInEnv env "(add2 5 3 4 6 7)"

    [<Test>]
    member this.``application of zero-param defuns should not be mistaken for partial application``() =
        let env = baseEnv()
        runIn env "(defun const () 8)" |> ignore
        assertEq (Int 8) (runIn env "(const)")

    [<Test>]
    member this.``primitives should be partially applicable``() =
        assertEq (Int 7) (run "((+ 10) -3)")
        
    [<Test>]
    member this.``applying a primitive to fewer arguments than it takes results in a partial``() =
        match run "(+ 2)" with
        | Func(Partial(Primitive("+", _, _), [Int 2])) -> ()
        | _ -> Assert.Fail "Partial expected"
        
    [<Test>]
    member this.``applying a primitive that takes 1 or more parameters to 0 arguments results in that same primitve``() =
        match run "(+)" with
        | Func(Primitive("+", _, _)) -> ()
        | x -> Assert.Fail "Primitive expected"

    [<Test>]
    member this.``applying a primitive to more arguments than it takes causes an error``() =
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
    member this.``applying a lambda to 0 arguments results in the same lambda``() =
        match run "((lambda X X))" with
        | Func(Lambda("X", _, _)) -> ()
        | _ -> Assert.Fail "Lambda expected"

    [<Test>]
    member this.``application of multiple arguments should work over curried lambdas``() =
        assertEq (Int 3) (run "(let F (lambda X (lambda Y (+ X Y))) (F 1 2))")

    [<Test>]
    member this.``arguments can be applied to freezes granted freeze eval's to an argument that accepts them``() =
        assertEq (Int 3) (run "(let F (freeze (lambda X (lambda Y (+ X Y)))) (F 1 2))")

    [<Test>]
    member this.``application of excessive arguments to defuns then get applied to returned function``() =
        let env = Startup.baseEnv()
        runIn env "(defun add (X) (lambda Y (+ X Y)))" |> ignore
        assertEq (Int 3) (runIn env "(add 1 2)")
