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
        runInEnv env "(defun add4 (A B C D) (+ A (+ B (+ C D))))" |> ignore
        assertEq
            (Int 10)
            (runInEnv env "(let X (add4 1) (let Y (X 2 3) (Y 4)))")

    [<Test>]
    member this.``applying a defun to fewer arguments than it takes results in a partial``() =
        let env = baseEnv()
        runInEnv env "(defun add3 (A B C) (+ A (+ B C)))" |> ignore
        match runInEnv env "(add3 1 2)" with
        | Func(Partial(Defun("add3", _, _), [Int 1; Int 2])) -> ()
        | _ -> Assert.Fail "Partial expected"

    [<Test>]
    member this.``applying a defun that takes 1 or more parameters to 0 arguments results in a partial``() =
        let env = baseEnv()
        runInEnv env "(defun inc (X) (+ X 1))" |> ignore
        match runInEnv env "(inc)" with
        | Func(Partial(Defun("inc", _, _), [])) -> ()
        | _ -> Assert.Fail "Partial expected"

    [<Test>]
    member this.``applying a defun to more arguments than it takes causes an error``() =
        let env = baseEnv()
        runInEnv env "(defun add2 (A B) (+ A B))" |> ignore
        assertErrorInEnv env "(add2 5 3 4 6 7)"

    [<Test>]
    member this.``application of zero-param defuns should not be mistaken for partial application``() =
        let env = baseEnv()
        runInEnv env "(defun const () 8)" |> ignore
        assertEq (Int 8) (runInEnv env "(const)")

    [<Test>]
    member this.``primitives should be partially applicable``() =
        assertEq (Int 7) (runIt "((+ 10) -3)")
        
    [<Test>]
    member this.``applying a primitive to fewer arguments than it takes results in a partial``() =
        match runIt "(+ 2)" with
        | Func(Partial(Primitive("+", _, _), [Int 2])) -> ()
        | _ -> Assert.Fail "Partial expected"
        
    [<Test>]
    member this.``applying a primitive that takes 1 or more parameters to 0 arguments results in a partial``() =
        match runIt "(+)" with
        | Func(Partial(Primitive("+", _, _), [])) -> ()
        | x -> Assert.Fail "Partial expected"

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
        assertEq (Int 4) (runIt "((lambda X X) 4)")

    [<Test>]
    member this.``lambdas cannot be partially applied to 0 arguments``() =
        assertError "((lambda X X))"
