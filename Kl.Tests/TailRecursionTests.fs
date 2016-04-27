namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Tokenizer
open Kl.Parser
open Kl.Startup
open TestCommon

[<TestFixture>]
type TailRecursionTests() =

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
