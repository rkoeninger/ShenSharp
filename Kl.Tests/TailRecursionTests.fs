namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Reader
open Kl.Startup
open TestCommon

[<TestFixture>]
type TailRecursionTests() =

    [<Test>]
    member this.``optimizes self tail calls in an if consequent expression``() =
        let env = baseEnv()
        runIn env "(defun count-down (X) (if (> X 0) (count-down (- X 1)) true))" |> ignore
        assertTrue (runIn env "(count-down 20000)")

    [<Test>]
    member this.``optimizes self tail calls in an if alternative expression``() =
        let env = baseEnv()
        runIn env "(defun count-down (X) (if (> X 0) true (count-down (- X 1))))" |> ignore
        assertTrue (runIn env "(count-down 20000)")

    [<Test>]
    member this.``optimizes self tail calls in a let body``() =
        let env = baseEnv()
        runIn env "(defun count-down (X) (if (<= X 0) true (let F 1 (count-down (- X 1)))))" |> ignore
        assertTrue (runIn env "(count-down 20000)")

    [<Test>]
    member this.``deep-running tail-recursive function does not stack overflow``() =
        let env = baseEnv()
        runIn env "(defun fill (Vec Start Stop Val) (if (= Stop Start) (address-> Vec Start Val) (fill (address-> Vec Start Val) (+ 1 Start) Stop Val)))" |> ignore
        runIn env "(fill (absvector 20000) 0 19999 0)" |> ignore
    
    [<Test>]
    member this.``deep-running mutually-recursive functions do not stack overflow``() =
        let env = baseEnv()
        runIn env "(defun odd? (X) (if (= 1 X) true (even? (- X 1))))" |> ignore
        runIn env "(defun even? (X) (if (= 1 X) false (odd? (- X 1))))" |> ignore
        assertEq Values.falsev (runIn env "(odd? 20000)")

    // TODO: how to test this directly now?
//    [<Test>]
//    member this.``when if expr is in head position, conditional and branches should be in head``() =
//        match parse Head (tokenize "(if (< 0 n) (* n 2) (- n 1))") with
//        | IfExpr(AppExpr(Head, _, _), AppExpr(Head, _, _), AppExpr(Head, _, _)) -> ()
//        | _ -> Assert.Fail "Head/Tail positions parsed incorrectly"
//
//    [<Test>]
//    member this.``when if expr is in tail position, conditional should be in head position, branches in tail``() =
//        match parse Tail (tokenize "(if (< 0 n) (* n 2) (- n 1))") with
//        | IfExpr(AppExpr(Head, _, _), AppExpr(Tail, _, _), AppExpr(Tail, _, _)) -> ()
//        | _ -> Assert.Fail "Head/Tail positions parsed incorrectly"
