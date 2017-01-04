﻿namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Reader
open Kl.Startup
open TestCommon

[<TestFixture>]
type TailRecursionTests() =

    let testTailCall body =
        let env = baseEnv()
        runIn env ("(defun count-down (X) " + body + ")") |> ignore
        assertTrue (runIn env "(count-down 20000)")

    [<Test>]
    member this.``optimizes self tail calls in an if consequent expression``() =
        testTailCall "(if (> X 0) (count-down (- X 1)) true)"

    [<Test>]
    member this.``optimizes self tail calls in an if alternative expression``() =
        testTailCall "(if (<= X 0) true (count-down (- X 1)))"

    [<Test>]
    member this.``optimizes self tail calls in nested if expressions``() =
        testTailCall "(if (<= X 0) true (if true (count-down (- X 1)) false))"

    [<Test>]
    member this.``optimizes self tail calls in a let body``() =
        testTailCall "(if (<= X 0) true (let F 1 (count-down (- X F))))"

    [<Test>]
    [<Ignore("failing")>]
    member this.``optimizes self tail calls in a first cond consequent``() =
        testTailCall "(cond ((> X 0) (count-down (- X 1))) (true true))"

    [<Test>]
    [<Ignore("failing")>]
    member this.``optimizes self tail calls in a last cond consequent``() =
        testTailCall "(cond ((<= X 0) true) (true (count-down (- X 1))))"

    [<Test>]
    member this.``optimizes self tail calls at the end of a do``() =
        testTailCall "(do 0 (if (<= X 0) true (do 0 (count-down (- X 1)))))"

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