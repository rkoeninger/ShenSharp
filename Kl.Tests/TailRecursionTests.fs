namespace Kl.Tests

open NUnit.Framework
open Kl.Values
open Kl.Reader
open Kl.Startup
open TestCommon

[<TestFixture>]
type TailRecursionTests() =

    let attempt body =
        let env = baseEnv()
        runIn env (sprintf "(defun count-down (X) %s)" body) |> ignore
        assertTrue (runIn env "(count-down 20000)")

    [<Test>]
    member this.``optimizes self tail calls in an if consequent expression``() =
        attempt "(if (> X 0) (count-down (- X 1)) true)"

    [<Test>]
    member this.``optimizes self tail calls in an if alternative expression``() =
        attempt "(if (<= X 0) true (count-down (- X 1)))"

    [<Test>]
    member this.``optimizes self tail calls in nested if expressions``() =
        attempt "(if (<= X 0) true (if true (count-down (- X 1)) false))"

    [<Test>]
    member this.``optimizes self tail calls in a let body``() =
        attempt "(if (<= X 0) true (let F 1 (count-down (- X F))))"

    [<Test>]
    member this.``optimizes self tail calls in a first cond consequent``() =
        attempt "(cond ((> X 0) (count-down (- X 1))) (true true))"

    [<Test>]
    member this.``optimizes self tail calls in a last cond consequent``() =
        attempt "(cond ((<= X 0) true) (true (count-down (- X 1))))"

    [<Test>]
    member this.``optimizes self tail calls at the end of a do``() =
        attempt "(do 0 (if (<= X 0) true (do 0 (count-down (- X 1)))))"

    [<Test>]
    member this.``optimizes self tail calls in handler of trap expression``() =
        attempt "(trap-error (if (> X 0) (simple-error \"recur\") true) (lambda E (count-down (- X 1))))"

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
        assertEq falsev (runIn env "(odd? 20000)")

    [<Test>]
    member this.``trampolines optimize through freeze calls``() =
        attempt "(if (<= X 0) true ((freeze (count-down (- X 1)))))"

    [<Test>]
    member this.``trampolines optimize through lambda calls``() =
        attempt "(if (<= X 0) true ((lambda Y (count-down (- X Y))) 1))"

    [<Test>]
    member this.``trampolines optimize through recursive lambdas``() =
        let env = baseEnv()
        runIn env "(defun recur (F) (F F 20000))" |> ignore
        runIn env "(recur (lambda F (lambda X (if (<= X 0) true (F (- X 1))))))" |> ignore

    [<Test>]
    member this.``non-optmized blows KL stack``() =
        let env = baseEnv()
        runIn env "(defun f (X) (+ 1 (g X)))" |> ignore
        runIn env "(defun g (X) (+ 1 (h X)))" |> ignore
        runIn env "(defun h (X) (+ 1 (i X)))" |> ignore
        runIn env "(defun i (X) (+ 1 (j X)))" |> ignore
        runIn env "(defun j (X) (+ 1 (k X)))" |> ignore
        runIn env "(defun k (X) (+ 1 (l X)))" |> ignore
        runIn env "(defun l (X) (+ 1 (m X)))" |> ignore
        runIn env "(defun m (X) (+ 1 (n X)))" |> ignore
        runIn env "(defun n (X) (+ 1 (o X)))" |> ignore
        runIn env "(defun o (X) (+ 1 X))" |> ignore
        runIn env "(f 0)" |> ignore
