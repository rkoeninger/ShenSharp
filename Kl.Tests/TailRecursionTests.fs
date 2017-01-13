namespace Kl.Tests

open NUnit.Framework
open Kl.Values
open Kl.Reader
open Kl.Startup
open TestCommon

[<TestFixture>]
type TailRecursionTests() =

    let attempt body =
        assertTrue (runAll (sprintf "(defun count-down (X) %s) (count-down 20000)" body))

    [<Test>]
    member this.``trampolines optimize tail recursive calls in an if consequent expression``() =
        attempt "(if (> X 0) (count-down (- X 1)) true)"

    [<Test>]
    member this.``trampolines optimize tail recursive calls in an if alternative expression``() =
        attempt "(if (<= X 0) true (count-down (- X 1)))"

    [<Test>]
    member this.``trampolines optimize tail recursive calls in nested if expressions``() =
        attempt "(if (<= X 0) true (if true (count-down (- X 1)) false))"

    [<Test>]
    member this.``trampolines optimize tail recursive calls in a let body``() =
        attempt "(if (<= X 0) true (let F 1 (count-down (- X F))))"

    [<Test>]
    member this.``trampolines optimize tail recursive calls in a first cond consequent``() =
        attempt "(cond ((> X 0) (count-down (- X 1))) (true true))"

    [<Test>]
    member this.``trampolines optimize tail recursive calls in a last cond consequent``() =
        attempt "(cond ((<= X 0) true) (true (count-down (- X 1))))"

    [<Test>]
    member this.``trampolines optimize tail recursive calls at the end of a do``() =
        attempt "(do 0 (if (<= X 0) true (do 0 (count-down (- X 1)))))"

    [<Test>]
    member this.``trampolines optimize tail recursive calls in handler of trap expression``() =
        attempt "(trap-error (if (> X 0) (simple-error \"recur\") true) (lambda E (count-down (- X 1))))"

    [<Test>]
    member this.``trampolines optimize mutually-recursive functions``() =
        assertEq falsev <| runAll
            "(defun odd? (X) (if (= 0 X) false (even? (- X 1))))
             (defun even? (X) (if (= 0 X) true (odd? (- X 1))))
             (odd? 20000)"

    [<Test>]
    member this.``trampolines optimize through freeze calls``() =
        attempt "(if (<= X 0) true ((freeze (count-down (- X 1)))))"

    [<Test>]
    member this.``trampolines optimize through lambda calls``() =
        attempt "(if (<= X 0) true ((lambda Y (count-down (- X Y))) 1))"

    [<Test>]
    member this.``trampolines optimize through recursive lambdas``() =
        runAll "(defun recur (F) (F F 20000))
                (recur (lambda F (lambda X (if (<= X 0) true (F (- X 1))))))" |> ignore

    [<Test>]
    member this.``trampolines optimize through zero-arg defuns``() =
        let env = baseEnv()
        runIn env "(set foo (absvector 3))" |> ignore
        runIn env "(set counter 20000)" |> ignore
        runIn env "(defun count-down () (if (= 0 (set counter (- (value counter) 1))) true (count-down)))" |> ignore
        assertTrue (runIn env "(count-down)")

    [<Test>]
    member this.``non-optmized blows KL stack``() =
        runAll "(defun f (X) (+ 1 (g X)))
                (defun g (X) (+ 1 (h X)))
                (defun h (X) (+ 1 (i X)))
                (defun i (X) (+ 1 (j X)))
                (defun j (X) (+ 1 (k X)))
                (defun k (X) (+ 1 (l X)))
                (defun l (X) (+ 1 (m X)))
                (defun m (X) (+ 1 (n X)))
                (defun n (X) (+ 1 (o X)))
                (defun o (X) (+ 1 X))
                (f 0)" |> ignore
