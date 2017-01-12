namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Reader
open Kl.Optimizer
open Kl.Evaluator
open Kl.Startup
open TestCommon

[<TestFixture>]
type OptimizerTests() =

    [<Test>]
    member this.``tail recursive functions get transformed into loop expressions``() =
        printfn "%O" (simplify (read "(defun factorial (N Acc) (if (= N 0) Acc (factorial (- N 1) (* N Acc))))"))

    [<Test>]
    member this.``TR functions are optimized by loops``() =
        let env = baseEnv()
        eval env (simplify (read "(defun count-down (X) (if (<= X 0) true (count-down (- X 1))))")) |> ignore
        runIn env "(count-down 20000)" |> ignore
