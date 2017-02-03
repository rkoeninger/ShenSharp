namespace Kl.Tests

open NUnit.Framework
open Kl.Values
open Kl.Expressions
open Kl.Import.Reader
open Assertions

[<TestFixture>]
type ``Expression Recognition``() =

    let check recognizer strings =
        let checkSample (some, s) =
            let expect = if some then Option.isSome else Option.isNone
            Assert.True(s |> read |> recognizer |> expect, s)
        List.iter checkSample strings

    [<Test>]
    member this.``if expressions when they start with 'if and have 4 forms total``() =
        check (|IfExpr|_|) [
            true,  "(if x x x)"
            false, "(if x x)"
            false, "(if x x x x)"
            false, "(if x)"]

    [<Test>]
    member this.``and expressions when they start with 'and and have 3 forms total``() =
        check (|AndExpr|_|) [
            true,  "(and x x)"
            false, "(and x)"
            false, "(and x x x)"
            false, "(and)"]

    [<Test>]
    member this.``or expressions when they start with 'or and have 3 forms total``() =
        check (|OrExpr|_|) [
            true,  "(or x x)"
            false, "(or x)"
            false, "(or x x x)"
            false, "(or)"]

    [<Test>]
    member this.``cond expressions when they start with 'cond, have 1+ forms total and clauses are cons lists of length 2``() =
        check (|CondExpr|_|) [
            true,  "(cond)"
            true,  "(cond (x x))"
            true,  "(cond (x x) (x x) (x x))"
            false, "(cond x)"
            false, "(cond (x x) (x x) (x x x))"
            false, "(cond (x x) ((x x) x) () (x x))"]

    [<Test>]
    member this.``let expressions when they start with 'let, have 4 forms total and 2nd is a symbol``() =
        check (|LetExpr|_|) [
            true,  "(let x y z)"
            false, "(let (x y) z w)"]

    [<Test>]
    member this.``lambda expressions when they start with 'lambda, have 3 forms total and 2nd is a symbol``() =
        check (|LambdaExpr|_|) [
            true,  "(lambda x y)"
            true,  "(lambda x ())"
            false, "(lambda)"
            false, "(lambda () x)"]

    [<Test>]
    member this.``freeze expressions when they start with 'freeze and have 2 forms total``() =
        check (|FreezeExpr|_|) [
            true,  "(freeze x)"
            false, "(freeze)"
            false, "(freeze x y)"]

    [<Test>]
    member this.``trap expressions when they start with 'trap-error and have 3 forms total``() =
        check (|TrapExpr|_|) [
            true,  "(trap-error x y)"
            false, "(trap-error x)"
            false, "(trap-error)"
            false, "(trap-error x y z)"]

    [<Test>]
    member this.``defun expressions when they start with 'defun and have 4 forms total``() =
        check (|DefunExpr|_|) [
            true,  "(defun x () y)"
            true,  "(defun x (a) y)"
            true,  "(defun x (a b) y)"
            false, "(defun x (x () x) y)"
            false, "(defun x ())"
            false, "(defun () () ())"]

    [<Test>]
    member this.``do expressions when they start with 'do and have 1+ forms total``() =
        check (|DoExpr|_|) [
            true,  "(do x y)"
            false, "(do)"
            false, "(do x y z)"
            false, "()"]

    [<Test>]
    member this.``application expressions when they have 1+ forms``() =
        check (|AppExpr|_|) [
            true,  "(x)"
            true,  "(x y z)"
            false, "()"]
