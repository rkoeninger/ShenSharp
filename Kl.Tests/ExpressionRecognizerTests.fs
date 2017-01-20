namespace Kl.Tests

open NUnit.Framework
open Kl.Values
open Kl.Expressions
open Kl.Load.Reader
open TestCommon

[<TestFixture>]
type ExpressionRecognizerTests() =

    let check recognizer strings =
        let checkSample (some, s) =
            let expect = if some then Option.isSome else Option.isNone
            Assert.True(s |> read |> recognizer |> expect, s)
        List.iter checkSample strings

    [<Test>]
    member this.``if expression starts with 'if and has 4 forms total``() =
        check (|IfExpr|_|) [
            true,  "(if x x x)"
            false, "(if x x)"
            false, "(if x x x x)"
            false, "(if x)"]

    [<Test>]
    member this.``and expression starts with 'and has 3 forms total``() =
        check (|AndExpr|_|) [
            true,  "(and x x)"
            false, "(and x)"
            false, "(and x x x)"
            false, "(and)"]

    [<Test>]
    member this.``or expression starts with 'or has 3 forms total``() =
        check (|OrExpr|_|) [
            true,  "(or x x)"
            false, "(or x)"
            false, "(or x x x)"
            false, "(or)"]

    [<Test>]
    member this.``cond expression starts with 'cond, has 1+ forms total and clauses must be cons lists of length 2``() =
        check (|CondExpr|_|) [
            true,  "(cond)"
            true,  "(cond (x x))"
            true,  "(cond (x x) (x x) (x x))"
            false, "(cond x)"
            false, "(cond (x x) (x x) (x x x))"
            false, "(cond (x x) ((x x) x) () (x x))"]

    [<Test>]
    member this.``let expression starts with 'let, has 4 forms total and 2nd must be a symbol``() =
        check (|LetExpr|_|) [
            true,  "(let x y z)"
            false, "(let (x y) z w)"]

    [<Test>]
    member this.``lambda expression starts with 'lambda, has 3 forms total and 2nd must be a symbol``() =
        check (|LambdaExpr|_|) [
            true,  "(lambda x y)"
            true,  "(lambda x ())"
            false, "(lambda)"
            false, "(lambda () x)"]

    [<Test>]
    member this.``freeze expression starts with 'freeze and has 2 forms total``() =
        check (|FreezeExpr|_|) [
            true,  "(freeze x)"
            false, "(freeze)"
            false, "(freeze x y)"]

    [<Test>]
    member this.``trap expression starts with 'trap-error and has 3 forms total``() =
        check (|TrapExpr|_|) [
            true,  "(trap-error x y)"
            false, "(trap-error x)"
            false, "(trap-error)"
            false, "(trap-error x y z)"]

    [<Test>]
    member this.``defun expression starts with 'defun and has 4 forms total``() =
        check (|DefunExpr|_|) [
            true,  "(defun x () y)"
            true,  "(defun x (a) y)"
            true,  "(defun x (a b) y)"
            false, "(defun x (x () x) y)"
            false, "(defun x ())"
            false, "(defun () () ())"]

    [<Test>]
    member this.``do expression starts with 'do and has 1+ forms total``() =
        check (|DoExpr|_|) [
            true,  "(do x y)"
            false, "(do)"
            false, "(do x y z)"
            false, "()"]

    [<Test>]
    member this.``application expression has 1+ forms``() =
        check (|AppExpr|_|) [
            true,  "(x)"
            true,  "(x y z)"
            false, "()"]
