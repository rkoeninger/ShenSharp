module Kl.Tests.``Expression Recognition``

open NUnit.Framework
open Kl.Values
open Assertions

[<Test>]
let ``cond expressions when they start with 'cond, have 1+ forms total and clauses are cons lists of length 2``() =
    checkPattern (|CondForm|_|) [
        true,  "(cond)"
        true,  "(cond (x x))"
        true,  "(cond (x x) (x x) (x x))"
        false, "(cond x)"
        false, "(cond (x x) (x x) (x x x))"
        false, "(cond (x x) ((x x) x) () (x x))"]

[<Test>]
let ``defun expressions when they start with 'defun and have 4 forms total``() =
    checkPattern (|DefunForm|_|) [
        true,  "(defun x () y)"
        true,  "(defun x (a) y)"
        true,  "(defun x (a b) y)"
        false, "(defun x (x () x) y)"
        false, "(defun x ())"
        false, "(defun () () ())"]
