module Kl.Tests.``Tail Call Optimization``

open NUnit.Framework
open Assertions

[<Test>]
let ``optimize tail recursive calls in an if consequent expression``() =
    attempt "(if (> X 0) (count-down (- X 1)) true)"

[<Test>]
let ``optimize tail recursive calls in an if alternative expression``() =
    attempt "(if (<= X 0) true (count-down (- X 1)))"

[<Test>]
let ``optimize tail recursive calls in nested if expressions``() =
    attempt "(if (<= X 0) true (if true (count-down (- X 1)) false))"

[<Test>]
let ``optimize tail recursive calls in a let body``() =
    attempt "(if (<= X 0) true (let F 1 (count-down (- X F))))"

[<Test>]
let ``optimize tail recursive calls in a first cond consequent``() =
    attempt "(cond ((> X 0) (count-down (- X 1))) (true true))"

[<Test>]
let ``optimize tail recursive calls in a last cond consequent``() =
    attempt "(cond ((<= X 0) true) (true (count-down (- X 1))))"

[<Test>]
let ``optimize tail recursive calls at the end of a do``() =
    attempt "(do 0 (if (<= X 0) true (do 0 (count-down (- X 1)))))"

[<Test>]
let ``optimize tail recursive calls in handler of trap expression``() =
    attempt "(trap-error (if (> X 0) (simple-error \"recur\") true) (lambda E (count-down (- X 1))))"

[<Test>]
let ``optimize through freeze calls``() =
    attempt "(if (<= X 0) true ((freeze (count-down (- X 1)))))"

[<Test>]
let ``optimize through lambda calls``() =
    attempt "(if (<= X 0) true ((lambda Y (count-down (- X Y))) 1))"

[<Test>]
let ``optimize through recursive lambdas``() =
    assertTrue "(let F (lambda F (lambda X (if (<= X 0) true ((F F) (- X 1))))) ((F F) 20000))"

[<Test>]
let ``optimize through recursive freezes``() =
    assertTrue
        "(set counter 20000)
         (defun decrement () (set counter (- (value counter) 1)))
         (set count-down (freeze (if (= 0 (decrement)) true ((value count-down)))))
         ((value count-down))"

[<Test>]
let ``optimize through zero-arg defuns``() =
    assertTrue
        "(set counter 20000)
         (defun decrement () (set counter (- (value counter) 1)))
         (defun count-down () (if (= 0 (decrement)) true (count-down)))
         (count-down)"

[<Test>]
let ``optimize mutually-recursive functions``() =
    assertFalse
        "(defun odd? (X) (if (= 0 X) false (even? (- X 1))))
         (defun even? (X) (if (= 0 X) true (odd? (- X 1))))
         (odd? 20000)"
