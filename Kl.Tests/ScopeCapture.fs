module Kl.Tests.``Scope Capture``

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Startup
open Assertions

[<Test>]
let ``lambda expressions should capture local variables``() =
    assertEq (Int 1) (run "(let X 1 (let F (lambda Y X) (F 0)))")

[<Test>]
let ``lambda expressions should not have access to symbols outside lexical scope``() =
    assertEq (Sym "Y") (run "(let F (lambda X Y) (let Y 3 (F 0)))")

[<Test>]
let ``freeze expressions should capture local variables``() =
    assertEq (Int 1) (run "(let X 1 (let F (freeze X) (F)))")
        
[<Test>]
let ``freeze expressions should not have access to symbols outside lexical scope``() =
    assertEq (Sym "Y") (run "(let F (freeze Y) (let Y 3 (F)))")

[<Test>]
let ``preserves the value of closed-over function parameters``() =
    let globals = baseGlobals()
    runIn globals "(set foo (absvector 3))" |> ignore
    runIn globals "(defun do-it (N) (if (= N 3) true (let _ (address-> (value foo) N (freeze N)) (do-it (+ N 1)))))" |> ignore
    runIn globals "(do-it 0)" |> ignore
    assertEq (Int 0) (runIn globals "((<-address (value foo) 0))")
    assertEq (Int 1) (runIn globals "((<-address (value foo) 1))")
    assertEq (Int 2) (runIn globals "((<-address (value foo) 2))")

[<Test>]
let ``preserves the value of closed-over local variables``() =
    let globals = baseGlobals()
    runIn globals "(set foo (absvector 3))" |> ignore
    runIn globals "(defun do-it (N) (if (= N 3) true (let X N (let _ (address-> (value foo) N (freeze X)) (do-it (+ N 1))))))" |> ignore
    runIn globals "(do-it 0)" |> ignore
    assertEq (Int 0) (runIn globals "((<-address (value foo) 0))")
    assertEq (Int 1) (runIn globals "((<-address (value foo) 1))")
    assertEq (Int 2) (runIn globals "((<-address (value foo) 2))")
