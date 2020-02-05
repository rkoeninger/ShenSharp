module Kl.Tests.``Error Handling``

open NUnit.Framework
open Kl
open Kl.Values
open Assertions

[<Test>]
let ``simple-error should cause uncaught error``() =
    try
        run "(simple-error \"whoops\")" |> ignore
        Assert.Fail "Exception expected"
    with
        e -> Assert.AreEqual("whoops", e.Message)

[<Test>]
let ``simple-error should be caught by trap-error``() =
    assertEq Empty (run "(trap-error (simple-error \"whoops\") (lambda E ()))")

[<Test>]
let ``trap-error should prevent uncaught error from propogating``() =
    assertEq Empty (run "(trap-error (pos \"\" 0) (lambda E ()))")

[<Test>]
let ``trap-error should eval and apply second expression if eval of first results in uncaught error``() =
    assertEq True (run "(trap-error (do (pos \"\" 0) false) (lambda E true))")

[<Test>]
let ``error message should be preserved when error is caught and handled``() =
    assertEq (Str "hi") (run "(trap-error (simple-error \"hi\") (lambda E (error-to-string E)))")

[<Test>]
let ``error handler can just be specified as a global symbol``() =
    assertEq (Str "hi") (run "(trap-error (simple-error \"hi\") error-to-string)")

[<Test>]
let ``error handler can just be specified as a local variable``() =
    assertEq (Str "hi") (run "(let ERR (lambda E (error-to-string E)) (trap-error (simple-error \"hi\") ERR))")
