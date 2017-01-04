﻿namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Startup
open Kl.Values
open TestCommon

[<TestFixture>]
type ErrorHandlingTests() =

    [<Test>]
    member this.``simple-error should cause uncaught error``() =
        try
            run "(simple-error \"whoops\")" |> ignore
            Assert.Fail "Exception expected"
        with
            SimpleError message -> Assert.AreEqual("whoops", message)

    [<Test>]
    member this.``simple-error should be caught by trap-error``() =
        assertEq Empty (run "(trap-error (simple-error \"whoops\") (lambda E ()))")

    [<Test>]
    member this.``trap-error should prevent uncaught error from propogating``() =
        assertEq Empty (run "(trap-error (pos \"\" 0) (lambda E ()))")

    [<Test>]
    member this.``trap-error should eval and apply second expression if eval of first results in uncaught error``() =
        let env = baseEnv()
        runIn env "(defun do (X Y) Y)" |> ignore
        assertEq truev (runIn env "(trap-error (do (pos \"\" 0) false) (lambda E true))")

    [<Test>]
    member this.``error message should be preserved when error is caught and handled``() =
        assertEq (Str "hi") (run "(trap-error (simple-error \"hi\") (lambda E (error-to-string E)))")

    [<Test>]
    member this.``error handler can just be specified as a global symbol``() =
        assertEq (Str "hi") (run "(trap-error (simple-error \"hi\") error-to-string)")

    [<Test>]
    member this.``error handler can just be specified as a local variable``() =
        assertEq (Str "hi") (run "(let ERR (lambda E (error-to-string E)) (trap-error (simple-error \"hi\") ERR))")