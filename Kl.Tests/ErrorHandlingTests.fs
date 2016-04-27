namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Startup
open TestCommon

[<TestFixture>]
type ErrorHandlingTests() =

    [<Test>]
    member this.``simple-error should cause uncaught error``() =
        try
            runIt "(simple-error \"whoops\")" |> ignore
            Assert.Fail "Exception expected"
        with
            SimpleError message -> Assert.AreEqual("whoops", message)

    [<Test>]
    member this.``simple-error should be caught by trap-error``() =
        assertEq Empty (runIt "(trap-error (simple-error \"whoops\") (lambda E ()))")

    [<Test>]
    member this.``trap-error should prevent uncaught error from propogating``() =
        assertEq Empty (runIt "(trap-error (pos \"\" 0) (lambda E ()))")

    [<Test>]
    member this.``trap-error should eval and apply second expression if eval of first results in uncaught error``() =
        let env = baseEnv()
        runInEnv env "(defun do (X Y) Y)" |> ignore
        assertEq
            Values.truev
            (runInEnv env "(trap-error (do (pos \"\" 0) false) (lambda E true))")

    [<Test>]
    member this.``error message should be preserved when error is caught and handled``() =
        assertEq
            (Str "hi")
            (runIt "(trap-error (simple-error \"hi\") (lambda E (error-to-string E)))")
