namespace Kl.Tests

open NUnit.Framework
open Kl
open TestCommon

[<TestFixture>]
type ScopeCaptureTests() =

    [<Test>]
    member this.``lambda expressions should capture local variables``() =
        assertEq (Int 1) (runIt "(let X 1 (let F (lambda Y X) (F 0)))")

    [<Test>]
    member this.``freeze expressions should capture local variables``() =
        assertEq (Int 1) (runIt "(let X 1 (let F (freeze X) (F)))")
