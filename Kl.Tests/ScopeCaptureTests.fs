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
    member this.``lambda expressions should not have access to symbols outside lexical scope``() =
        assertEq (Sym "Y") (runIt "(let F (lambda X Y) (let Y 3 (F 0)))")

    [<Test>]
    member this.``freeze expressions should capture local variables``() =
        assertEq (Int 1) (runIt "(let X 1 (let F (freeze X) (F)))")
        
    [<Test>]
    member this.``freeze expressions should not have access to symbols outside lexical scope``() =
        assertEq (Sym "Y") (runIt "(let F (freeze Y) (let Y 3 (F)))")
