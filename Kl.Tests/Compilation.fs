namespace Kl.Tests

open Fantomas
open NUnit.Framework
open Kl
open Kl.Import.Compiler
open Assertions

[<TestFixture>]
type Compilation() =

    [<Test>]
    member this.``test parse``() =
        let text = "
module Stuff
let someBinding =
    statement1 ()
    statement2 () |> ignore
    statement3 ()
"
        let ast = Fantomas.CodeFormatter.Parse("./test.fs", text)
        Assert.IsTrue(CodeFormatter.IsValidAST ast)
        ()
