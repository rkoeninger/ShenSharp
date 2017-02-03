namespace Kl.Tests

open Fantomas
open NUnit.Framework
open Kl
open Kl.Import.Compiler
open TestCommon

[<TestFixture>]
type CompilationTests() =

    [<Test>]
    member this.``test parse``() =
        let text = "module Stuff
        
open Kl
let fff = match (match 0 with
                 | 0 -> 0
                 | x -> x) with
          | 0 -> 0
          | x -> x
"
        let ast = Fantomas.CodeFormatter.Parse("./test.fs", text)
        Assert.IsTrue(CodeFormatter.IsValidAST ast)
        ()
