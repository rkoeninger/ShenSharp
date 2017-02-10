namespace Kl.Tests

open Fantomas
open Fantomas.FormatConfig
open NUnit.Framework
open Kl
open Kl.Startup
open Kl.Import.Compiler
open Assertions

[<TestFixture>]
type Compilation() =

    [<Test>]
    member this.``test parse``() =
        let text = "
module ShenRuntime
let g = function
        | Func f -> ()
        | _ -> ()
"
        let ast = CodeFormatter.Parse("./test.fs", text)
        Assert.IsTrue(CodeFormatter.IsValidAST ast)
        printfn "%A" ast

    [<Test>]
    member this.``test build``() =
        let ast = compile "file" (baseGlobals())
        let format = {FormatConfig.Default with PageWidth = 1024}
        printfn "%s" (CodeFormatter.FormatAST(ast, "file", None, format))
        printfn "%A" ast
