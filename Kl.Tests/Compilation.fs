namespace Kl.Tests

open Fantomas
open Fantomas.FormatConfig
open NUnit.Framework
open Kl
open Kl.Startup
open Kl.Import.Reader
open Kl.Import.Compiler
open Assertions

[<TestFixture>]
type Compilation() =

    let fn globals name args body =
        globals.Functions.[name] <- Defun(name, List.length args, InterpretedDefun(args, read body))

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
        let globals = baseGlobals()
        fn globals "xor" ["X"; "Y"] "(and (not (and X Y)) (or X Y))"
        let ast = compile "ShenRuntime" globals
        let format = {FormatConfig.Default with PageWidth = 1024}
        printfn "%s" (CodeFormatter.FormatAST(ast, "file", None, format))
