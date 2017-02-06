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
open Kl
open Kl.Builtins
open Kl.Evaluator
// (defun shen.posint? (V14609) (and (integer? V14609) (>= V14609 0)))
//let ``kl_shen.posint?`` globals = function
//    | [kl_V14609] -> Bool(isTrue(``kl_integer?`` globals [kl_V14609]) && isTrue(``kl_>=`` globals [kl_V14609; 0]))
//    | args -> argsErr \"shen.posint?\" [\"value\"] args
let f x = if true then () else ()
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
