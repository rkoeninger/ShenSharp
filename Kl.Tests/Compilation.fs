namespace Kl.Tests

open Fantomas
open Fantomas.FormatConfig
open NUnit.Framework
open Kl
open Kl.Values
open Kl.Startup
open Kl.Import.Reader
open Kl.Import.Compiler
open Assertions

[<TestFixture>]
type Compilation() =

    let fn globals name args body =
        globals.Functions.[name] <- Defun(name, List.length args, InterpretedDefun(args, read body))

    let sy globals name value =
        globals.Symbols.[name] <- value

    [<Test>]
    member this.``test parse``() =
        let text = "
module ShenRuntime
let g = globals.Functions.[\"xor\"] <- ()
"
        let ast = CodeFormatter.Parse("./test.fs", text)
        Assert.IsTrue(CodeFormatter.IsValidAST ast)
        printfn "%A" ast

    [<Test>]
    member this.``test build``() =
        let globals = baseGlobals()
        fn globals "xor" ["X"; "Y"] "(and (not (and X Y)) (or X Y))"
        fn globals "factorial" ["N"] "(if (= N 0) 1 (* N (factorial (- N 1))))"
        fn globals "inc-all" ["Xs"] "(map (lambda X (+ 1 X)) Xs)"
        sy globals "array-value" (Vec [|Int 1; Int 2; Int 3|])
        sy globals "cons-test" (Cons(Int 1, Int 2))
        sy globals "lambda-test" (Func(Lambda(InterpretedLambda(Map.empty, "X", toCons [Sym "+"; Sym "X"; Int 1]))))
        //TODO: sy globals "lambda-2" (Func(Lambda(InterpretedLambda(Map.empty, "X", Func(Lambda(InterpretedLambda(Map.empty, "Y", toCons [Sym "+"; Sym "X"; Sym "Y"])))))))
        let ast = compile "ShenRuntime" globals
        let format = {FormatConfig.Default with PageWidth = 1024}
        printfn "%s" (CodeFormatter.FormatAST(ast, "file", None, format))
