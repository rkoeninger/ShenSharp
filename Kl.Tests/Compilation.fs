namespace Kl.Tests

open Fantomas
open Fantomas.FormatConfig
open NUnit.Framework
open Kl
open Kl.Values
open Kl.Analysis
open Kl.Startup
open Kl.Make.Reader
open Kl.Make.Compiler
open Assertions

[<TestFixture>]
type Compilation() =

    let pars = parse (newGlobals(), Set.empty)

    let fn globals name paramz body =
        define globals name (Interpreted(Map.empty, paramz, pars <| read body))

    let sy globals name value =
        assign globals name value

    [<Test>]
    member this.``test parse``() =
        let text = "
[<System.Runtime.CompilerServices.Extension>]
module Shen.Runtime
    [<System.Runtime.CompilerServices.Extension>]
    let Load(globals: Globals, path: string) = kl_load globals [Str path] |> ignore
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
        fn globals "lambda-value-test" [] "((lambda X (+ 1 X)) 0)"
        fn globals "lmabda-local-test" [] "(let F (lambda X (+ 1 X)) (F 0))"
        fn globals "curried-defun" ["X"] "(do X (lambda Y (+ X Y)))"
        fn globals "freeze-in-defun" ["X"] "(do X (freeze X))"
        fn globals "partial-test" [] "((+ 1) 2)"
        sy globals "array-value" (Vec [|Int 1; Int 2; Int 3|])
        sy globals "cons-test" (Cons(Int 1, Int 2))
        sy globals "lambda-test" (Func(Interpreted(Map.empty, ["X"], pars <| toCons [Sym "+"; Sym "X"; Int 1])))
        let ast = compile ["Shen"; "Runtime"] globals
        let format = {FormatConfig.Default with PageWidth = 1024}
        printfn "%s" (CodeFormatter.FormatAST(ast, "file", None, format))
        printfn "%A" ast
