namespace KlCompiler.Tests

open NUnit.Framework
open Kl
open Kl.Tokenizer
open Kl.Parser
open KlCompiler
open Fantomas
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open System.IO
open System
open System.Reflection

type CompilerTests() =

    let openKl = SynModuleDecl.Open(LongIdentWithDots.LongIdentWithDots([new Ident("Kl", FsAst.defaultRange)], []), FsAst.defaultRange)
    let formatConfig = Fantomas.FormatConfig.FormatConfig.create(4, 160, false, true, true, true, true, true, false, false, true)
    let singleBinding args syn =
        FsFile.Of(
                "KlExprTest",
                [FsModule.Of(
                    "KlExprTestMod",
                    [openKl
                     FsModule.SingleLet("z", args, syn)])])

    [<Test>]
    member this.CompilerServicesBuildAst() =
        let p = tokenize >> parse Head >> Compiler.build
        let r = AndExpr(BoolExpr true, BoolExpr false) |> Compiler.build
        let text = """module Stuff
        
open Kl

let fff = match (match 0 with
                 | 0 -> 0
                 | x -> x) with
          | 0 -> 0
          | x -> x
"""
        let parsedInput = Fantomas.CodeFormatter.Parse("./test.fs", text)
        let reformatted = Fantomas.CodeFormatter.FormatAST(parsedInput, None, formatConfig)
        let stopHere = ()
        let ast = FsFile.Of(
                      "ShenNs",
                      [FsModule.Of(
                          "ShenStuff",
                          [openKl
                           FsModule.SingleLet(
                              "f",
                              ["envGlobals", FsType.Of("Globals")
                               "X", FsType.Of("Value")
                               "Y", FsType.Of("Value")],
                              Compiler.build(
                                Expr.AppExpr(
                                  Position.Head,
                                  Expr.SymbolExpr "+",
                                  [SymbolExpr "X"; SymbolExpr "Y"])))])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "ShenAsm", ["Kl.dll"], None)
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let res1 = types.[0].GetMethods().[0].Invoke(null, [|Values.newGlobals(); IntValue 1; IntValue 2|])
        Assert.AreEqual(res1, ((IntValue 3) :> obj))

    [<Test>]
    member this.KlExprToSynExpr() =
        let kl = Expr.AndExpr(Expr.BoolExpr true, Expr.BoolExpr false)
        let syn = Compiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
        Assert.AreEqual(Values.falsev, v)
    
    [<Test>]
    member this.BuildFreezeExpr() =
        let kl = Expr.FreezeExpr(Expr.AppExpr(Head, Expr.SymbolExpr "number?", [Expr.StringExpr "hi"]))
        let syn = Compiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
        match v :?> Value with
        | FunctionValue _ -> ()
        | _ -> Assert.Fail("function expected")

    [<Test>]
    member this.BuildCondExpr() =
        let kl = "(cond ((> X 0) \"positive\") ((< X 0) \"negative\") (true \"zero\"))" |> tokenize |> parse Position.Head
        let syn = Compiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals"); "X", FsType.Of("Value")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals(); Value.IntValue(5)|])
        Assert.AreEqual(Value.StringValue "positive", v)
        let v2 = methods.[0].Invoke(null, [|Values.newGlobals(); Value.IntValue(-5)|])
        Assert.AreEqual(Value.StringValue "negative", v2)
        let v3 = methods.[0].Invoke(null, [|Values.newGlobals(); Value.IntValue(0)|])
        Assert.AreEqual(Value.StringValue "zero", v3)

    [<Test>]
    member this.BuildLetExpr() =
        let kl = "(let X 5 (if (> X 0) \"positive\" \"non-positive\"))" |> tokenize |> parse Position.Head
        let syn = Compiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
        Assert.AreEqual(StringValue "positive", v)

    [<Test>]
    member this.BuildModule() =
        let workingDirectory = Path.GetDirectoryName((new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath)
        let src = File.ReadAllText(Path.Combine(workingDirectory, @"..\..\..\KLambda\toplevel.kl"))
        let exprs = src |> tokenizeAll |> List.map rootParse |> Seq.take 6 |> Seq.toList
        let parsedInput = Compiler.buildModule exprs
        let str = Fantomas.CodeFormatter.FormatAST(parsedInput, None, formatConfig)
        System.Console.WriteLine(str)
