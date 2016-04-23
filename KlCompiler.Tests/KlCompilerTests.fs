namespace KlCompiler.Tests

open NUnit.Framework
open Kl
open Kl.Tokenizer
open KlCompiler
open Fantomas
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open System.IO

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
        let p = tokenize >> KlParser.parse Head >> KlCompiler.build
        let r = AndExpr(BoolExpr true, BoolExpr false) |> KlCompiler.build
        let text = """module Stuff
        
open Kl

let f = new Function("f", 1, [], fun globals -> (fun X -> Completed(ValueResult(KlBuiltins.klIsCons globals X))))
"""
        let parsedInput = Fantomas.CodeFormatter.Parse("./test.fs", text)
        let ast = FsFile.Of(
                      "ShenNs",
                      [FsModule.Of(
                          "ShenStuff",
                          [openKl
                           FsModule.SingleLet(
                              "f",
                              ["envGlobals", FsType.Of("Globals")
                               "X", FsType.Of("KlValue")
                               "Y", FsType.Of("KlValue")],
                              KlCompiler.build(
                                KlExpr.AppExpr(
                                  Position.Head,
                                  KlExpr.SymbolExpr "+",
                                  [SymbolExpr "X"; SymbolExpr "Y"])))])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "ShenAsm", ["Kl.dll"], None)
            Assert.AreEqual(0, i)
            let types = asm.Value.GetTypes()
            let res1 = types.[0].GetMethods().[0].Invoke(null, [|KlBuiltins.newGlobals(); IntValue 1; IntValue 2|])
            assert (res1 = (IntValue 3 :> obj))
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    member this.KlExprToSynExpr() =
        let kl = KlExpr.AndExpr(KlExpr.BoolExpr true, KlExpr.BoolExpr false)
        let syn = KlCompiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
            Assert.AreEqual(0, i)
            let types = asm.Value.GetTypes()
            let methods = types.[0].GetMethods()
            let props = types.[0].GetProperties()
            let fields = types.[0].GetFields()
            let v = methods.[0].Invoke(null, [|KlBuiltins.newGlobals()|])
            Assert.AreEqual(false, v)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()
    
    [<Test>]
    member this.BuildFreezeExpr() =
        let kl = KlExpr.FreezeExpr(KlExpr.AppExpr(Head, KlExpr.SymbolExpr "number?", [KlExpr.StringExpr "hi"]))
        let syn = KlCompiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
            Assert.AreEqual(0, i)
            let types = asm.Value.GetTypes()
            let methods = types.[0].GetMethods()
            let props = types.[0].GetProperties()
            let fields = types.[0].GetFields()
            let v = methods.[0].Invoke(null, [|KlBuiltins.newGlobals()|])
            match v :?> KlValue with
            | FunctionValue _ -> ()
            | _ -> assert false
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    member this.BuildCondExpr() =
        let kl = "(cond ((> X 0) \"positive\") ((< X 0) \"negative\") (true \"zero\"))" |> tokenize |> KlParser.parse Position.Head
        let syn = KlCompiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals"); "X", FsType.Of("KlValue")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
            Assert.AreEqual(0, i)
            let types = asm.Value.GetTypes()
            let methods = types.[0].GetMethods()
            let props = types.[0].GetProperties()
            let fields = types.[0].GetFields()
            let v = methods.[0].Invoke(null, [|KlBuiltins.newGlobals(); KlValue.IntValue(5)|])
            Assert.AreEqual(KlValue.StringValue "positive", v)
            let v2 = methods.[0].Invoke(null, [|KlBuiltins.newGlobals(); KlValue.IntValue(-5)|])
            Assert.AreEqual(KlValue.StringValue "negative", v2)
            let v3 = methods.[0].Invoke(null, [|KlBuiltins.newGlobals(); KlValue.IntValue(0)|])
            Assert.AreEqual(KlValue.StringValue "zero", v3)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    member this.BuildLetExpr() =
        let kl = "(let X 5 (if (> X 0) \"positive\" \"non-positive\"))" |> tokenize |> KlParser.parse Position.Head
        let syn = KlCompiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
            Assert.AreEqual(0, i)
            let types = asm.Value.GetTypes()
            let methods = types.[0].GetMethods()
            let props = types.[0].GetProperties()
            let fields = types.[0].GetFields()
            let v = methods.[0].Invoke(null, [|KlBuiltins.newGlobals()|])
            Assert.AreEqual(KlValue.StringValue "positive", v)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Ignore("relative paths don't work on travis-ci")>]
    [<Test>]
    member this.BuildModule() =
        let src = System.IO.File.ReadAllText(@"..\..\..\KLambda\toplevel.kl")
        let exprs = src |> tokenizeAll |> List.map (KlParser.parse Head) |> Seq.take 6 |> Seq.toList
        let parsedInput = KlCompiler.buildModule exprs
        let str = Fantomas.CodeFormatter.FormatAST(parsedInput, None, formatConfig)
        System.Console.WriteLine(str)
        ()
