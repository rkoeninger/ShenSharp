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

let iff cond ifTrue ifFalse =
    match cond with
    | Ok(BoolValue true) -> ifTrue()
    | Ok(BoolValue false) -> ifFalse()
    | Ok _ -> Err "Bool expected"
    | Err message -> Err message
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
                               "X", FsType.Of("Value")
                               "Y", FsType.Of("Value")],
                              Compiler.build(
                                Expr.AppExpr(
                                  Position.Head,
                                  Expr.SymbolExpr "+",
                                  [SymbolExpr "X"; SymbolExpr "Y"])))])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "ShenAsm", ["Kl.dll"], None)
            Assert.AreEqual(0, i)
            let types = asm.Value.GetTypes()
            let res1 = types.[0].GetMethods().[0].Invoke(null, [|Values.newGlobals(); IntValue 1; IntValue 2|])
            assert (res1 = (Result.Ok(IntValue 3) :> obj))
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    [<Ignore("and/or exprs need to be fixed like if exprs")>]
    member this.KlExprToSynExpr() =
        let kl = Expr.AndExpr(Expr.BoolExpr true, Expr.BoolExpr false)
        let syn = Compiler.build kl
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
            let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
            Assert.AreEqual(false, v)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()
    
    [<Test>]
    [<Ignore("functions aren't getting built properly")>]
    member this.BuildFreezeExpr() =
        let kl = Expr.FreezeExpr(Expr.AppExpr(Head, Expr.SymbolExpr "number?", [Expr.StringExpr "hi"]))
        let syn = Compiler.build kl
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
            let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
            match v :?> Value with
            | FunctionValue _ -> ()
            | _ -> assert false
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    [<Ignore("cond expres need to be fixed like if exprs")>]
    member this.BuildCondExpr() =
        let kl = "(cond ((> X 0) \"positive\") ((< X 0) \"negative\") (true \"zero\"))" |> tokenize |> parse Position.Head
        let syn = Compiler.build kl
        let ast = singleBinding ["envGlobals", FsType.Of("Globals"); "X", FsType.Of("Value")] syn
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
            let v = methods.[0].Invoke(null, [|Values.newGlobals(); Value.IntValue(5)|])
            Assert.AreEqual(Result.Ok(Value.StringValue "positive"), v)
            let v2 = methods.[0].Invoke(null, [|Values.newGlobals(); Value.IntValue(-5)|])
            Assert.AreEqual(Result.Ok(Value.StringValue "negative"), v2)
            let v3 = methods.[0].Invoke(null, [|Values.newGlobals(); Value.IntValue(0)|])
            Assert.AreEqual(Result.Ok(Value.StringValue "zero"), v3)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    member this.BuildLetExpr() =
        let kl = "(let X 5 (if (> X 0) \"positive\" \"non-positive\"))" |> tokenize |> parse Position.Head
        let syn = Compiler.build kl
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
            let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
            Assert.AreEqual(Result.Ok(StringValue "positive"), v)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Ignore("relative paths don't work on travis-ci")>]
    [<Test>]
    member this.BuildModule() =
        let src = System.IO.File.ReadAllText(@"..\..\..\KLambda\toplevel.kl")
        let exprs = src |> tokenizeAll |> List.map rootParse |> Seq.take 6 |> Seq.toList
        let parsedInput = Compiler.buildModule exprs
        let str = Fantomas.CodeFormatter.FormatAST(parsedInput, None, formatConfig)
        System.Console.WriteLine(str)
        ()
