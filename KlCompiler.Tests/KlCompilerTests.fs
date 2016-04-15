namespace KlCompiler.Tests

open NUnit.Framework
open Kl
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

    [<Test>]
    member this.CompilerServicesBuildAst() =
        let p = KlTokenizer.tokenize >> KlParser.parse Head >> KlCompiler.build
        let r = AndExpr(BoolExpr true, BoolExpr false) |> KlCompiler.build
        let text = """
namespace Testing

open Kl

module Test =
    let d = new DateTime()
    let t = (1, 2)
    let e = true && true
    let rec f (y:int) = if y >= 0 then y else f (y + 1)
    and g x y = if y = 1 then x else x + (y - 1 |> g x)
    let h = 123
        """
        let parsedInput = Fantomas.CodeFormatter.Parse("./test.fs", text)
        let ast = FsFile.Of(
                      "ShenNs",
                      [FsModule.Of(
                          "ShenStuff",
                          [openKl
                           FsModule.SingleLet(
                              "f",
                              [("KlValue", "x")
                               ("KlValue", "y")],
                              KlCompiler.build(
                                KlExpr.AppExpr(
                                  Position.Head,
                                  KlExpr.SymbolExpr "+",
                                  [SymbolExpr "x"; SymbolExpr "y"])))])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "ShenAsm", ["Kl.dll"], None)
            Assert.IsEmpty(errors)
            let types = asm.Value.GetTypes()
            let res1 = types.[0].GetMethods().[0].Invoke(null, [|IntValue 1; IntValue 2|])
            assert (res1 = (IntValue 3 :> obj))
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    member this.KlExprToSynExpr() =
        let kl = KlExpr.AndExpr(KlExpr.BoolExpr true, KlExpr.BoolExpr false)
        let syn = KlCompiler.build kl
        let ast =
            FsFile.Of(
                "KlExprTest",
                [FsModule.Of(
                    "KlExprTestMod",
                    [openKl
                     FsModule.SingleLet("z", [], syn)])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
            Assert.IsEmpty(errors)
            let types = asm.Value.GetTypes()
            let methods = types.[0].GetMethods()
            let props = types.[0].GetProperties()
            let fields = types.[0].GetFields()
            let v = props.[0].GetValue(null)
            Assert.AreEqual(false, v)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    [<Ignore("")>]
    member this.BuildFreezeExpr() =
        let kl = KlExpr.FreezeExpr(KlExpr.AppExpr(Head, KlExpr.SymbolExpr "number?", [KlExpr.StringExpr "hi"]))
        let syn = KlCompiler.build kl
        let ast =
            FsFile.Of(
                "KlExprTest",
                [FsModule.Of(
                    "KlExprTestMod",
                    [openKl
                     FsModule.SingleLet("z", [], syn)])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
            Assert.IsEmpty(errors)
            let types = asm.Value.GetTypes()
            let methods = types.[0].GetMethods()
            let props = types.[0].GetProperties()
            let fields = types.[0].GetFields()
            let v = props.[0].GetValue(null)
            //Assert.AreEqual(false, v)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()

    [<Test>]
    member this.BuildCondExpr() =
        let kl = "(cond ((> x 0) \"positive\") ((< x 0) \"negative\") (true \"zero\"))" |> KlTokenizer.tokenize |> KlParser.parse Position.Head
        let syn = KlCompiler.build kl
        let ast =
            FsFile.Of(
                "KlExprTest",
                [FsModule.Of(
                    "KlExprTestMod",
                    [openKl
                     FsModule.SingleLet("z", ["KlValue", "x"], syn)])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        try
            let s = new SimpleSourceCodeServices()
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
            Assert.IsEmpty(errors)
            let types = asm.Value.GetTypes()
            let methods = types.[0].GetMethods()
            let props = types.[0].GetProperties()
            let fields = types.[0].GetFields()
            let v = methods.[0].Invoke(null, [|KlValue.IntValue(5)|])
            Assert.AreEqual(KlValue.StringValue "positive", v)
            let v2 = methods.[0].Invoke(null, [|KlValue.IntValue(-5)|])
            Assert.AreEqual(KlValue.StringValue "negative", v2)
            let v3 = methods.[0].Invoke(null, [|KlValue.IntValue(0)|])
            Assert.AreEqual(KlValue.StringValue "zero", v3)
            ()
        with
            ex -> printfn "%s" <| ex.ToString()
                  assert false
        ()