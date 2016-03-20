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
        let fileName = "..\\..\\..\\..\\test.fs"
        let text = File.ReadAllText(fileName)
        let parsedInput = Fantomas.CodeFormatter.Parse(fileName, text)
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
