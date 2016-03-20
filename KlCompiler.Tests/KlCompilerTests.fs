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

    [<Test>]
    [<Ignore("")>]
    member this.CompilerServicesBuildAst() =
        let p = KlTokenizer.tokenize >> KlParser.parse Head >> KlCompiler.build
        let r = AndExpr(BoolExpr true, BoolExpr false) |> KlCompiler.build
        let s = new SimpleSourceCodeServices()
        let fileName = "..\\..\\..\\..\\test.fs"
        let text = File.ReadAllText(fileName)
        let parsedInput = Fantomas.CodeFormatter.Parse(fileName, text)
        let ast = FsFile.Of(
                      "ShenNs",
                      [FsModule.Of(
                          "ShenStuff",
                          [SynModuleDecl.Open(
                            LongIdentWithDots.LongIdentWithDots([new Ident("Kl", FsAst.defaultRange)], []), FsAst.defaultRange)
                           FsModule.SingleLet(
                              "f",
                              [("KlValue", "x")
                               ("KlValue", "y")],
                              KlCompiler.build(
                                KlExpr.AppExpr(
                                  Position.Head,
                                  KlExpr.SymbolExpr "+",
                                  [SymbolExpr "x"; SymbolExpr "y"])))])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, Fantomas.FormatConfig.FormatConfig.Default)
        try
            let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "ShenAsm", ["Kl.dll"], None)
            let types = asm.Value.GetTypes()
            let res1 = types.[0].GetMethods().[0].Invoke(null, [|IntValue 1; IntValue 2|])
            assert (res1 = (IntValue 3 :> obj))
        with
            ex -> printfn "%s" <| ex.ToString()
        ()

    [<Test>]
    [<Ignore("")>]
    member this.KlExprToSynExpr() =
        let kl = KlExpr.AndExpr(KlExpr.BoolExpr true, KlExpr.BoolExpr false)
        let syn = FsFile.Of("KlExprTest", [FsModule.Of("KlExprTestMod", [FsModule.SingleLet("z", [], KlCompiler.build kl)])])
        let str = Fantomas.CodeFormatter.FormatAST(syn, None, Fantomas.FormatConfig.FormatConfig.Default)
        ()
