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

    let assertEq (x: 'a) (y: 'a) = Assert.AreEqual(x, y)
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
        let p = tokenize >> parse Head >> Compiler.build "test" Set.empty
        let r = AndExpr(BoolExpr true, BoolExpr false) |> Compiler.build "test" Set.empty
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
                              [Compiler.globalsParam
                               "X", FsType.Of("Value")
                               "Y", FsType.Of("Value")],
                              Compiler.build "test" (Set.ofList ["X"; "Y"]) (
                                Expr.AppExpr(
                                  Position.Head,
                                  Expr.SymExpr "+",
                                  [SymExpr "X"; SymExpr "Y"])))])])
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "ShenAsm", ["Kl.dll"], None)
        for e in errors do
            printfn "%s" e.Message
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let res1 = types.[0].GetMethods().[0].Invoke(null, [|Values.newGlobals(); Int 1; Int 2|])
        Assert.AreEqual(res1, ((Int 3) :> obj))

    [<Test>]
    member this.KlExprToSynExpr() =
        let kl = Expr.AndExpr(Expr.BoolExpr true, Expr.BoolExpr false)
        let syn = Compiler.build "test" Set.empty kl
        let ast = singleBinding [Compiler.globalsParam] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        for e in errors do
            printfn "%s" e.Message
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
        Assert.AreEqual(Values.falsev, v)
    
    [<Test>]
    member this.BuildFreezeExpr() =
        let kl = Expr.FreezeExpr(Expr.AppExpr(Head, Expr.SymExpr "number?", [Expr.StrExpr "hi"]))
        let syn = Compiler.build "test" Set.empty kl
        let ast = singleBinding [Compiler.globalsParam] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        for e in errors do
            printfn "%s" e.Message
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
        match v :?> Value with
        | Func _ -> ()
        | _ -> Assert.Fail("function expected")

    [<Test>]
    member this.BuildCondExpr() =
        let kl = "(cond ((> X 0) \"positive\") ((< X 0) \"negative\") (true \"zero\"))" |> tokenize |> parse Position.Head
        let syn = Compiler.build "test" (Set.singleton "X") kl
        let ast = singleBinding [Compiler.globalsParam; "X", FsType.Of("Value")] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        for e in errors do
            printfn "%s" e.Message
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals(); Value.Int(5)|])
        Assert.AreEqual(Value.Str "positive", v)
        let v2 = methods.[0].Invoke(null, [|Values.newGlobals(); Value.Int(-5)|])
        Assert.AreEqual(Value.Str "negative", v2)
        let v3 = methods.[0].Invoke(null, [|Values.newGlobals(); Value.Int(0)|])
        Assert.AreEqual(Value.Str "zero", v3)

    [<Test>]
    member this.BuildLetExpr() =
        let kl = "(let X 5 (if (> X 0) \"positive\" \"non-positive\"))" |> tokenize |> parse Position.Head
        let syn = Compiler.build "test" Set.empty kl
        let ast = singleBinding [Compiler.globalsParam] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        for e in errors do
            printfn "%s" e.Message
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
        Assert.AreEqual(Str "positive", v)

    [<Test>]
    member this.BuildModule() =
        let workingDirectory = Path.GetDirectoryName((new Uri(Assembly.GetExecutingAssembly().CodeBase)).LocalPath)
        let src = File.ReadAllText(Path.Combine(workingDirectory, "../../../KLambda/toplevel.kl"))
        let exprs = src |> tokenizeAll |> List.map rootParse |> Seq.take 6 |> Seq.toList
        let parsedInput = Compiler.buildModule exprs
        let str = Fantomas.CodeFormatter.FormatAST(parsedInput, None, formatConfig)
        System.Console.WriteLine(str)

    [<Test>]
    member this.``compiler should keep track of local variables so it know what to emit as variable or idle symbol``() =
        let expr = parse Head (tokenize "(cons A (cons B (cons C ())))")
        let syn = Compiler.build "test" Set.empty expr
        let ast = singleBinding [Compiler.globalsParam] syn
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        System.Console.WriteLine(str)
        let s = new SimpleSourceCodeServices()
        let (errors, i, asm) = s.CompileToDynamicAssembly([ast], "KlExprTest", ["Kl.dll"], None)
        for e in errors do
            printfn "%s" e.Message
        Assert.AreEqual(0, i)
        let types = asm.Value.GetTypes()
        let methods = types.[0].GetMethods()
        let props = types.[0].GetProperties()
        let fields = types.[0].GetFields()
        let v = methods.[0].Invoke(null, [|Values.newGlobals()|])
        ()

    [<Test>]
    [<Ignore("would only work locally/not needed?")>]
    member this.``dump code to disk and compile it there``() =
        let kl = """
        (defun pi () 3.1415926)

        (defun sq (X) (* X X))

        (defun circle-area (R) (* (pi) (sq R)))
        """       |> tokenizeAll |> List.map rootParse
        let ast = Compiler.buildModule kl
        let str = Fantomas.CodeFormatter.FormatAST(ast, None, formatConfig)
        let srcPath = Path.Combine(Path.GetTempPath(), "klcompiler", Guid.NewGuid().ToString() + ".fs")
        if not(Directory.Exists(Path.GetDirectoryName(srcPath))) then
            Directory.CreateDirectory(Path.GetDirectoryName(srcPath)) |> ignore
        File.WriteAllText(srcPath, str)
        let fsCorePath = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
        let mscorlibPath = @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\mscorlib.dll"
        let binDir = TestContext.CurrentContext.TestDirectory
        let klPath = Path.Combine(binDir, @"..\..\..\Kl\bin\Debug\Kl.dll")
        let outDir = Path.Combine(binDir, "out")
        if not(Directory.Exists(outDir)) then
            Directory.CreateDirectory(outDir) |> ignore
        let outPath = Path.Combine(outDir, "Shen.Core.dll")
        let pdbPath = Path.Combine(outDir, "Shen.Core.pdb")
        let s = new SimpleSourceCodeServices()
        let (errors, i) = s.Compile([ast], "Shen.Core", outPath, [mscorlibPath; fsCorePath; klPath], pdbPath, false, true)
        for e in errors do
            printfn "%s" e.Message
        Assert.AreEqual(0, i)
        let asm = Assembly.LoadFrom(outPath)
        let typ = asm.GetType("Shen", true)
        let methods = typ.GetMethods()
        let m = Array.find (fun (m: MethodInfo) -> m.Name = "circle-area") methods
        let v = m.Invoke(null, [|Values.newGlobals(); [Int 5]|]) :?> Value
        match v with
        | Dec i ->
            if i > 70m && i < 80m
                then ()
                else Assert.Fail "decimal value out of range"
        | _ -> Assert.Fail "Expected decimal value"
