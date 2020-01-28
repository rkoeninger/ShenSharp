module Kl.Make.Loader

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Kl
open Kl.Evaluator
open Kl.Startup
open Kl.Values
open Reader
open Compiler
open ShenSharp.Shared

let private dllName = sprintf "%s.dll" generatedModule
let private pdbName = sprintf "%s.pdb" generatedModule
let private searchPattern = sprintf "%s.*" generatedModule
let private deps = ["Kl.dll"]
let private sharedMetadataPath = combine [".."; ".."; ".."; ".."; "Shared.fs"]

let private import sourcePath sourceFiles =
    let globals = baseGlobals()
    for file in sourceFiles do
        printf "Loading %s " file
        stdout.Flush()
        let text = File.ReadAllText(combine [sourcePath; file])
        for ast in readAll text do
            printf "."
            eval globals ast |> ignore
        printfn ""
    printfn "shen.initialise..."
    eval globals <| toCons [Sym "shen.initialise"] |> ignore
    printfn ""
    printfn "Applying post-import declarations..."
    postImport globals

let private raiseErrors messages =
    let errors = Seq.filter (fun (m: FSharpErrorInfo) -> m.Severity = FSharpErrorSeverity.Error) messages
    raise(Exception(String.Join("\r\n\r\n", Seq.map string errors)))

let private parseFile (checker: FSharpChecker) file =
    let input = SourceText.ofString(File.ReadAllText file)
    let projOptions, projErrors =
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously
    if not projErrors.IsEmpty then
        raiseErrors projErrors
    let parsingOptions, parsingErrors =
        checker.GetParsingOptionsFromProjectOptions projOptions
    if not parsingErrors.IsEmpty then
        raiseErrors parsingErrors
    let result =
        checker.ParseFile(file, input, parsingOptions)
        |> Async.RunSynchronously
    match result.ParseTree with
    | Some tree -> tree
    | None -> raiseErrors result.Errors

let private emit (checker: FSharpChecker) asts =
    let (errors, returnCode) =
        checker.Compile(
            asts,
            generatedModule,
            dllName,
            deps,
            pdbName,
            false,
            false)
        |> Async.RunSynchronously
    if returnCode <> 0 then
        raiseErrors errors

let private copy source destination =
    if File.Exists destination then
        File.Delete destination
    Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
    File.WriteAllBytes(destination, File.ReadAllBytes source)

let make sourcePath sourceFiles outputPath =
    let checker = FSharpChecker.Create()
    let globals = import sourcePath sourceFiles
    printfn "Translating kernel..."
    let ast = buildInstallationFile generatedModule globals
    let sharedAst = parseFile checker sharedMetadataPath
    let metadataAst = buildMetadataFile generatedModule
    printfn "Compiling kernel..."
    emit checker [ast; sharedAst; metadataAst]
    printfn "Copying artifacts to output path..."
    for file in Directory.GetFiles(".", searchPattern) do
        copy file (combine [outputPath; file])
    printfn "Done."
