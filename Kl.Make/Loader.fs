module Kl.Make.Loader

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices
open Kl.Values
open Kl.Evaluator
open Kl.Startup
open Reader
open Compiler
open ShenSharp.Shared

let private dllName = sprintf "%s.dll" GeneratedModule
let private pdbName = sprintf "%s.pdb" GeneratedModule
let private searchPattern = sprintf "%s.*" GeneratedModule
let private deps = ["Kl.dll"]
let private sharedMetadataPath = combine [".."; ".."; ".."; "Shared.fs"]
let private contentPath = "Content"
let private metadataPath = combine [contentPath; "Metadata.fs.txt"]
let private runtimePath = combine [contentPath; "Runtime.fs.txt"]
let private extensionMethodsPath = combine [contentPath; "ExtensionMethods.fs.txt"]

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
    printfn ""
    globals

let private raiseErrors messages =
    let isError (m: FSharpErrorInfo) = m.Severity = FSharpErrorSeverity.Error
    let errors = Seq.filter isError messages
    raise(Exception(String.Join("\r\n\r\n", Seq.map string errors)))

let private parseFile file =
    let input = File.ReadAllText file
    let trimmedFile = file.Replace(".txt", "")
    let checker = FSharpChecker.Create()
    let projOptions =
        checker.GetProjectOptionsFromScript(trimmedFile, input)
        |> Async.RunSynchronously
    let result =
        checker.ParseFileInProject(trimmedFile, input, projOptions)
        |> Async.RunSynchronously
    match result.ParseTree with
    | Some tree -> tree
    | None -> raiseErrors result.Errors

let private emit asts =
    let service = SimpleSourceCodeServices()
    let (errors, returnCode) =
        service.Compile(
            asts,
            GeneratedModule,
            dllName,
            deps,
            pdbName,
            false,
            false)
    if returnCode <> 0 then
        raiseErrors errors

let private copy source destination =
    if File.Exists destination then
        File.Delete destination
    Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
    File.WriteAllBytes(destination, File.ReadAllBytes source)

let make sourcePath sourceFiles outputPath =
    let globals = import sourcePath sourceFiles
    printfn "Generating installation code..."
    let installationAst = buildInstallationFile GeneratedModule globals
    let sharedAst = parseFile sharedMetadataPath
    let metadataAst = parseFile metadataPath
    let runtimeAst = parseFile runtimePath
    let extensionMethodsAst = parseFile extensionMethodsPath
    printfn "Compiling installation code..."
    emit [sharedAst; metadataAst; installationAst; runtimeAst; extensionMethodsAst]
    printfn "Copying artifacts to output path..."
    for file in Directory.GetFiles(".", searchPattern) do
        copy file (combine [outputPath; file])
