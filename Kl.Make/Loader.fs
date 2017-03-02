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

let private nameParts = ["Shen"; "Runtime"]
let private joinedName = String.Join(".", nameParts)
let private dllName = sprintf "%s.dll" joinedName
let private pdbName = sprintf "%s.pdb" joinedName
let private searchPattern = sprintf "%s.*" joinedName
let private deps = ["Kl.dll"]
let private sharedMetadataPath = combine [".."; ".."; ".."; "Shared.fs"]

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
    let errors = Seq.filter (fun (m: FSharpErrorInfo) -> m.Severity = FSharpErrorSeverity.Error) messages
    raise(new Exception(String.Join("\r\n\r\n", Seq.map string errors)))

let private parseFile file =
    let input = File.ReadAllText file
    let checker = FSharpChecker.Create()
    let projOptions =
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously
    let result =
        checker.ParseFileInProject(file, input, projOptions)
        |> Async.RunSynchronously
    match result.ParseTree with
    | Some tree -> tree
    | None -> raiseErrors result.Errors

let private emit asts =
    let service = new SimpleSourceCodeServices()
    let (errors, returnCode) = service.Compile(asts, joinedName, dllName, deps, pdbName, false, false)
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
    let ast = buildInstallationFile nameParts globals
    printfn "Compiling installation code..."
    emit [ast; parseFile sharedMetadataPath; buildMetadataFile "Shen.Runtime"]
    printfn "Copying artifacts to output path..."
    for file in Directory.GetFiles(".", searchPattern) do
        copy file (combine [outputPath; file])
