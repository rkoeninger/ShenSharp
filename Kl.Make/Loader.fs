module Kl.Make.Loader

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Kl.Values
open Reader
open Compiler
open ShenSharp.Shared

let private dllName = sprintf "%s.dll" GeneratedModule
let private pdbName = sprintf "%s.pdb" GeneratedModule
let private searchPattern = sprintf "%s.*" GeneratedModule
let private deps = ["Kl.dll"]
let private sharedMetadataPath = fromRoot ["Shared.fs"]

let private import sourcePath sourceFiles =
    sourceFiles
    |> List.collect (fun f -> combine [sourcePath; f] |> File.ReadAllText |> readAll)
    |> List.filter isCons

let private filterMessages severity messages = Seq.filter (fun (m: FSharpErrorInfo) -> m.Severity = severity) messages

let private logWarnings messages =
    messages |> filterMessages FSharpErrorSeverity.Warning |> Seq.iter (fun (m: FSharpErrorInfo) -> printfn "%O" m)

let private raiseErrors messages =
    let errors = filterMessages FSharpErrorSeverity.Error messages
    raise(Exception(String.Join("\r\n\r\n", Seq.map string errors)))

let private handleResults (value, messages) =
    logWarnings messages
    if filterMessages FSharpErrorSeverity.Error messages |> Seq.length > 0
        then raiseErrors messages
        else value

let private parseFile (checker: FSharpChecker) file =
    let input = SourceText.ofString(File.ReadAllText file)
    let projOptions =
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously
        |> handleResults
    let parsingOptions =
        checker.GetParsingOptionsFromProjectOptions projOptions
        |> handleResults
    let result =
        checker.ParseFile(file, input, parsingOptions)
        |> Async.RunSynchronously
    logWarnings result.Errors
    match result.ParseTree with
    | Some tree -> tree
    | None -> raiseErrors result.Errors

// TODO: specify arguments to exclude mscorlib.dll

let private emit (checker: FSharpChecker) asts =
    let (errors, _) =
        checker.Compile(
            asts,
            GeneratedModule,
            dllName,
            deps,
            pdbName,
            false,
            true)
        |> Async.RunSynchronously
    handleResults ((), errors)

let private copy source destination =
    if File.Exists destination then
        File.Delete destination
    Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
    File.WriteAllBytes(destination, File.ReadAllBytes source)

let make sourcePath sourceFiles outputPath =
    let checker = FSharpChecker.Create()
    let exprs = import sourcePath sourceFiles
    printfn "Translating kernel..."
    let ast = buildInstallationFile GeneratedModule exprs
    let sharedAst = parseFile checker sharedMetadataPath
    let metadataAst = buildMetadataFile GeneratedModule
    printfn "Compiling kernel..."
    emit checker [ast; sharedAst; metadataAst]
    printfn "Copying artifacts to output path..."
    for file in Directory.GetFiles(".", searchPattern) do
        copy file (combine [outputPath; file])
    printfn "Done."
