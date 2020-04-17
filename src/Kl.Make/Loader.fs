module Kl.Make.Loader

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Kl
open Kl.Values
open Reader
open Compiler
open Writer
open ShenSharp.Shared

let private dllName = sprintf "%s.dll" GeneratedModule
let private pdbName = sprintf "%s.pdb" GeneratedModule
let private deps = ["Kl.dll"]
let private sharedMetadataPath = fromRoot ["src"; "Shared.fs"]

let private import sourcePath =
    List.collect (fun f -> combine [sourcePath; f] |> File.ReadAllText |> readAll)

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

let private move source destination =
    if File.Exists destination then
        File.Delete destination
    Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
    File.Move(source, destination)

let private filterDefuns excluded =
    let filter = function
        | Form [Sym "defun"; Sym name; _; _] -> List.contains name excluded |> not
        | _ -> false // Exclude all non-defuns too
    List.filter filter

let make sourcePath sourceFiles outputPath =
    let checker = FSharpChecker.Create()
    let exprs = import sourcePath sourceFiles |> filterDefuns ["cd"]
    printfn "Translating kernel..."
    let ast = buildInstallationFile GeneratedModule exprs
    let sharedAst = parseFile checker sharedMetadataPath
    let metadataAst = buildMetadataFile GeneratedModule Copyright Revision BuildConfig
    printfn "Writing kernel..."
    File.WriteAllText(combine [outputPath; "Kernel.fs"], writeFile ast)
    //printfn "Compiling kernel..."
    //emit checker [ast; sharedAst; metadataAst]
    //printfn "Copying artifacts to output path..."
    //move dllName (combine [outputPath; dllName])
    printfn "Done."
