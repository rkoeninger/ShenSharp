namespace Kl.Make

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Kl.Evaluator
open Kl.Startup
open Reader
open Compiler

module Loader =

    let private nameParts = ["Shen"; "Runtime"]
    let private joinedName = String.Join(".", nameParts)
    let private fileName = sprintf "%s.dll" joinedName
    let private deps = ["Kl.dll"]

    let private import klFolder klFiles =
        let globals = baseGlobals()
        for file in klFiles do
            printf "Loading %s " file
            stdout.Flush()
            let text = File.ReadAllText(Path.Combine(klFolder, file))
            for ast in readAll text do
                printf "."
                eval globals ast |> ignore
            printfn ""
        printfn ""
        globals

    let private emit ast =
        let service = new SimpleSourceCodeServices()
        let (errors, returnCode) = service.Compile([ast], joinedName, fileName, deps)
        if returnCode <> 0 then
            raise <| new Exception(String.Join("\r\n\r\n", Seq.map string errors))

    let private copy source destination =
        if File.Exists destination then
            File.Delete destination
        Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
        File.WriteAllBytes(destination, File.ReadAllBytes source)

    let make sourcePath sourceFiles outputPath =
        let globals = import sourcePath sourceFiles
        printfn "Generating installation code..."
        let ast = compile nameParts globals
        printfn "Compiling installation code..."
        emit ast
        printfn "Copying dll to dependent projects..."
        copy fileName (Path.Combine(outputPath, fileName))
