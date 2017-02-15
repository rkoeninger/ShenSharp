namespace Kl.Import

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Fantomas
open Fantomas.FormatConfig
open Kl
open Kl.Evaluator
open Kl.Startup
open Reader
open Compiler

module Loader =

    let private load klFolder klFiles =
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

    let nameParts = ["Shen"; "Runtime"]
    let joinedName = String.Join(".", nameParts)
    let fileName = sprintf "%s.dll" joinedName
    let deps = ["Kl.dll"]

    let errorsToString (errors: FSharpErrorInfo seq) =
        String.Join("\r\n\r\n", Seq.map (fun (e: FSharpErrorInfo) -> e.ToString()) errors)

    let emitDll ast =
        let service = new SimpleSourceCodeServices()
        let (errors, returnCode) = service.Compile([ast], joinedName, fileName, deps)
        if returnCode <> 0 then
            raise <| new Exception(errorsToString errors)

    let cache klFolder klFiles =
        if not(File.Exists fileName) then
            let globals = load klFolder klFiles
            printfn "Generating installation code..."
            let ast = compile nameParts globals
            printfn "Compiling installation code..."
            emitDll ast
            printfn "Installation code cached."
            printfn ""
        let assembly = Assembly.LoadFile(Path.Combine(Environment.CurrentDirectory, fileName))
        let installation = assembly.GetType(joinedName)
        let install = installation.GetMethod("NewRuntime")
        install.Invoke(null, [||]) :?> Globals
