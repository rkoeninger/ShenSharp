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
open Generator
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

    let private emitDll name =
        let service = new SimpleSourceCodeServices()
        let options = [|
            "fsc.exe"
            "-o"; name + ".dll"
            "-a"; name + ".fs"
            "-r"; "Kl.dll"
        |]
        let (errors, returnCode) = service.Compile options
        if returnCode <> 0 then
            raise <| new Exception(String.Join(", ", Seq.map (fun (e: FSharpErrorInfo) -> e.Message) errors))

    let cache klFolder klFiles =
        if not(File.Exists "Installer.dll") then
            let globals = load klFolder klFiles
            printfn "Generating installation code..."
            let origGlobals = baseGlobals()
            let syntax = generateInstallerCode origGlobals.Symbols.Keys origGlobals.Functions.Keys globals
            File.WriteAllText("Installer.fs", syntax)
            printfn "Compiling installation code..."
            emitDll "Installer"
            printfn "Installation code cached."
            printfn ""
            globals
        else
            let assembly = Assembly.LoadFile(Path.Combine(Environment.CurrentDirectory, "Installer.dll"))
            let installation = assembly.GetType("Kl.Installation.Installer")
            let install = installation.GetMethod("Install")
            let globals = baseGlobals()
            install.Invoke(null, [|globals|]) |> ignore
            globals

    let outputDll name ast =
        let service = new SimpleSourceCodeServices()
        let (errors, returnCode) = service.Compile([ast], "ShenRuntime", "ShenRuntime.dll", ["Kl.dll"])
        if returnCode <> 0 then
            raise <| new Exception(String.Join("\r\n\r\n", Seq.map (fun (e: FSharpErrorInfo) -> e.ToString()) errors))

    let cacheCompile globals =
        if not(File.Exists "ShenRuntime.dll") then
            printfn "Generating installation code..."
            let ast = compile ["Shen"; "Runtime"] globals
            printfn "Compiling installation code..."
            outputDll "ShenRuntime" ast
            printfn "Installation code cached."
            printfn ""
            globals
        else
            let assembly = Assembly.LoadFile(Path.Combine(Environment.CurrentDirectory, "ShenRuntime.dll"))
            let installation = assembly.GetType("Shen.Runtime")
            let install = installation.GetMethod("NewRuntime")
            install.Invoke(null, [||]) :?> Globals
