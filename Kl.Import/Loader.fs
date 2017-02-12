namespace Kl.Import

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Kl.Evaluator
open Kl.Startup
open Reader
open Generator

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
            // TODO: method name should be Shen.Runtime.Install
            let assembly = Assembly.LoadFile(Path.Combine(Environment.CurrentDirectory, "Installer.dll"))
            let installation = assembly.GetType("Kl.Installation.Installer")
            let install = installation.GetMethod("Install")
            let globals = baseGlobals()
            install.Invoke(null, [|globals|]) |> ignore
            globals
