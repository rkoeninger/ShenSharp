module Shen.BuildRuntime

open System.IO
open System.Threading
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Make.Loader

let stackSize = 16777216
let klFolder = @"..\..\..\Distribution\Kl"
let klFiles = [
    "toplevel.kl"
    "core.kl"
    "sys.kl"
    "sequent.kl"
    "yacc.kl"
    "reader.kl"
    "prolog.kl"
    "track.kl"
    "load.kl"
    "writer.kl"
    "macros.kl"
    "declarations.kl"
    "types.kl"
    "t-star.kl"
]

let copy source destination =
    if File.Exists destination then
        File.Delete destination
    File.WriteAllBytes(destination, File.ReadAllBytes source)

#if DEBUG
let buildConfig = "Debug"
#else
let buildConfig = "Release"
#endif

let dllName = "Shen.Runtime.dll"
let projects = ["Shen.Repl"; "Shen.Tests"]

let buildRuntime () =
    doCompile klFolder klFiles
    printfn "Copying dll to dependent projects..."
    for project in projects do
        copy dllName (sprintf @"..\..\..\%s\bin\%s\%s" project buildConfig dllName)

[<EntryPoint>]
let main args =
    let thread = new Thread(buildRuntime, stackSize)
    thread.Start()
    thread.Join()
    0
