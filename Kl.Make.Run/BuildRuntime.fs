module Kl.Make.Run.BuildRuntime

open System.IO
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Make.Loader

let klFolder = combine [".."; ".."; ".."; "Distribution"; "Kl"]
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
    Directory.CreateDirectory(Path.GetDirectoryName destination) |> ignore
    File.WriteAllBytes(destination, File.ReadAllBytes source)

#if DEBUG
let buildConfig = "Debug"
#else
let buildConfig = "Release"
#endif

let dllName = "Shen.Runtime.dll"

let buildRuntime () =
    make klFolder klFiles
    printfn "Copying dll to dependent projects..."
    copy dllName (combine [".."; ".."; ".."; "Artifacts"; buildConfig; dllName])

[<EntryPoint>]
let main _ = separateThread buildRuntime
