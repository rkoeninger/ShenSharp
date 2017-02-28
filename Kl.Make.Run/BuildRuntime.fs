module Kl.Make.Run.BuildRuntime

open Kl.Values
open Kl.Make.Loader

#if DEBUG
let buildConfig = "Debug"
#else
let buildConfig = "Release"
#endif

let outputPath = combine [".."; ".."; ".."; "Artifacts"; buildConfig]
let sourcePath = combine [".."; ".."; ".."; "Distribution"; "Kl"]
let sourceFiles = [
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

let buildRuntime () = make sourcePath sourceFiles outputPath

[<EntryPoint>]
let main _ = separateThread buildRuntime
