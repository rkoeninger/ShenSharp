module Kl.Make.BuildRuntime

open Kl.Values
open Loader
open ShenSharp.Shared

let outputPath = fromRoot ["kernel"; "dotnet"]
let sourcePath = fromRoot ["kernel"; "klambda"]
let sourceFiles = [
    "toplevel.kl"
    "core.kl"
    "sys.kl"
    "dict.kl"
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
    "init.kl"
]

let buildRuntime () = make sourcePath sourceFiles outputPath

[<EntryPoint>]
let main _ = separateThread128MB buildRuntime
