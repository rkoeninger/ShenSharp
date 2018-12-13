module Kl.Make.BuildRuntime

open Kl.Values
open Loader
open ShenSharp.Shared

let outputPath = fromRoot ["Artifacts"; BuildConfig]
let sourcePath = fromRoot ["packages"; KernelFolderName; "klambda"]
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
]

let buildRuntime () = make sourcePath sourceFiles outputPath

[<EntryPoint>]
let main _ = separateThread16MB buildRuntime
