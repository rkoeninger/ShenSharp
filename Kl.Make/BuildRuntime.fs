module Kl.Make.BuildRuntime

open Kl.Values
open Loader
open ShenSharp.Shared

let outputPath = combine [".."; ".."; ".."; "Artifacts"; BuildConfig]
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
