module Kl.Make.BuildRuntime

open System
open System.IO
open Kl.Values
open Loader
open ShenSharp.Shared

let findRoot () =
    let mutable current = Environment.CurrentDirectory
    while not <| current.EndsWith("ShenSharp") do
        current <- Path.GetDirectoryName current
    current
let root = findRoot()
let fromRoot = combine << (@) [root]
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
    "init.kl"
]

let buildRuntime () = make sourcePath sourceFiles outputPath

[<EntryPoint>]
let main _ = separateThread16MB buildRuntime
