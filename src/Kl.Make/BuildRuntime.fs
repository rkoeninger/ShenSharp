module Kl.Make.BuildRuntime

open Kl.Values
open Loader
open ShenSharp.Shared

let sourcePath = fromRoot ["kernel"; "klambda"]
let sourceFiles = [
   "sys.kl"
   "writer.kl"
   "core.kl" 
   "reader.kl"
   "declarations.kl" 
   "toplevel.kl"
   "macros.kl"
   "load.kl" 
   "prolog.kl"
   "sequent.kl"
   "track.kl" 
   "t-star.kl"
   "yacc.kl"
   "types.kl"
]

let buildRuntime () = make sourcePath sourceFiles

[<EntryPoint>]
let main _ = separateThread128MB buildRuntime
