module Shen

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

let runRepl files =
    let globals = cache klFolder klFiles

    if Array.isEmpty files then
        eval globals (toCons [Sym "shen.shen"]) |> ignore
    else
        for file in files do
            eval globals (toCons [Sym "load"; Str file]) |> ignore

[<EntryPoint>]
let main args =
    let thread = new Thread((fun () -> runRepl args), stackSize)
    thread.Start()
    thread.Join()
    0
