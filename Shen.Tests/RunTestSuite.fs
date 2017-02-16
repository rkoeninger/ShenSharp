module Shen.Tests

open System.Threading
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Make.Loader

let stackSize = 16777216
let testFolder = @"..\..\..\Distribution\Tests"
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

let runTestSuite () =
    let globals = cache klFolder klFiles
    eval globals (toCons [Sym "cd"; Str testFolder]) |> ignore
    eval globals (toCons [Sym "load"; Str "README.shen"]) |> ignore
    eval globals (toCons [Sym "load"; Str "tests.shen"]) |> ignore

[<EntryPoint>]
let main args =
    let thread = new Thread(runTestSuite, stackSize)
    thread.Start()
    thread.Join()
    0
