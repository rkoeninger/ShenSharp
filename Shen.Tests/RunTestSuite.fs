module Shen.Tests

open System
open System.IO
open System.Threading
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Load.Compiler

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
    let globals = compile klFolder klFiles
    globals.Functions.["y-or-n?"] <- Native("y-or-n?", 1, fun _ _ -> True)
    Environment.CurrentDirectory <- Path.Combine(Environment.CurrentDirectory, testFolder)
    globals.Symbols.["*home-directory*"] <- Str(Environment.CurrentDirectory.Replace('\\', '/'))
    eval globals (toCons [Sym "load"; Str "README.shen"]) |> ignore
    eval globals (toCons [Sym "load"; Str "tests.shen"]) |> ignore
    printfn ""
    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore

[<EntryPoint>]
let main args =
    let thread = new Thread(runTestSuite, stackSize)
    thread.Start()
    thread.Join()
    0
