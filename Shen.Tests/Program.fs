module Shen.Tests

open System
open System.IO
open System.Threading
open Kl
open Kl.Values
open Kl.Reader
open Kl.Evaluator
open Kl.Builtins
open Kl.Startup

let stackSize = 16777216
let testFolder = @"..\..\..\Shen.Tests.Source"
let klFolder = @"..\..\..\Kl.Source"
let files = [
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
let load env path = eval env (toCons [Sym "load"; Str path]) |> ignore

let main0 () =
    let env = baseEnv()
    Overrides.overrides.["y-or-n?"] <- Native("y-or-n?", 1, fun _ _ -> truev)
    for file in files do
        printfn "Loading %s" file
        for ast in readAll(File.ReadAllText(Path.Combine(klFolder, file))) do
            rootEval env.Globals ast |> ignore
    Environment.CurrentDirectory <- Path.Combine(Environment.CurrentDirectory, testFolder)
    load env "README.shen"
    load env "tests.shen"
    printfn ""
    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore
    0

[<EntryPoint>]
let main args =
    let mutable returnCode = 0
    let thread = new Thread((fun () -> returnCode <- main0 ()), stackSize)
    thread.Start()
    thread.Join()
    returnCode
