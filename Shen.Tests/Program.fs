module Shen.Tests

open System
open System.IO
open Kl
open Kl.Reader
open Kl.Evaluator
open Kl.Startup

let klFolder = @"..\..\..\KLambda"
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
let load env path = eval env (Cons(Sym "load", Cons(Str path, Empty))) |> ignore

[<EntryPoint>]
let main argv =
    let env = baseEnv()
    for file in files do
        printfn "Loading %s" file
        for ast in readAll(File.ReadAllText(Path.Combine(klFolder, file))) do
            match ast with
            | Str _ -> ()
            | _ -> rootEval env.Globals ast |> ignore
    Environment.CurrentDirectory <- Path.Combine(Environment.CurrentDirectory, "Tests")
    load env "README.shen"
    load env "tests.shen"
    printfn ""
    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore
    0
