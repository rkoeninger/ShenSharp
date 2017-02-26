module Shen.Tests

open System.Threading
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Make.Loader

let stackSize = 16777216
let testFolder = @"..\..\..\Distribution\Tests"

let runTestSuite () =
    let globals = load()
    eval globals (toCons [Sym "cd"; Str testFolder]) |> ignore
    eval globals (toCons [Sym "load"; Str "README.shen"]) |> ignore
    eval globals (toCons [Sym "load"; Str "tests.shen"]) |> ignore

[<EntryPoint>]
let main args =
    let thread = new Thread(runTestSuite, stackSize)
    thread.Start()
    thread.Join()
    0
