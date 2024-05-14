module Shen.TestSuite

open System
open System.Diagnostics
open Kl
open Kl.Values
open Shen.Runtime
open ShenSharp.Shared

let testFolder = fromRoot ["kernel"; "tests"]

let runTestSuite () =
    let globals = newRuntime ()
    define globals "y-or-n?" (Compiled(1, fun _ _ -> True))
    changeDirectory globals testFolder
    let stopwatch = Stopwatch.StartNew()
    load globals "runme.shen"
    printfn ""
    printfn "%O" stopwatch.Elapsed
    printfn ""

[<EntryPoint>]
let main _ = separateThread16MB runTestSuite
