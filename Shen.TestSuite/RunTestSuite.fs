module Shen.TestSuite

open System
open Kl
open Kl.Values
open Kl.Evaluator
open Shen.Runtime
open ShenSharp.Shared

let testFolder = combine [".."; ".."; ".."; "packages"; "ShenOSKernel-20.0"; "tests"]

let runTestSuite () =
    let globals = newRuntime()
    define globals "y-or-n?" (Compiled(0, fun _ _ -> Environment.Exit 1; Empty))
    eval globals (toCons [Sym "cd"; Str testFolder]) |> ignore
    eval globals (toCons [Sym "load"; Str "README.shen"]) |> ignore
    eval globals (toCons [Sym "load"; Str "tests.shen"]) |> ignore

[<EntryPoint>]
let main _ = separateThread16MB runTestSuite
