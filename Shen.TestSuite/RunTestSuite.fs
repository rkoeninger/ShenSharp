module Shen.TestSuite

open System
open System.IO
open System.Threading
open Kl
open Kl.Values
open Kl.Evaluator
open Shen.Runtime

let rec combine = function
    | [] -> "."
    | [x] -> x
    | x :: xs -> Path.Combine(x, combine xs)
let stackSize = 16777216
let testFolder = combine [".."; ".."; ".."; "Distribution"; "Tests"]

let runTestSuite () =
    let globals = newRuntime()
    define globals "y-or-n?" (Compiled(0, fun _ _ -> Environment.Exit 1; Empty))
    eval globals (toCons [Sym "cd"; Str testFolder]) |> ignore
    eval globals (toCons [Sym "load"; Str "README.shen"]) |> ignore
    eval globals (toCons [Sym "load"; Str "tests.shen"]) |> ignore

[<EntryPoint>]
let main args =
    let thread = new Thread(runTestSuite, stackSize)
    thread.Start()
    thread.Join()
    0
