module Shen.Repl

open System.Threading
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Make.Loader

let stackSize = 16777216

let runRepl files =
    let globals = load()

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
