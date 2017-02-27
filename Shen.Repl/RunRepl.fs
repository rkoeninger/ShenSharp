module Shen.Repl

open Kl
open Kl.Values
open Kl.Evaluator
open Shen.Runtime

let runRepl files () =
    let globals = newRuntime()
    match files with
    | [||] ->
        eval globals (toCons [Sym "shen"]) |> ignore
    | _ ->
        for file in files do
            eval globals (toCons [Sym "load"; Str file]) |> ignore

[<EntryPoint>]
let main args = separateThread (runRepl args)
