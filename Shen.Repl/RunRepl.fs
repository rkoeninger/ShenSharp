module Shen.Repl

open System
open Kl
open Kl.Values
open Kl.Evaluator
open Shen.Runtime

let runRepl files () =
    let globals = newRuntime()
    try
        match files with
        | [] ->
            eval globals (toCons [Sym "shen"]) |> ignore
        | "-e" :: rest ->
            printfn "%O" (Eval(globals, (String.Join(" ", rest))))
        | _ ->
            for file in files do
                eval globals (toCons [Sym "load"; Str file]) |> ignore
    with
        e -> printfn "Unhandled error: %s" e.Message

[<EntryPoint>]
let main args = separateThread (runRepl (Array.toList args))
