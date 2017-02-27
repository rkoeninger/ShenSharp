module Shen.Repl

open System
open System.IO
open Kl
open Kl.Values
open Kl.Evaluator
open Shen.Runtime

let runRepl files () =
    let globals = newRuntime()
    try
        match files with
        | ["-h"] ->
            printfn "shen            : Starts REPL"
            printfn "shen -e <expr>  : Evaluates expr and prints result"
            printfn "shen <file>*    : Loads files in given order"
        | "-e" :: rest ->
            printfn "%O" (Eval(globals, String.Join(" ", rest)))
        | [] ->
            eval globals (toCons [Sym "shen"]) |> ignore
        | _ ->
            for file in files do
                eval globals (toCons [Sym "load"; Str file]) |> ignore
    with
        e -> printfn "Unhandled error: %s" e.Message

[<EntryPoint>]
let main args = separateThread (runRepl (Array.toList args))
