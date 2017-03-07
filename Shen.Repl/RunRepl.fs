module Shen.Repl

open System
open System.IO
open Kl
open Kl.Values
open Kl.Evaluator
open Shen.Runtime
open ShenSharp.Shared

let runRepl files () =
    let globals = newRuntime()
    try
        match files with
        | ("--help" | "-h") :: _ ->
            let shenVersion =
                match retrieve globals "*version*" with
                | Str s -> s
                | _ -> "Unknown"
            printfn "%s" shenVersion
            printfn "ShenSharp %s" Revision
            printfn ""
            printfn "Usage: shen [MODE] [FILES]..."
            printfn "  -h, --help                  : Shows this help"
            printfn "  -e, --eval <expr>           : Evaluates expr and prints result"
            printfn ""
            printfn "Loads [FILES] in order"
            printfn "Starts the REPL if no mode/files specified"
        | ("--eval" | "-e") :: rest ->
            printfn "%O" (evalSyntax globals (String.Join(" ", rest)))
        | [] ->
            eval globals (toCons [Sym "shen"]) |> ignore
        | _ ->
            for file in files do
                eval globals (toCons [Sym "load"; Str file]) |> ignore
    with
        e -> printfn "Unhandled error: %s" e.Message

[<EntryPoint>]
let main args = separateThread16MB(runRepl(Array.toList args))
