module Shen.Repl

open System
open Kl
open Kl.Values
open Kl.Evaluator
open Shen.Runtime
open ShenSharp.Shared

let private printVersion globals =
    let shenVersion =
        match retrieve globals "*version*" with
        | Str s -> s
        | _ -> "Unknown"
    printfn "%s" shenVersion
    printfn "ShenSharp %s" Revision

let rec private evalOptions globals = function
    | [] -> true
    | ("-v" | "--version") :: _ ->
        printVersion globals
        Environment.Exit 0
        false
    | ("-h" | "--help") :: _ ->
        printVersion globals
        printfn ""
        printfn "Usage: shen [OPTIONS...]"
        printfn "  -v, --version       : Prints Shen, ShenSharp version numbers and exits"
        printfn "  -h, --help          : Shows this help and exits"
        printfn "  -e, --eval <expr>   : Evaluates expr and prints result"
        printfn "  -l, --load <file>   : Reads and evaluates file"
        printfn ""
        printfn "Evaluates options in order"
        printfn "Starts the REPL if no eval/load options specified"
        Environment.Exit 0
        false
    | ("-e" | "--eval") :: expr :: rest ->
        printfn "%O" (evalSyntax globals expr)
        evalOptions globals rest |> ignore
        false
    | ("-l" | "--load") :: file :: rest ->
        load globals file
        evalOptions globals rest |> ignore
        false
    | _ :: rest ->
        evalOptions globals rest

let private runRepl args () =
    let globals = newRuntime()
    assign globals "*argv*" (toCons (List.map Str args))
    try
        if evalOptions globals args then
            eval globals (toCons [Sym "shen.shen"]) |> ignore
    with
        e -> printfn "Unhandled error: %s" e.Message

[<EntryPoint>]
let main args = moreRam(runRepl(Array.toList args))
