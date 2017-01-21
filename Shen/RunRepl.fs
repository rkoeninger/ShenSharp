open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Startup
open Kl.Load.Reader
open Kl.Load.Translator
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq

let main0 args =
    let stopwatch = Stopwatch.StartNew()
    let files = [
                    "toplevel.kl"
                    "core.kl"
                    "sys.kl"
                    "sequent.kl"
                    "yacc.kl"
                    "reader.kl"
                    "prolog.kl"
                    "track.kl"
                    "load.kl"
                    "writer.kl"
                    "macros.kl"
                    "declarations.kl"
                    "types.kl"
                    "t-star.kl"
                ]
    let klFolder = @"..\..\..\Kl.Source"
    let globals = baseGlobals()
    globals.Symbols.["shen-*installing-kl*"] <- truev
    for file in (List.map (fun f -> Path.Combine(klFolder, f)) files) do
        printfn ""
        printfn "Loading %s" file
        printfn ""
        stdout.Flush()
        let text = File.ReadAllText(file)
        for ast in readAll text do
            match ast with
            | Cons(command, Cons(symbol, _)) ->
                printfn "%O %O" command symbol
                eval globals ast |> ignore
                if (command = Sym "set") && (symbol = Sym "shen.*symbol-table*") then
                    ()
            | _ -> () // ignore copyright block at top
    globals.Symbols.["shen-*installing-kl*"] <- falsev
    globals.Symbols.["*home-directory*"] <- Str(Environment.CurrentDirectory.Replace('\\', '/'))
    globals.Functions.["exit"] <- Native("exit", 1, Builtins.klExit)
    printfn ""
    printfn "Loading done"
    printfn "Time: %s" <| stopwatch.Elapsed.ToString()
    printfn ""
    let origGlobals = baseGlobals()
    let tup (kv: KeyValuePair<'a, 'b>) = (kv.Key, kv.Value)
    printfn "%s" (encodeSymbols (Seq.toList origGlobals.Symbols.Keys) (Seq.map tup globals.Symbols))
    printfn "%s" (encodeFunctions (Seq.toList origGlobals.Functions.Keys) (Seq.map tup globals.Functions))
    eval globals (Cons(Sym "shen.shen", Empty)) |> ignore
    0

[<EntryPoint>]
let main args =
    let thread = new System.Threading.Thread((fun () -> main0 args |> ignore), 16777216)
    thread.Start()
    thread.Join()
    0
