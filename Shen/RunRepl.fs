open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Startup
open Kl.Load.Reader
open System
open System.Diagnostics
open System.IO

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
    let rec valToStr expr =
        match expr with
        | Empty -> "Empty"
        | Int x -> sprintf "Int %A" x
        | Num x -> sprintf "Dec %Am" x
        | Sym s -> sprintf "Sym \"%s\"" s
        | Str s -> sprintf "Str \"%s\"" s
        | Cons(x, y) -> sprintf "Cons(%s, %s)" (valToStr x) (valToStr y)
        | Vec xs -> sprintf "Vec [|%s|]" (String.Join("; ", Array.map valToStr xs))
        | Err s -> sprintf "Err \"%s\"" s
        | Func _ -> string expr
        | Pipe io -> string expr
    for kv in globals.Symbols do
        printfn "globals.Symbols.[\"%s\"] <- %O" kv.Key (valToStr kv.Value)
    for kv in globals.Functions do
        match kv.Value with
        | Defun(name, args, body) ->
            printfn "globals.Functions.[\"%s\"] <- Defun(\"%s\", [%s], %O)"
                kv.Key
                name
                (String.Join(", ", List.map (fun x -> "\"" + x + "\"") args))
                (valToStr body)
        | _ -> ()
    eval globals (Cons(Sym "shen.shen", Empty)) |> ignore
    0

[<EntryPoint>]
let main args =
    let thread = new System.Threading.Thread((fun () -> main0 args |> ignore), 16777216)
    thread.Start()
    thread.Join()
    0
