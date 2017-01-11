open Kl
open Kl.Reader
open Kl.Evaluator
open System
open System.IO

let main0 args =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
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
    let env = Startup.baseEnv()
    env.Globals.Symbols.["shen-*installing-kl*"] <- Bool true
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
                rootEval env.Globals ast |> ignore
                if (command = Sym "set") && (symbol = Sym "shen.*symbol-table*") then
                    ()
            | _ -> () // ignore copyright block at top
    env.Globals.Symbols.["shen-*installing-kl*"] <- Bool false
    env.Globals.Symbols.["*home-directory*"] <- Str(Environment.CurrentDirectory.Replace('\\', '/'))
    printfn ""
    printfn "Loading done"
    printfn "Time: %s" <| stopwatch.Elapsed.ToString()
    printfn ""
    eval env (Cons(Sym "shen.shen", Empty)) |> ignore
    0

[<EntryPoint>]
let main args =
    let thread = new System.Threading.Thread((fun () -> main0 args |> ignore), 16777216)
    thread.Start()
    thread.Join()
    0
