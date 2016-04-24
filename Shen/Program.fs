open Kl
open Kl.Tokenizer
open Kl.Parser
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
                    "t-star.kl" // TODO contrary to spec, this gets loaded before types.kl?
                                // it contains (defun shen.typecheck ...) which types.kl uses
                                // Double check this now that irresolvable symbols are Errors instead of failures
                    "types.kl"
                ]
    let klFolder = @"..\..\..\KLambda"
    let rec astToStr = function
        | ComboToken tokens -> sprintf "(%s)" <| String.concat " " (Seq.map astToStr tokens)
        | BoolToken b -> if b then "true" else "false"
        | NumberToken n -> n.ToString()
        | StringToken s -> "\"" + s + "\""
        | SymbolToken s -> s
    let env = KlBuiltins.baseEnv ()
    for file in (List.map (fun f -> Path.Combine(klFolder, f)) files) do
        printfn ""
        printfn "Loading %s" file
        printfn ""
        stdout.Flush()
        let text = System.IO.File.ReadAllText(file)
        for ast in tokenizeAll text do
            match ast with
            | ComboToken (command :: symbol :: _) ->
                printfn "%s %s" (astToStr command) (astToStr symbol)
                let expr = parse Head ast
                eval env expr |> ignore
            | _ -> () // ignore copyright block at top
    printfn ""
    printfn "Loading done"
    printfn "Time: %s" <| stopwatch.Elapsed.ToString()
    printfn ""
    let load path = eval env (AppExpr (Head, (SymbolExpr "load"), [StringExpr path])) |> ignore
//    let runIt = KlTokenizer.tokenize >> KlParser.parse Head >> KlEvaluator.eval env >> ignore
//    printfn "Starting shen repl..."
//    printfn ""
//    while true do
//        printf "> "
//        let line = System.Console.ReadLine()
//        match line |> KlTokenizer.tokenize |> KlParser.parse Head |> KlEvaluator.eval env with
//        | ValueResult v -> printfn "%s" (KlBuiltins.klStr v)
//        | ErrorResult e -> printfn "ERROR %s" e
//    KlEvaluator.eval env (AppExpr (Head, SymbolExpr "shen.shen", [])) |> ignore
    Environment.CurrentDirectory <- Path.Combine(Environment.CurrentDirectory, "..\\..\\..\\Tests")
    load <| "README.shen"
    load <| "tests.shen"
//    env.SymbolDefinitions.["logging"] <- IntValue 1
//    load <| Path.Combine(testDir, "debug.shen")
    printfn ""
    printfn "Press any key to exit..."
    System.Console.ReadKey() |> ignore
    0

[<EntryPoint>]
let main args =
    let thread = new System.Threading.Thread((fun () -> main0 args |> ignore), 16777216)
    thread.Start()
    thread.Join()
    0
