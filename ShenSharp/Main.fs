open Kl

module Shen =
    [<EntryPoint>]
    let main args =
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
            ]
        let klFolder = @"..\..\..\KLambda"
        let env = KlBuiltins.baseEnv ()
        let run = KlTokenizer.tokenize >> KlParser.parse Head >> KlEvaluator.eval env
        run "(defun shen.demod (X) X)" |> ignore
        for file in (List.map (fun f -> System.IO.Path.Combine(klFolder, f)) files) do
            stdout.Flush()
            let text = System.IO.File.ReadAllText(file)
            for ast in KlTokenizer.tokenizeAll text do
                let expr = KlParser.parse Head ast
                KlEvaluator.eval env expr |> ignore
        printf ">>> "
        match run "(str (lineread (value *stinput*)))" with
        | ValueResult (StringValue s) -> printfn "%s" s
        | ValueResult v -> printfn "%s" (v.ToString())
        | ErrorResult e -> printfn "error %s" e
        | ThunkResult t -> printfn "THUNK"
        printf "press any key to exit..."
        System.Console.ReadKey() |> ignore
        System.Console.ReadKey() |> ignore
        0

//module Shen =
//    [<EntryPoint>]
//    let main args =
//        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
//        let files = [
//                        "toplevel.kl"
//                        "core.kl"
//                        "sys.kl"
//                        "sequent.kl"
//                        "yacc.kl"
//                        "reader.kl"
//                        "prolog.kl"
//                        "track.kl"
//                        "load.kl"
//                        "writer.kl"
//                        "macros.kl"
//                        "declarations.kl"
//                        "t-star.kl" // TODO contrary to spec, this gets loaded before types.kl?
//                                    // it contains (defun shen.typecheck ...) which types.kl uses
//                        "types.kl"
//                    ]
//        let klFolder = @"..\..\..\KLambda"
//        let rec astToStr = function
//            | ComboToken tokens -> sprintf "(%s)" <| String.concat " " (Seq.map astToStr tokens)
//            | BoolToken b -> if b then "true" else "false"
//            | NumberToken n -> n.ToString()
//            | StringToken s -> "\"" + s + "\""
//            | SymbolToken s -> s
//        let env = KlBuiltins.baseEnv ()
//        "(defun shen.demod (X) X)" |> KlTokenizer.tokenize |> KlParser.parse Head |> KlEvaluator.eval env |> ignore
//        for file in (List.map (fun f -> System.IO.Path.Combine(klFolder, f)) files) do
//            printfn "Loading %s" file
//            printfn ""
//            stdout.Flush()
//            let text = System.IO.File.ReadAllText(file)
//            for ast in KlTokenizer.tokenizeAll text do
//                match ast with
//                | ComboToken (command :: symbol :: _) ->
//                    //printfn "%s %s" (astToStr command) (astToStr symbol)
//                    //printfn ""
//                    stdout.Flush()
//                    let expr = KlParser.parse Head ast
//                    KlEvaluator.eval env expr |> ignore
//                | _ -> () // ignore copyright block at top
//        printfn "Loading done"
//        printfn "Time: %s" <| stopwatch.Elapsed.ToString()
//        printfn ""
//        printfn "Starting shen repl..."
//        printfn ""
//        KlEvaluator.logging = true |> ignore
//        "(shen.shen)" |> KlTokenizer.tokenize
//                      |> KlParser.parse Head
//                      |> KlEvaluator.eval env
//                      |> ignore
//        printfn ""
//        printfn "Press any key to exit..."
//        System.Console.ReadKey() |> ignore
//        0
