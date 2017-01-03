open Kl
open Kl.Tokenizer
open Kl.Parser
open Kl.Evaluator
open System
open System.IO

let klVectorBuilder _ (args: Value list) =
    let array = Array.create<Value>(args.Length) (Sym "shen.fail!")
    List.iteri (fun i v -> array.[i] = v |> ignore) args
    Vec array

let buildClosure array =
    let chunks = Array.chunkBySize 2 array
    let define locals pair =
        match pair with
        | [|Sym name; value|] -> Map.add name value locals
        | [|_; _|] -> Values.err "First item in each pair should be a symbol"
        | _ -> failwith "shouldn't happen"
    Array.fold define Map.empty chunks

let klLambdaClosure _ args =
    match args with
    | [Vec array; Func(Lambda(param, _, body))] -> Func(Lambda(param, buildClosure array, body))
    | _ -> failwith "lambda-closure: wrong number or type of args"

let klFreezeClosure _ args =
    match args with
    | [Vec array; Func(Freeze(_, body))] -> Func(Freeze(buildClosure array, body))
    | _ -> failwith "freeze-closure: wrong number or type of args"

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
    let klFolder = @"..\..\..\KLambda"
    let rec astToStr = function
        | ComboToken tokens -> sprintf "(%s)" <| String.concat " " (Seq.map astToStr tokens)
        | BoolToken b -> if b then "true" else "false"
        | IntToken i -> i.ToString()
        | DecToken d -> d.ToString()
        | StrToken s -> "\"" + s + "\""
        | SymToken s -> s
    let env = Startup.baseEnv()
    //env.Globals.Functions.["vector-builder"] <- Native("vector-builder", -1, klVectorBuilder)
    //env.Globals.Functions.["lambda-closure"] <- Native("lambda-closure", 2, klLambdaClosure)
    //env.Globals.Functions.["freeze-closure"] <- Native("freeze-closure", 2, klFreezeClosure)
//    let overrides =
//        Map.ofList [
//            "symbol?", (1, Builtins.klIsSymbol)
//            "shen.fillvector", (4, Builtins.klFillVector)
//            "element?", (2, Builtins.klElement)
//            "map", (2, Builtins.klMap)
//            "reverse", (1, Builtins.klReverse)
//        ]
    //let defunList = new System.Collections.Generic.List<string>()
    env.Globals.Symbols.["shen-*installing-kl*"] <- Bool true
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
                let expr = rootParse ast
                //match expr with
                //| DefunExpr(name, _, _) -> defunList.Add(name)
                //| _ -> ()
                rootEval env.Globals expr |> ignore
                if (command = SymToken "set") && (symbol = SymToken "shen.*symbol-table*") then
                    ()
            | _ -> () // ignore copyright block at top
    env.Globals.Symbols.["shen-*installing-kl*"] <- Bool false
    env.Globals.Symbols.["*home-directory*"] <- Str(Environment.CurrentDirectory.Replace('\\', '/'))
    printfn ""
    printfn "Loading done"
    printfn "Time: %s" <| stopwatch.Elapsed.ToString()
    printfn ""
    
    env.Globals.Functions.["symbol?"] <- Native("symbol?", 1, Builtins.klIsSymbol)
    env.Globals.Functions.["boolean?"] <- Native("boolean?", 1, Builtins.klIsBoolean)
    //env.Globals.Functions.["shen.fillvector"] <- Native("shen.fillvector", 4, Builtins.klFillVector)
    env.Globals.Functions.["shen.mod"] <- Native("shen.mod", 2, Builtins.klModulus)
    //env.Globals.Functions.["element?"] <- Native("element?", 2, Builtins.klElement)
    //env.Globals.Functions.["map"] <- Native("map", 2, Builtins.klMap)
    //env.Globals.Functions.["reverse"] <- Native("reverse", 1, Builtins.klReverse)
    //env.Globals.Functions.["shen.alpha?"] <- Native("shen.alpha?", 1, Builtins.klIsAlpha)
    //env.Globals.Functions.["shen.digit?"] <- Native("shen.digit?", 1, Builtins.klIsDigit)
    env.Globals.Functions.["append"] <- Native("append", 2, Builtins.klAppend)
    env.Globals.Functions.["cd"] <- Native("cd", 1, Builtins.klCd)
//    env.CallCounts
//    |> Seq.sortBy (fun (KeyValue(k, v)) -> v)
//    |> Seq.iter (fun (KeyValue(k,v)) -> printfn "%s: %d" k v)
    let load path = eval env (AppExpr (Head, (SymExpr "load"), [StrExpr path])) |> ignore
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
    load "README.shen"
    load "tests.shen"
    //load "einstein.shen"
    //eval env (AppExpr(Head, SymExpr "shen.shen", [])) |> ignore
//    env.SymbolDefinitions.["logging"] <- Int 1
//    load <| Path.Combine(testDir, "debug.shen")
//    rootEval env.Globals env.CallCounts (OtherExpr(AppExpr(Head, SymExpr "shen.shen", []))) |> ignore
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
