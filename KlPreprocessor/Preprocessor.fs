open Kl
open Kl.Reader
open Kl.Evaluator
open System
open System.IO

let rec genExpr = string

let rec genValue v =
    match v with
    | Empty -> "()"
    | Bool true -> "true"
    | Bool false -> "false"
    | Int i -> i.ToString()
    | Dec d -> d.ToString()
    | Str s -> sprintf "\"%s\"" s
    | Sym s -> s
    | Cons(h, t) -> sprintf "(cons %s %s)" (genValue h) (genValue t)
    | Err m -> sprintf "(trap-error (simple-error \"%s\") (lambda E E))" m
    | Func f -> genFunc f
    | Vec v ->
        if v.Length = 0
            then "(vector-builder)"
            else sprintf "(vector-builder %s)" (String.Join(" ", Array.map genValue v))
    | InStream _ -> failwith "can't serialize streams"
    | OutStream _ -> failwith "can't serialize streams"
and genClosure (locals: Locals) =
    if locals.IsEmpty
        then "(vector-builder)"
        else sprintf "(vector-builder %s)" (String.Join(" ", List.map (fun (x, y) -> sprintf "(intern \"%s\") %s" x (genValue y)) (Map.toList locals)))
and genFunc f =
    match f with
    | Defun(name, paramz, body) -> sprintf "(defun %s (%s) %s)" name (String.Join(" ", paramz)) (genExpr body)
    | Native(name, _, _) -> name
    | Partial(f, args) -> sprintf "(%s %s)" (genValue (Func f)) (String.Join(" ", List.map genValue args))
    | Lambda(param, locals, body) -> sprintf "(lambda-closure %s (lambda %s %s))" (genClosure locals) param (genExpr body)
    | Freeze(locals, body) -> sprintf "(freeze-closure %s (freeze %s))" (genClosure locals) (genExpr body)

let genFuncs funcs =
    String.Join("\r\n", Seq.map genFunc funcs)

let genSymbol (k, v) = sprintf "(set %s %s)" k (genValue v)

let genSymbols syms =
    System.String.Join("\r\n", Seq.map (fun (KeyValue(k, v)) -> genSymbol (k, v)) syms)

let gen env =
    let b = Startup.baseEnv()
    sprintf "%s\r\n%s"
        (genFuncs (Seq.map (fun (KeyValue(_, v)) -> v) (Seq.filter (fun (KeyValue(k, _)) -> b.Globals.Functions.ContainsKey(k) |> not) env.Globals.Functions)))
        (genSymbols (Seq.filter (fun (KeyValue(k, _)) -> b.Globals.Symbols.ContainsKey(k) |> not) env.Globals.Symbols))

[<EntryPoint>]
let main args =
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
    let env = Startup.baseEnv()
    let overrides =
        Map.ofList [
            "symbol?", (1, Builtins.klIsSymbol)
            "shen.fillvector", (4, Builtins.klFillVector)
            "element?", (2, Builtins.klElement)
            "reverse", (1, Builtins.klReverse)
        ]
    for file in (List.map (fun f -> Path.Combine(klFolder, f)) files) do
        printfn ""
        printfn "Loading %s" file
        printfn ""
        stdout.Flush()
        let text = System.IO.File.ReadAllText(file)
        for ast in readAll text do
            match ast with
            | Cons(command, Cons(symbol, _)) ->
                printfn "%A %A" command symbol
                rootEval env.Globals ast |> ignore
            | _ -> () // ignore copyright block at top
    printfn ""
    printfn "Loading done"
    printfn "Time: %A" <| stopwatch.Elapsed
    printfn ""

    let preprocessedFolder = Path.Combine(klFolder, "preprocessed")
    let preprocessedPath = Path.Combine(preprocessedFolder, "preprocessed.kl")
    if not(Directory.Exists preprocessedFolder) then
        Directory.CreateDirectory preprocessedFolder |> ignore
    File.WriteAllText(preprocessedPath, gen env)
    0
