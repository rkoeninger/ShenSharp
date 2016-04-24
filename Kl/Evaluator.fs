namespace Kl

open Extensions
open FSharpx.Option
open FSharpx.Choice

module Evaluator =

    type private FunctionResolveResult =
        | FunctionResult of Function
        | FunctionResolveError of string

    let vBool x =
        match x with
        | BoolValue b -> b
        | _ -> failwith "Boolean value expected"
    let trueR = BoolValue true |> Ok
    let falseR = BoolValue false |> Ok
    let trueW = Done trueR
    let falseW = Done falseR
    let thunkW f = new Thunk(f) |> Pending
    let funcW name arity locals f = new Function(name, arity, locals, f) |> FunctionValue |> Ok |> Done
    let append env defs = {env with Locals = List.Cons(Map.ofList defs, env.Locals)}
    let append1 env k v = append env [(k, v)]
    let closure eval env (paramz: string list) body =
        new Function("Anonymous", paramz.Length, env.Locals, fun _ args -> eval (append env (List.zip paramz args)) body) |> FunctionValue
    let private vFunc (env: Env) = function
        | FunctionValue f -> FunctionResult f
        | SymbolValue s ->
            match env.Globals.Functions.GetMaybe(s) with
            | Some (FunctionValue f) -> FunctionResult f
            | Some _ -> sprintf "Symbol \"%s\" does not represent function" s |> FunctionResolveError
            | None -> sprintf "Symbol \"%s\" is undefined" s |> FunctionResolveError
        | _ -> sprintf "Function value or symbol expected" |> FunctionResolveError
    let rec go = function
        | Pending thunk -> thunk.Run() |> go
        | Done result -> result
    let rec private apply pos globals fr (args: Value list) =
        match fr with
        | FunctionResolveError e -> Err e |> Done
        | FunctionResult f ->
            match args.Length, f.Arity with
            | Greater -> "Too many arguments" |> Err |> Done
            | Lesser -> funcW ("Partial " + f.Name)
                              (f.Arity - args.Length)
                              f.Locals
                              (fun globals moreArgs -> apply pos globals fr (List.append args moreArgs))
            | Equal -> match pos with
                       | Head -> f.Apply(globals, args)
                       | Tail -> thunkW (fun () -> f.Apply(globals, args))
    let (>>=) result f =
        match result with
        | Ok value -> f value
        | Err _ as error -> Done error
    let (>>>=) result f =
        match result with
        | Ok value -> f value |> Done
        | Err _ as error -> error |> Done
    let resolve locals symbolName =
        Seq.map (Map.tryFind symbolName) locals
        |> Seq.tryFind Option.isSome
        |> concat
        |> getOrElse (SymbolValue symbolName)
    let rec evalArgs evalE vals args =
        match args with
        | [] -> Choice1Of2 vals
        | arg :: args ->
            match evalE arg |> go with
            | Ok v -> evalArgs evalE (List.append vals [v]) args
            | Err e -> Choice2Of2 e

    let private vbranch ifTrue ifFalse value = if vBool value then ifTrue() else ifFalse()

    let rec eval env expr = evalw env expr |> go

    and evalw env expr =
        let evale = eval env
        let evalwe = evalw env

        match expr with
        | EmptyExpr     -> EmptyValue           |> Ok |> Done
        | BoolExpr b    -> BoolValue b          |> Ok |> Done
        | IntExpr n     -> IntValue n           |> Ok |> Done
        | DecimalExpr n -> DecimalValue n       |> Ok |> Done
        | StringExpr s  -> StringValue s        |> Ok |> Done
        | SymbolExpr s  -> resolve env.Locals s |> Ok |> Done

        | AndExpr(left, right) ->
            evale left >>= vbranch (fun () -> evalwe right) (fun () -> falseW)

        | OrExpr(left, right) ->
            evale left >>= vbranch (fun () -> trueW) (fun () -> evalwe right)

        | IfExpr (condition, consequent, alternative) ->
            evale condition >>= vbranch (fun () -> evalwe consequent) (fun () -> evalwe alternative)

        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> failwith "No condition was true"
                | (condition, consequent) :: rest ->
                    evale condition >>= vbranch (fun () -> evalwe consequent) (fun () -> evalClauses rest)
            evalClauses clauses

        | LetExpr (symbol, binding, body) ->
            let evalBody v = eval (append1 env symbol v) body
            eval env binding >>>= evalBody

        | LambdaExpr (param, body) ->
            closure evalw env [param] body |> Ok |> Done

        | FreezeExpr expr ->
            closure evalw env [] expr |> Ok |> Done

        | TrapExpr (pos, body, handler) ->
            match eval env body with
            | Err e -> eval env handler >>= (fun v -> apply pos env.Globals (vFunc env v) [ErrorValue e])
            | r -> Done r

        | AppExpr (pos, f, args) ->
            //List.fold (fun argvs arge -> ()) () args |> ignore
            eval env f >>= (fun v -> choice (apply pos env.Globals (vFunc env v))
                                            (Err >> Done)
                                            (evalArgs (evalw env) [] args))
    let rootEval globals expr =
        let env = {Globals = globals; Locals = []}

        match expr with
        | DefunExpr(name, paramz, body) ->
            let f = closure evalw env paramz body
            globals.Functions.[name] <- f
            Ok f

        | OtherExpr expr -> eval env expr
