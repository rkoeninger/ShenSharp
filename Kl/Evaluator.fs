namespace Kl

open Extensions
open FSharpx.Option
open FSharpx.Choice

module Evaluator =

    let funcW name arity locals f = new Function(name, arity, locals, f) |> FunctionValue |> Ok |> Done
    let append env defs = {env with Locals = List.Cons(Map.ofList defs, env.Locals)}
    let append1 env k v = append env [(k, v)]
    let closure eval env (paramz: string list) body =
        new Function(
            "Anonymous",
            paramz.Length,
            env.Locals,
            fun _ args -> eval (append env (List.zip paramz args)) body)
    let rec go = function
        | Pending thunk -> thunk.Run() |> go
        | Done result -> result
    let (>>=) result f =
        match result with
        | Ok value -> f value
        | Err _ as error -> Done error
    let (>>>=) result f =
        match result with
        | Ok value -> f value |> Done
        | Err _ as error -> error |> Done

    let rec private apply pos globals (f: Function) (args: Value list) =
        match args.Length, f.Arity with
        | Greater -> "Too many arguments" |> Err |> Done
        | Lesser -> funcW ("Partial " + f.Name)
                            (f.Arity - args.Length)
                            f.Locals
                            (fun globals moreArgs -> apply pos globals f (List.append args moreArgs))
        | Equal -> match pos with
                    | Head -> f.Apply(globals, args)
                    | Tail -> Values.thunkw(fun () -> f.Apply(globals, args))

    let rec private resolveLocalSymbol locals id =
        match locals with
        | [] -> None
        | frame :: rest ->
            match Map.tryFind id frame with
            | Some(value) -> Some(value)
            | None -> resolveLocalSymbol rest id

    let private resolveSymbol env id =
        if Values.isVar id
            then getOrElse (SymbolValue id) (resolveLocalSymbol env.Locals id)
            else SymbolValue id

    let private resolveFunction env id =
        if Values.isVar id then
            match resolveLocalSymbol env.Locals id with
            | Some value ->
                match value with
                | FunctionValue f -> Ok f
                | _ -> Err "Symbol does not represent a function"
            | None -> Err "Symbol not defined"
        else
            match env.Globals.Functions.GetMaybe id with
            | Some f -> Ok f
            | None -> Err "Symbol not defined"

    let private vbranch ifTrue ifFalse value = if Values.vbool value then ifTrue() else ifFalse()

    let rec private evalw env expr =
        let evale = eval env
        let evalwe = evalw env

        match expr with
        | EmptyExpr     -> EmptyValue          |> Ok |> Done
        | BoolExpr b    -> BoolValue b         |> Ok |> Done
        | IntExpr n     -> IntValue n          |> Ok |> Done
        | DecimalExpr n -> DecimalValue n      |> Ok |> Done
        | StringExpr s  -> StringValue s       |> Ok |> Done
        | SymbolExpr s  -> resolveSymbol env s |> Ok |> Done

        | AndExpr(left, right) ->
            evale left >>= vbranch (fun () -> evalwe right) (fun () -> Values.falsew)

        | OrExpr(left, right) ->
            evale left >>= vbranch (fun () -> Values.truew) (fun () -> evalwe right)

        | IfExpr (condition, consequent, alternative) ->
            evale condition >>= vbranch (fun () -> evalwe consequent) (fun () -> evalwe alternative)

        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> failwith "No condition was true"
                | (condition, consequent) :: rest ->
                    evale condition >>= vbranch (fun () -> evalwe consequent) (fun () -> evalClauses rest)
            evalClauses clauses

        | LetExpr (symbol, binding, body) ->
            let evalBody v = evalw (append1 env symbol v) body
            eval env binding >>= evalBody

        | LambdaExpr (param, body) ->
            closure evalw env [param] body |> FunctionValue |> Ok |> Done

        | FreezeExpr expr ->
            closure evalw env [] expr |> FunctionValue |> Ok |> Done

        | TrapExpr (pos, body, handler) ->
            match evale body with
            | Err e ->
                match evale handler with
                | Ok(FunctionValue f) -> apply pos env.Globals f [ErrorValue e]
                | Ok(_) -> Err "Trap handler did not evaluate to a function" |> Done
                | Err message -> Err message |> Done
            | r -> Done r

        | AppExpr (pos, f, args) ->
            match f with
            | SymbolExpr s ->
                match resolveFunction env s with
                | Ok f ->
                    let rec evalArgs args =
                        match args with
                        | [] -> Ok []
                        | arg :: rest ->
                            match evale arg with
                            | Ok value ->
                                match evalArgs rest with
                                | Ok values -> Ok (value :: values)
                                | Err _ as error -> error
                            | Err message -> Err message

                    match evalArgs args with
                    | Ok argsv -> apply pos env.Globals f argsv
                    | Err message -> Err message |> Done
                | Err message -> Err message |> Done
            | _ -> failwith "Application must begin with a symbol"
            
    and eval env expr = evalw env expr |> go

    let rootEval globals expr =
        let env = {Globals = globals; Locals = []}

        match expr with
        | DefunExpr(name, paramz, body) ->
            let f = closure evalw env paramz body
            globals.Functions.[name] <- f
            FunctionValue f |> Ok

        | OtherExpr expr -> eval env expr
