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
    let trueR = BoolValue true |> ValueResult
    let falseR = BoolValue false |> ValueResult
    let trueW = Completed trueR
    let falseW = Completed falseR
    let thunkW f = new Thunk(f) |> Pending
    let funcW name arity locals f = new Function(name, arity, locals, f) |> FunctionValue |> ValueResult |> Completed
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
        | Completed result -> result
    let rec private apply pos globals fr (args: Value list) =
        match fr with
        | FunctionResolveError e -> ErrorResult e |> Completed
        | FunctionResult f ->
            match args.Length, f.Arity with
            | Greater -> "Too many arguments" |> ErrorResult |> Completed
            | Lesser -> funcW ("Partial " + f.Name)
                              (f.Arity - args.Length)
                              f.Locals
                              (fun globals moreArgs -> apply pos globals fr (List.append args moreArgs))
            | Equal -> match pos with
                       | Head -> f.Apply(globals, args)
                       | Tail -> thunkW (fun () -> f.Apply(globals, args))
    let (>>=) result f =
        match result with
        | ValueResult value -> f value
        | ErrorResult _ as error -> Completed error
    let (>>>=) result f =
        match result with
        | ValueResult value -> f value |> Completed
        | ErrorResult _ as error -> error |> Completed
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
            | ValueResult v -> evalArgs evalE (List.append vals [v]) args
            | ErrorResult e -> Choice2Of2 e
    let rec eval env expr = evalw env expr |> go
    and evalw env = function
        | EmptyExpr     -> EmptyValue           |> ValueResult |> Completed
        | BoolExpr b    -> BoolValue b          |> ValueResult |> Completed
        | IntExpr n     -> IntValue n           |> ValueResult |> Completed
        | DecimalExpr n -> DecimalValue n       |> ValueResult |> Completed
        | StringExpr s  -> StringValue s        |> ValueResult |> Completed
        | SymbolExpr s  -> resolve env.Locals s |> ValueResult |> Completed

        | AndExpr(left, right) ->
            let evalRight b =
                match b with
                | BoolValue true -> evalw env right
                | _ -> falseW
            eval env left >>= evalRight

        | OrExpr(left, right) ->
            let evalRight b =
                match b with
                | BoolValue true -> trueW
                | _ -> evalw env right
            eval env left >>= evalRight

        | IfExpr (condition, consequent, alternative) ->
            let evalBranch b =
                match b with
                | BoolValue true -> evalw env consequent
                | _ -> evalw env alternative
            eval env condition >>= evalBranch

        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> failwith "No condition was true"
                | (condition, consequent) :: rest ->
                    match eval env condition with
                    | ValueResult(BoolValue true) -> evalw env consequent
                    | ValueResult _ -> evalClauses rest
                    | ErrorResult _ as e -> Completed e
            evalClauses clauses

        | LetExpr (symbol, binding, body) ->
            let evalBody v = eval (append1 env symbol v) body
            eval env binding >>>= evalBody

        | LambdaExpr (param, body) ->
            closure evalw env [param] body |> ValueResult |> Completed

        | DefunExpr (name, paramz, body) ->
            let f = closure evalw env paramz body
            env.Globals.Functions.[name] <- f
            f |> ValueResult |> Completed

        | FreezeExpr expr ->
            closure evalw env [] expr |> ValueResult |> Completed

        | TrapExpr (pos, body, handler) ->
            match eval env body with
            | ErrorResult e -> eval env handler >>= (fun v -> apply pos env.Globals (vFunc env v) [ErrorValue e])
            | r -> Completed r

        | AppExpr (pos, f, args) ->
            eval env f >>= (fun v -> choice (apply pos env.Globals (vFunc env v))
                                            (ErrorResult >> Completed)
                                            (evalArgs (evalw env) [] args))
