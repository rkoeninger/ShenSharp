namespace Kl

open Extensions
open Values
open ExpressionPatterns

module Evaluator =

    let private appendLocals env defs =
        let locals = List.fold (fun m (k, v) -> Map.add k v m) env.Locals defs
        {env with Locals = locals}

    // Symbols not in operator position are either defined locally or they are idle.
    let private resolveSymbol env id =
        match Map.tryFind id env.Locals with
        | Some value -> value
        | None -> Sym id

    let private resolveGlobalFunction env id =
        match env.Globals.Functions.GetMaybe id with
        | Some f -> f
        | None -> err("Function not defined: " + id)

    // Symbols in operator position are either:
    //   * A local variable whose value is a function.
    //   * A local variable whose value is a symbol that resolves to a global function.
    //   * Resolves to a global function.
    // Symbols in operator position are never idle.
    let private resolveFunction env id =
        match Map.tryFind id env.Locals with
        | Some(Func f) -> f
        | Some(Sym id) -> resolveGlobalFunction env id
        | Some _ -> err("Function not defined: " + id)
        | _ -> resolveGlobalFunction env id

    let rec private applyw globals f args =
        match f with

        // Freezes can only be applied to 0 arguments.
        // They evaluate their body with local state captured where they were formed.
        | Freeze(locals, body) ->
            match args with
            | [] -> evalw (newEnv globals locals) body
            | _ -> err(sprintf "Too many arguments (%i) provided to freeze" args.Length)

        // Each lambda only takes 1 argument.
        // Applying a lambda to 0 arguments returns the same lambda.
        // Applying a lambda to more than 1 argument will apply the remaining
        // arguments to the returned function.
        // Lambdas evaluate their body with local state captured when they were formed.
        | Lambda(param, locals, body) as lambda ->
            let env = newEnv globals locals
            match args with
            | [] -> err "Zero arguments provided to lambda"
            | [arg0] -> evalw (appendLocals env [param, arg0]) body
            | arg0 :: args1 ->
                match eval (appendLocals env [param, arg0]) body with
                | Func f -> applyw globals f args1
                | Sym s ->
                    let f = resolveFunction env s
                    applyw globals f args1
                | _ -> err(sprintf "Too many arguments (%i) provided to lambda" args.Length)

        // Defuns can be applied to anywhere between 0 and the their full parameter list.
        // An error is raised if a Defun is applied to more arguments than it takes.
        // If applied to fewer arguments than the full parameter list, a Partial is returned.
        // They do not retain local state and are usually evaluated at the root level.
        | Defun(name, paramz, body) as defun ->
            let env = newEnv globals Map.empty
            match args.Length, paramz.Length with
            | Lesser ->
                match args with
                | [] -> Done(Func defun)
                | _ -> Done(Func(Partial(defun, args)))
            | Equal ->
                let env = appendLocals env (List.zip paramz args)
                thunkw(fun () -> evalw env body)
            | Greater ->
                err(sprintf "Too many arguments (%i) provided to defun \"%s\"" args.Length name)

        // Natives have the same rules as Defuns.
        | Native(name, arity, f) as native ->
            match args.Length, arity with
            | Lesser ->
                match args with
                | [] -> Done(Func native)
                | _ -> Done(Func(Partial(native, args)))
            | Equal ->
                Done(f globals args)
            | Greater ->
                err(sprintf "Too many arguments (%i) provided to native \"%s\"" args.Length name)

        // Applying a partial applies the original function
        // to the previous and current argument lists appended.
        | Partial(f, previousArgs) as partial ->
            match args with
            | [] -> Done(Func(partial))
            | _ -> applyw globals f (List.append previousArgs args)

    and private evalw env expr =

        let vbool = function
            | Bool b -> b
            | _ -> err "Conditional must evaluate to boolean"

        match expr with

        // Atomic values besides Symbols are self-evaluating
        | (Empty | Bool _ | Int _ | Dec _ | Str _) -> Done expr

        // When Shen code is translated to KL, `true` and `false` come through as Symbols.
        | Sym "true" -> Done truev
        | Sym "false" -> Done falsev

        // Should only get here in the case of Symbols not in operator position.
        // In this case, Symbols always evaluate without error.
        | Sym s -> Done(resolveSymbol env s)

        // Short-circuit evaluation. Both left and right must eval to Bool.
        | AndExpr(left, right) ->
            Done(Bool(vbool(eval env left) && vbool(eval env right)))

        // Short-circuit evaluation. Both left and right must eval to Bool.
        | OrExpr(left, right) ->
            Done(Bool(vbool(eval env left) || vbool(eval env right)))

        // Condition must evaluate to Bool. Consequent and alternative are in tail position.
        | IfExpr(condition, consequent, alternative) ->
            if vbool(eval env condition)
                then evalw env consequent
                else evalw env alternative

        // Conditions must evaluate to Bool. Consequents are in tail position.
        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> err "No condition was true"
                | (condition, consequent) :: rest ->
                    if vbool(eval env condition)
                        then evalw env consequent
                        else evalClauses rest
            evalClauses clauses

        // Body expression is in tail position.
        | LetExpr(symbol, binding, body) ->
            let value = eval env binding
            evalw (appendLocals env [symbol, value]) body

        // Lambdas capture local scope.
        | LambdaExpr(param, body) ->
            Done(Func(Lambda(param, env.Locals, body)))

        // Freezes capture local scope.
        | FreezeExpr body ->
            Done(Func(Freeze(env.Locals, body)))

        // Handler expression only evaluated unless body results in an error.
        // Handler expression must evaluate to a Function.
        | TrapExpr(body, handler) ->
            try
                Done(eval env body)
            with
            | :? SimpleError as e ->
                let operator = evalFunction env handler
                applyw env.Globals operator [Err e.Message]
            | _ -> reraise()

        // Evaluate all expressions, returns result of last expression.
        // All but the last expression are evaluated in Head position.
        // Last expression is evaluated in Tail position.
        // Default result is Empty.
        | DoExpr exprs ->
            let rec doAll result exprs =
                match exprs with
                | [] -> result
                | expr :: rest ->
                    go result |> ignore
                    doAll (evalw env expr) rest
            doAll (Done Empty) exprs

        // Evaluating a defun just takes the name, param list and body
        // and stores them in the global function scope.
        | DefunExpr(name, paramz, body) ->
            let f =
                match Overrides.overrides.GetMaybe name with
                | Some f -> f
                | None -> Defun(name, paramz, body)
            env.Globals.Functions.[name] <- f
            Done(Sym name)

        // Expression in operator position must evaluate to a Function.
        | AppExpr(f, args) ->
            let operator = evalFunction env f
            let operands = List.map (eval env) args
            applyw env.Globals operator operands

        | _ -> err "Unexpected value type - cannot evaluate"

    // Does a full eval of expr, looking to get a Function.
    // 3 ways this can work:
    //   * expr can eval to function
    //   * expr can be a symbol that resolves to a function
    //   * expr can eval to a symbol that evals to a function
    and private evalFunction env expr =
        match expr with
        | Sym s -> resolveFunction env s
        | _ ->
            match eval env expr with
            | Func f -> f
            | Sym s -> resolveFunction env s
            | _ -> err "Expression must resolve to a function"

    /// <summary>
    /// Evaluates an expression into a value.
    /// </summary>
    and eval env expr = go(evalw env expr)

    /// <summary>
    /// Evaluates a root-level expression into a value, running all side
    /// effects in the process. Starts with a new, empty local scope.
    /// </summary>
    let rootEval globals = eval (newEnv globals Map.empty)
