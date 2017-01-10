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
        | None -> err("Symbol not defined: " + id)

    // Symbols in operator position are either:
    //   * A local variable whose value is a function.
    //   * A local variable whose value is a symbol that resolves to a global function.
    //   * Resolves to a global function.
    // Symbols in operator position are never idle.
    let private resolveFunction env id =
        match Map.tryFind id env.Locals with
        | Some(Func f) -> f
        | Some(Sym id) -> resolveGlobalFunction env id
        | Some _ -> err("Local symbol does not represent a function: " + id)
        | _ -> resolveGlobalFunction env id

    /// <summary>
    /// Applies a function to a set of arguments in a given global
    /// environment, conditionally returning a partial or pending Work.
    /// </summary>
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
        match expr with

        // Atomic values besides symbols are self-evaluating
        | (Empty | Bool _ | Int _ | Dec _ | Str _) as x -> Done x

        | Sym "true" -> truew
        | Sym "false" -> falsew

        // Should only get here in the case of symbols not in operator position
        // In this case, symbols always evaluate without error.
        | Sym s -> Done(resolveSymbol env s)

        // When the first expression evaluates to false,
        // false is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | AndExpr(left, right) ->
            Done(Bool(vbool(eval env left) && vbool(eval env right)))

        // When the first expression evaluates to true,
        // true is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | OrExpr(left, right) ->
            Done(Bool(vbool(eval env left) || vbool(eval env right)))

        // If expressions selectively evaluate depending on the result
        // of evaluating the condition expression
        // The condition must evaluate to a boolean value
        | IfExpr(condition, consequent, alternative) ->
            if vbool(eval env condition)
                then evalw env consequent
                else evalw env alternative

        // Condition expressions must evaluate to boolean values
        // Evaluation of clauses stops when one of their conditions
        // evaluates to true.
        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> err "No condition was true"
                | (condition, consequent) :: rest ->
                    if vbool(eval env condition)
                        then evalw env consequent
                        else evalClauses rest
            evalClauses clauses

        // Let expressions evaluate the symbol binding first and then evaluate the body
        // with the result of evaluating the binding bound to the symbol
        | LetExpr(symbol, binding, body) ->
            let value = eval env binding
            evalw (appendLocals env [symbol, value]) body

        // Evaluating a lambda captures the local state,
        // the lambda parameter name and the body expression
        | LambdaExpr(param, body) ->
            Done(Func(Lambda(param, env.Locals, body)))

        // Evaluating a freeze just captures the local state and the body expression
        | FreezeExpr body ->
            Done(Func(Freeze(env.Locals, body)))

        // Handler expression is not evaluated unless body results in an error
        // Handler expression must evaluate to a function
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

        // Expression in operator position must eval to a function
        // or to a symbol which resolves to a function.
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
    and evalFunction env expr =
        match expr with
        | Sym s -> resolveFunction env s
        | _ ->
            match eval env expr with
            | Func f -> f
            | Sym s -> resolveFunction env s
            | _ -> err "Expression must resolve to a function"

    /// <summary>
    /// Evaluates an sub-expression into a value, running all deferred
    /// computations in the process.
    /// </summary>
    and eval env expr = go(evalw env expr)

    /// <summary>
    /// Evaluates a root-level expression into a value, running all side
    /// effects in the process. Starts with a new, empty local scope.
    /// </summary>
    let rootEval globals = eval (newEnv globals Map.empty)

    /// <summary>
    /// Applies function to argument list in given global scope.
    /// </summary>
    let apply globals f args = go(applyw globals f args)
