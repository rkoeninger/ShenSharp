namespace Kl

open Extensions
open Values

module Evaluator =

    let private appendLocals env defs =
        let locals = List.fold (fun m (k, v) -> Map.add k v m) env.Locals defs
        {env with Locals = locals}

    // For symbols not in operator position:
    // Those starting with upper-case letter are idle if not defined.
    // Those not starting with upper-case letter are always idle.
    let private resolveSymbol env id =
        if isVar id then
            match Map.tryFind id env.Locals with
            | Some value -> value
            | None -> Sym id
        else
            Sym id

    // For symbols in operator position:
    // Those starting with upper-case letter are resolved using the local stack.
    // Those not starting with upper-case letter are resolved using the global function namespace.
    // Symbols in operator position are never idle.
    let private resolveFunction env id =
        if isVar id then
            match Map.tryFind id env.Locals with
            | Some value ->
                match value with
                | Func f -> f
                | _ -> err "Symbol does not represent a function"
            | None -> err("Symbol not defined: " + id)
        else
            match env.Globals.Functions.GetMaybe id with
            | Some f -> f
            | None -> err("Symbol not defined: " + id)

    /// <summary>
    /// Applies a function to a set of arguments and a global
    /// environment, considering whether to defer evaluation
    /// based on the Head/Tail position of the application.
    /// </summary>
    /// <remarks>
    /// Applying a function to fewer arguments than it takes
    /// results in a partial. Applying a function to more arguments
    /// than it takes causes the function to get applied to only
    /// the first N arguments and the result must resolve to
    /// a function which is then applied to the remaining arguments.
    /// </remarks>
    let rec apply pos globals pinfo f args =
        match f with

        // Freezes always take 0 arguments and they retain local
        // state that they capture when a freeze expression is evaluated.
        // Freezes do not consider Head/Tail position as they cannot
        // naturally be recursive.
        | Freeze(locals, body) ->
            let env = {Globals = globals; Locals = locals; PInfo = pinfo}
            match args with
            | [] -> evalw env body
            | _ ->
                match eval env body with
                | Func f -> apply pos globals pinfo f args
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos globals pinfo f args
                | _ -> err "Function expected/too many arguments provided to freeze"
                
        // Lambdas always take 1 arguments and they retain local
        // state that they capture when a lambda expression is evaluated.
        // Lambdas do not consider Head/Tail position as they cannot
        // naturally be recursive.
        | Lambda(param, locals, body) as lambda ->
            let env = {Globals = globals; Locals = locals; PInfo = pinfo}
            match args with
            | [] -> Done(Func lambda)
            | [arg0] -> evalw (appendLocals env [param, arg0]) body
            | arg0 :: args1 ->
                match eval (appendLocals env [param, arg0]) body with
                | Func f -> apply pos globals pinfo f args1
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos globals pinfo f args1
                | _ -> err "Function expected/too many arguments provided to lambda"

        // Defuns take any number of arguments and do not retain any local state.
        // Evaluation is deferred if the application is in tail position.
        | Defun(name, paramz, body) as defun ->
            let env = {Globals = globals; Locals = Map.empty; PInfo = pinfo}
            match args.Length, paramz.Length with
            | Lesser ->
                match args with
                | [] -> Done(Func defun)
                | _ -> Done(Func(Partial(defun, args)))
            | Equal ->
                let env = appendLocals env (List.zip paramz args)
                match pos with
                | Head -> evalw env body
                | Tail -> thunkw(fun () -> evalw env body)
            | Greater ->
                let args0 = List.take paramz.Length args
                let args1 = List.skip paramz.Length args
                match eval (appendLocals env (List.zip paramz args0)) body with
                | Func f -> apply pos globals pinfo f args1
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos globals pinfo f args1
                | _ -> err "Function expected/too many arguments provided to defun"

        // Primitives take and number of arguments and do not retain any local state.
        // Head/Tail position is also not considered.
        | Native(name, arity, f) as native ->
            match args.Length, arity with
            | Lesser ->
                match args with
                | [] -> Done(Func native)
                | _ -> Done(Func(Partial(native, args)))
            | Equal -> Done(f globals args)
            | Greater ->
                let (args0, args1) = List.splitAt arity args
                match f globals args0 with
                | Func f -> apply pos globals pinfo f args1
                | Sym s ->
                    let env = {Globals = globals; Locals = Map.empty; PInfo = pinfo}
                    let f = resolveFunction env s
                    apply pos globals pinfo f args1
                | _ -> err "Function expected/too many arguments provided to native"

        // Applying a partial is just applying  the original function
        // with the previous and current argument lists appended.
        | Partial(f, previousArgs) as partial ->
            match args with
            | [] -> Done(Func(partial))
            | _ -> apply pos globals pinfo f (List.append previousArgs args)

    and private evalw env expr =
        match expr with

        // Atomic values besides symbols are self-evaluating
        | EmptyExpr  -> Done(Empty)
        | BoolExpr b -> Done(Bool b)
        | IntExpr n  -> Done(Int n)
        | DecExpr n  -> Done(Dec n)
        | StrExpr s  -> Done(Str s)

        // Should only get here in the case of symbols not in operator position
        // In this case, symbols always evaluate without error.
        | SymExpr s -> Done(resolveSymbol env s)

        // When the first expression evaluates to false,
        // false is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | AndExpr(left, right) ->
            if vbool(eval env left)
                then evalw env right
                else falsew
            
        // When the first expression evaluates to true,
        // true is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | OrExpr(left, right) ->
            if vbool(eval env left)
                then truew
                else evalw env right

        // If expressions selectively evaluate depending on the result
        // of evaluating the condition expression
        // The condition must evaluate to a boolean value
        | IfExpr (condition, consequent, alternative) ->
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
        | LetExpr (symbol, binding, body) ->
            let value = eval env binding
            evalw (appendLocals env [symbol, value]) body

        // Evaluating a lambda captures the local state, the lambda parameter name and the body expression
        | LambdaExpr (param, body) ->
            Done(Func(Lambda(param, env.Locals, body)))

        // Evaluating a freeze just captures the local state and the body expression
        | FreezeExpr expr ->
            Done(Func(Freeze(env.Locals, expr)))

        // Handler expression is not evaluated unless body results in an error
        // Handler expression must evaluate to a function
        | TrapExpr (pos, body, handler) ->
            try
                Done(eval env body)
            with
            | SimpleError message ->
                match eval env handler with
                | Func f -> apply pos env.Globals env.PInfo f [Err message]
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos env.Globals env.PInfo f [Err message]
                | _ -> err "Trap handler did not evaluate to a function"
            | _ -> reraise()

        // Expression in operator position must eval to a function
        // or to a symbol which resolves to a function.
        | AppExpr (pos, f, args) ->
            match f with
            | SymExpr s ->
                let operator = resolveFunction env s
                let operands = List.map (eval env) args
                if env.PInfo.On then
                    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
                    let result = apply pos env.Globals env.PInfo operator operands
                    stopwatch.Stop()
                    if env.PInfo.Times.ContainsKey(s) then
                        let (n, avg) = env.PInfo.Times.[s]
                        env.PInfo.Times.[s] <- (n + 1, ((int64 n) * avg + stopwatch.ElapsedMilliseconds) / (int64 (n + 1)))
                    else
                        env.PInfo.Times.Add(s, (1, stopwatch.ElapsedMilliseconds))
                    result
                else
                    apply pos env.Globals env.PInfo operator operands
            | expr ->
                match eval env expr with
                | Func operator ->
                    let operands = List.map (eval env) args
                    apply pos env.Globals env.PInfo operator operands
                | Sym s ->
                    let operator = resolveFunction env s
                    let operands = List.map (eval env) args
                    apply pos env.Globals env.PInfo operator operands
                | _ -> err "Expression at head of application did not resolve to function"

    /// <summary>
    /// Evaluates an sub-expression into a value, running all deferred
    /// computations in the process.
    /// </summary>
    and eval env expr = go(evalw env expr)

    /// <summary>
    /// Evaluates a root-level expression into a value, running all side
    /// effects in the process.
    /// </summary>
    let rootEval globals pinfo expr =
        match expr with
        | DefunExpr(name, paramz, body) ->
            let f = Defun(name, paramz, body)
            globals.Functions.[name] <- f
            Sym name

        | OtherExpr expr ->
            let env = {Globals = globals; Locals = Map.empty; PInfo = pinfo}
            eval env expr

    let applyc globals f args = go(apply Head globals (new PInfo()) f args)
