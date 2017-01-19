namespace Kl

open Extensions
open Values
open Expressions

module Evaluator =

    // Work that may be deferred. Used as trampolines for tail-call optimization.
    type private Work =
        | Done    of Value
        | Pending of Env * Value

    let private defer env expr = Pending(env, expr)

    let private appendLocals env defs =
        {env with Locals = List.fold (fun m (k, v) -> Map.add k v m) env.Locals defs}

    // Symbols not in operator position are either defined locally or they are idle.
    let private resolveSymbol env id =
        match Map.tryFind id env.Locals with
        | Some value -> value
        | None -> Sym id

    let private resolveGlobalFunction env id =
        match env.Globals.Functions.GetMaybe id with
        | Some f -> f
        | None -> errf "Function not defined: %s" id

    // Symbols in operator position are either:
    //   * A local variable whose value is a function.
    //   * A local variable whose value is a symbol that resolves to a global function.
    //   * A symbol that resolves to a global function.
    // Symbols in operator position are never idle.
    let private resolveFunction env id =
        match Map.tryFind id env.Locals with
        | Some(Func f) -> f
        | Some(Sym id) -> resolveGlobalFunction env id
        | Some _ -> errf "Function not defined: %s" id
        | None -> resolveGlobalFunction env id

    let rec private applyw env f args =
        match f with

        // Freezes can only be applied to 0 arguments.
        // They evaluate their body with local state captured where they were formed.
        | Freeze(locals, body) ->
            match args with
            | [] -> defer {env with Locals = locals} body
            | _ -> errf "Too many arguments (%i) provided to freeze" args.Length

        // Each lambda only takes 1 argument.
        // Applying a lambda to 0 arguments returns the same lambda.
        // Applying a lambda to more than 1 argument will apply the remaining
        // arguments to the returned function.
        // Lambdas evaluate their body with local state captured when they were formed.
        | Lambda(param, locals, body) as lambda ->
            let env = {env with Locals = locals}
            match args with
            | [] -> err "Zero arguments provided to lambda"
            | [arg0] -> defer (appendLocals env [param, arg0]) body
            | arg0 :: args1 ->
                match eval (appendLocals env [param, arg0]) body with
                | Func f -> applyw env f args1
                | Sym s ->
                    let f = resolveFunction env s
                    applyw env f args1
                | _ -> errf "Too many arguments (%i) provided to lambda" args.Length

        // Defuns can be applied to anywhere between 0 and the their full parameter list.
        // An error is raised if a Defun is applied to more arguments than it takes.
        // If applied to fewer arguments than the full parameter list, a Partial is returned.
        // They do not retain local state and are usually evaluated at the root level.
        | Defun(name, paramz, body) as defun ->
            let env = {env with Locals = Map.empty}
            match args.Length, paramz.Length with
            | Lesser ->
                match args with
                | [] -> Done(Func defun)
                | _ -> Done(Func(Partial(defun, args)))
            | Equal ->
                defer (appendLocals env (List.zip paramz args)) body
            | Greater ->
                errf "Too many arguments (%i) provided to defun \"%s\"" args.Length name

        // Natives have the same rules as Defuns.
        | Native(name, arity, f) as native ->
            match args.Length, arity with
            | Lesser ->
                match args with
                | [] -> Done(Func native)
                | _ -> Done(Func(Partial(native, args)))
            | Equal ->
                Done(f env.Globals args)
            | Greater ->
                errf "Too many arguments (%i) provided to native \"%s\"" args.Length name

        // Applying a partial applies the original function
        // to the previous and current argument lists appended.
        | Partial(f, previousArgs) as partial ->
            match args with
            | [] -> Done(Func(partial))
            | _ -> applyw env f (List.append previousArgs args)

    and private evalw env expr =
    
        // Booleans are just these two particular symbols.
        let isTrue = function
            | Sym "true" -> true
            | Sym "false" -> false
            | _ -> err "Conditional must evaluate to boolean"

        match expr with

        // Should only get here in the case of Symbols not in operator position.
        // In this case, Symbols always evaluate without error.
        | Sym s -> Done(resolveSymbol env s)

        // Short-circuit evaluation. Both left and right must eval to Bool.
        | AndExpr(left, right) ->
            Done(boolv(isTrue(eval env left) && isTrue(eval env right)))

        // Short-circuit evaluation. Both left and right must eval to Bool.
        | OrExpr(left, right) ->
            Done(boolv(isTrue(eval env left) || isTrue(eval env right)))

        // Condition must evaluate to Bool. Consequent and alternative are in tail position.
        | IfExpr(condition, consequent, alternative) ->
            if isTrue(eval env condition)
                then defer env consequent
                else defer env alternative

        // Conditions must evaluate to Bool. Consequents are in tail position.
        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> Done Empty
                | (condition, consequent) :: rest ->
                    if isTrue(eval env condition)
                        then defer env consequent
                        else evalClauses rest
            evalClauses clauses

        // Body expression is in tail position.
        | LetExpr(symbol, binding, body) ->
            let value = eval env binding
            defer (appendLocals env [symbol, value]) body

        // Lambdas capture local scope.
        | LambdaExpr(param, body) ->
            Done(Func(Lambda(param, env.Locals, body)))

        // Freezes capture local scope.
        | FreezeExpr body ->
            Done(Func(Freeze(env.Locals, body)))

        // Handler expression only evaluated if body results in an error.
        // Handler expression must evaluate to a Function.
        // Handler expression is in tail position.
        | TrapExpr(body, handler) ->
            try
                Done(eval env body)
            with
            | :? SimpleError as e ->
                let operator = evalFunction env handler
                applyw env operator [Err e.Message]
            | _ -> reraise()

        // Second expression is in tail position.
        | DoExpr(first, second) ->
            eval env first |> ignore
            defer env second

        // Evaluating a defun just takes the name, param list and body
        // and stores them in the global function scope.
        | DefunExpr(name, paramz, body) ->
            env.Globals.Functions.[name] <- Defun(name, paramz, body)
            Done(Sym name)

        // Expression in operator position must evaluate to a Function.
        | AppExpr(f, args) ->
            let operator = evalFunction env f
            let operands = List.map (eval env) args
            applyw env operator operands

        // All other expressions/values are self-evaluating.
        | _ -> Done expr

    // Does a full eval of expr, looking to get a Function.
    // 3 ways this can work:
    //   * expr can be a symbol that resolves to a function.
    //   * expr can eval to function.
    //   * expr can eval to a symbol that resolves to a function.
    and private evalFunction env expr =
        match expr with
        | Sym s -> resolveFunction env s
        | _ ->
            match eval env expr with
            | Func f -> f
            | Sym s -> resolveFunction env s
            | _ -> err "Operator expression must resolve to a function"

    /// <summary>
    /// Evaluates an expression into a value.
    /// </summary>
    and eval env expr =

        // Must be tail recursive. This is where tail call optimization happens.
        match evalw env expr with
        | Done value -> value
        | Pending(env, expr) -> eval env expr

    /// <summary>
    /// Evaluates an expression into a value, starting with a new, empty local scope.
    /// </summary>
    let rootEval globals = eval {Globals = globals; Locals = Map.empty}
