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
    let rec apply pos globals f args =
        match f with

        // Freezes always take 0 arguments and they retain local
        // state that they capture when a freeze expression is evaluated.
        // Freezes do not consider Head/Tail position as they cannot
        // naturally be recursive.
        | Freeze(locals, body) ->
            let env = {Globals = globals; Locals = locals}
            match args with
            | [] -> evalw Tail env body
            | _ ->
                match eval env body with
                | Func f -> apply pos globals f args
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos globals f args
                | _ -> err "Function expected/too many arguments provided to freeze"
                
        // Lambdas always take 1 arguments and they retain local
        // state that they capture when a lambda expression is evaluated.
        // Lambdas do not consider Head/Tail position as they cannot
        // naturally be recursive.
        | Lambda(param, locals, body) as lambda ->
            let env = {Globals = globals; Locals = locals}
            match args with
            | [] -> Done(Func lambda)
            | [arg0] -> evalw Tail (appendLocals env [param, arg0]) body
            | arg0 :: args1 ->
                match eval (appendLocals env [param, arg0]) body with
                | Func f -> apply pos globals f args1
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos globals f args1
                | _ -> err "Function expected/too many arguments provided to lambda"

        // Defuns take any number of arguments and do not retain any local state.
        // Evaluation is deferred if the application is in tail position.
        | Defun(name, paramz, body) as defun ->
            let env = {Globals = globals; Locals = Map.empty}
            match args.Length, paramz.Length with
            | Lesser ->
                match args with
                | [] -> Done(Func defun)
                | _ -> Done(Func(Partial(defun, args)))
            | Equal ->
                let env = appendLocals env (List.zip paramz args)
                match pos with
                | Head -> evalw pos env body
                | Tail -> thunkw(fun () -> evalw pos env body)
            | Greater ->
                let args0 = List.take paramz.Length args
                let args1 = List.skip paramz.Length args
                match eval (appendLocals env (List.zip paramz args0)) body with
                | Func f -> apply pos globals f args1
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos globals f args1
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
                | Func f -> apply pos globals f args1
                | Sym s ->
                    let env = {Globals = globals; Locals = Map.empty}
                    let f = resolveFunction env s
                    apply pos globals f args1
                | _ -> err "Function expected/too many arguments provided to native"

        // Applying a partial is just applying  the original function
        // with the previous and current argument lists appended.
        | Partial(f, previousArgs) as partial ->
            match args with
            | [] -> Done(Func(partial))
            | _ -> apply pos globals f (List.append previousArgs args)

    and private evalw pos env expr =
        match expr with

        // TODO: add DoExpr?
        //       (do ~@exprs)
        //       has last expression in tail position
        //       https://github.com/gregspurrier/klam/blob/master/spec/functional/extensions/do_spec.rb

        // Atomic values besides symbols are self-evaluating
        | (Empty | Bool _ | Int _ | Dec _ | Str _) as x  -> Done x

        // Should only get here in the case of symbols not in operator position
        // In this case, symbols always evaluate without error.
        | Sym s -> Done(resolveSymbol env s)

        // (and ~left ~right)
        // When the first expression evaluates to false,
        // false is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | Cons(Sym "and", Cons(left, Cons(right, Empty))) ->
            if vbool(eval env left)
                then evalw pos env right
                else falsew
        
        // (or ~left ~right)
        // When the first expression evaluates to true,
        // true is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | Cons(Sym "or", Cons(left, Cons(right, Empty))) ->
            if vbool(eval env left)
                then truew
                else evalw pos env right

        // (if ~condition ~consequent ~alternative)
        // If expressions selectively evaluate depending on the result
        // of evaluating the condition expression
        // The condition must evaluate to a boolean value
        | Cons(Sym "if", Cons(condition, Cons(consequent, Cons(alternative, Empty)))) ->
            if vbool(eval env condition)
                then evalw pos env consequent
                else evalw pos env alternative
        
        // (cond ~@clauses)
        // Condition expressions must evaluate to boolean values
        // Evaluation of clauses stops when one of their conditions
        // evaluates to true.
        | Cons(Sym "cond", clauses) ->
            let rec evalClauses = function
                | Empty -> err "No condition was true"
                | Cons(condition, Cons(consequent, rest)) ->
                    if vbool(eval env condition)
                        then evalw pos env consequent
                        else evalClauses rest
                | _ -> err "Unexpected value in cond"
            evalClauses clauses

        // (let ~symbol ~binding ~body)
        // Let expressions evaluate the symbol binding first and then evaluate the body
        // with the result of evaluating the binding bound to the symbol
        | Cons(Sym "let", Cons(Sym symbol, Cons(binding, Cons(body, Empty)))) ->
            let value = eval env binding
            evalw pos (appendLocals env [symbol, value]) body

        // (lambda ~param ~body)
        // Evaluating a lambda captures the local state, the lambda parameter name and the body expression
        | Cons(Sym "lambda", Cons(Sym param, Cons(body, Empty))) ->
            Done(Func(Lambda(param, env.Locals, body)))

        // (freeze ~body)
        // Evaluating a freeze just captures the local state and the body expression
        | Cons(Sym "freeze", Cons(body, Empty)) ->
            Done(Func(Freeze(env.Locals, body)))

        // (trap-error ~body ~handler)
        // Handler expression is not evaluated unless body results in an error
        // Handler expression must evaluate to a function
        | Cons(Sym "trap-error", Cons(body, Cons(handler, Empty))) ->
            try
                Done(eval env body)
            with
            | SimpleError message ->
                match eval env handler with
                | Func f -> apply pos env.Globals f [Err message]
                | Sym s ->
                    let f = resolveFunction env s
                    apply pos env.Globals f [Err message]
                | _ -> err "Trap handler did not evaluate to a function"
            | _ -> reraise()

        // (~f ~@args)
        // Expression in operator position must eval to a function
        // or to a symbol which resolves to a function.
        | Cons(f, args) ->
            match f with
            | Sym s ->
                let operator = resolveFunction env s
                let operands = List.map (eval env) (toList args)
                apply pos env.Globals operator operands
            | expr ->
                match eval env expr with
                | Func operator ->
                    let operands = List.map (eval env) (toList args)
                    apply pos env.Globals operator operands
                | Sym s ->
                    let operator = resolveFunction env s
                    let operands = List.map (eval env) (toList args)
                    apply pos env.Globals operator operands
                | _ -> err "Expression at head of application did not resolve to function"

        // TODO: explicitly raise error on special forms with incorrect number of args?
        //        | ComboToken(SymToken "or" :: _) ->
        //    failwith "or expression must have exactly 2 argument expressions"

        // TODO: explicitly reject defun's not at root level?
        //// (defun ...)
        //| ComboToken(SymToken "defun" :: _) ->
        //    failwith "defun expressions cannot appear below the root level"

        | _ -> err "Unexpected value type - cannot evaluate"

    /// <summary>
    /// Evaluates an sub-expression into a value, running all deferred
    /// computations in the process.
    /// </summary>
    and eval env expr = go(evalw Tail env expr)

    /// <summary>
    /// Evaluates a root-level expression into a value, running all side
    /// effects in the process.
    /// </summary>
    let rootEval globals expr =
        match expr with
        | Cons(Sym "defun", Cons(Sym name, Cons(paramz, Cons(body, Empty)))) ->
            let f = Defun(name, List.map vsym (toList paramz), body)
            globals.Functions.[name] <- f
            Sym name

        | expr ->
            let env = {Globals = globals; Locals = Map.empty}
            eval env expr

    let applyc globals f args = go(apply Head globals f args)
