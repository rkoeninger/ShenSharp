namespace Kl

open Extensions

module Evaluator =

    let private appendLocals env defs = {env with Locals = List.Cons(Map.ofList defs, env.Locals)}

    let private appendTrace env frame = {env with Trace = List.Cons(frame, env.Trace)}

    let rec private resolveLocalSymbol locals id =
        match locals with
        | [] -> None
        | frame :: rest ->
            match Map.tryFind id frame with
            | Some(value) -> Some(value)
            | None -> resolveLocalSymbol rest id

    // For symbols not in operator position:
    // Those starting with upper-case letter are idle if not defined.
    // Those not starting with upper-case letter are always idle.
    let private resolveSymbol env id =
        if Values.isVar id then
            match resolveLocalSymbol env.Locals id with
            | Some value -> value
            | None -> Sym id
        else
            Sym id

    // For symbols in operator position:
    // Those starting with upper-case letter are resolved using the local stack.
    // Those not starting with upper-case letter are resolved using the global function namespace.
    // Symbols in operator position are never idle.
    let private resolveFunction env id =
        if Values.isVar id then
            match resolveLocalSymbol env.Locals id with
            | Some value ->
                match value with
                | Func f -> f
                | _ -> Values.err "Symbol does not represent a function"
            | None -> Values.err("Symbol not defined: " + id)
        else
            match env.Globals.Functions.GetMaybe id with
            | Some f -> f
            | None -> Values.err("Symbol not defined: " + id)

    /// <summary>
    /// Applies a function to a set of arguments and a global
    /// environment, considering whether to full evaluate
    /// passed on the Head/Tail position of the application
    /// </summary>
    let rec apply pos globals trace f args =
        let env = {Globals = globals; Locals = []; Trace = trace}

        // Applying functions to zero args just returns the same function,
        // except for freezes, which fail if applied to any arguments
        match f with
        | Freeze(locals, body) ->
            match args with
            | [] -> evalw {env with Locals = locals} body
            | _ -> Done(Values.err "Freezes do not take arguments")
        | Lambda(param, locals, body) as lambda ->
            match args with
            | [] -> Done(Func(lambda))
            | [x] -> evalw (appendLocals env [(param, x)]) body
            | _ -> Done(Values.err "Lambdas take exactly 1 argument")

        // Defuns and Primitives can have any number of arguments and
        // can be partially applied
        | Defun(name, paramz, body) as defun ->
            match args with
            | [] -> Done(Func(defun))
            | _ ->
                match args.Length, paramz.Length with
                | Greater -> Values.arityErr name paramz.Length args
                | Lesser -> Done(Func(Partial(defun, args)))
                | Equal ->
                    let env = appendLocals env (List.zip paramz args)
                    match pos with
                    | Head -> evalw env body
                    | Tail -> Values.thunkw(fun () -> evalw env body)
        | Primitive(name, arity, f) as primitive ->
            match args with
            | [] -> Done(Func primitive)
            | _ ->
                match args.Length, arity with
                | Greater -> Values.arityErr name arity args
                | Lesser -> Done(Func(Partial(primitive, args)))
                | Equal -> Done(f globals args)
        | Partial(f, args0) as partial ->
            match args with
            | [] -> Done(Func(partial))
            | _ -> apply pos globals trace f (List.append args0 args)

    and private evalw env expr =
        match expr with

        // Atomic values besides symbols are self-evaluating
        | EmptyExpr  -> Done(Empty)
        | BoolExpr b -> Done(Bool b)
        | IntExpr n  -> Done(Int n)
        | DecExpr n  -> Done(Dec n)
        | StrExpr s  -> Done(Str s)

        // Should only get here in the case of symbols not in operator position
        // In this case, symbols always evaluate without error
        | SymExpr s -> Done(resolveSymbol env s)

        // And/Or expressions are lazily evaluated

        // When the first expression evaluates to false,
        // false is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | AndExpr(left, right) ->
            if Values.vbool(eval (appendTrace env "and/left") left)
                then evalw (appendTrace env "and/right") right
                else Values.falsew
            
        // When the first expression evaluates to true,
        // true is the result without evaluating the second expression
        // The first expression must evaluate to a boolean value
        | OrExpr(left, right) ->
            if Values.vbool(eval (appendTrace env "or/left") left)
                then Values.truew
                else evalw (appendTrace env "or/right") right

        // If expressions selectively evaluate depending on the result
        // of evaluating the condition expression
        // The condition must evaluate to a boolean value
        | IfExpr (condition, consequent, alternative) ->
            if Values.vbool(eval (appendTrace env "if/condition") condition)
                then evalw (appendTrace env "if/consequent") consequent
                else evalw (appendTrace env "if/alternative") alternative
        
        // Condition expressions must evaluate to boolean values
        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> Values.err "No condition was true"
                | (condition, consequent) :: rest ->
                    if Values.vbool(eval (appendTrace env "cond/condition") condition)
                        then evalw (appendTrace env "cond/consequent") consequent
                        else evalClauses rest
            evalClauses clauses

        | LetExpr (symbol, binding, body) ->
            let value = eval (appendTrace env (sprintf "let/%s/binding" symbol)) binding
            evalw (appendLocals (appendTrace env (sprintf "let/%s/body" symbol)) [symbol, value]) body

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
                Done(eval (appendTrace env "trap-error/body") body)
            with
            | SimpleError message ->
                match eval (appendTrace env "trap-error/handler") handler with
                | Func f -> apply pos env.Globals env.Trace f [Err message]
                | _ -> Values.err "Trap handler did not evaluate to a function"
            | e -> raise e

        // Applications expect a symbol to be in operator position
        // That symbol must evaluate to a function
        // If evaluating any of the argument expressions results in an error,
        // then the application expression results in that error without
        // the function being applied
        | AppExpr (pos, f, args) ->
            match f with
            | SymExpr s ->
                let operator = resolveFunction env s
                let operands = List.map (eval env) args
                let env = appendTrace env (sprintf "app/%s" s)
                apply pos env.Globals env.Trace operator operands
            | _ -> Values.err "Application must begin with a symbol"
    
    /// <summary>
    /// Evaluates an sub-expression into a value, running all side effects
    /// in the process.
    /// </summary>
    and eval env expr = Values.go(evalw env expr)

    /// <summary>
    /// Evaluates a root-level expression into a value, running all side
    /// effects in the process.
    /// </summary>
    let rootEval globals expr =
        let env = {Globals = globals; Locals = []; Trace = []}

        match expr with
        | DefunExpr(name, paramz, body) ->
            let f = Defun(name, paramz, body)
            globals.Functions.[name] <- f
            Func f

        | OtherExpr expr -> eval env expr
