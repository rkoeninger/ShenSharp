namespace Kl

open Extensions
open Values
open Expressions

module Evaluator =

    // Work that may be deferred. Used as trampolines for tail-call optimization.
    type private Work =
        | Done of Value
        | Pending of Locals * Value

    let private defer locals expr = Pending(locals, expr)

    let private appendLocals = List.fold (fun m (k, v) -> Map.add k v m)

    // Symbols in operand position are either defined locally or they are idle.
    let private resolveSymbol locals id =
        match Map.tryFind id locals with
        | Some value -> value
        | None -> Sym id

    let private resolveGlobalFunction globals id =
        match globals.Functions.GetMaybe id with
        | Some f -> f
        | None -> errf "Function not defined: %s" id

    // Symbols in operator position are either:
    //   * A local variable whose value is a function.
    //   * A local variable whose value is a symbol that resolves to a global function.
    //   * A symbol that resolves to a global function.
    let private resolveFunction (globals, locals) id =
        match Map.tryFind id locals with
        | Some(Func f) -> f
        | Some(Sym id) -> resolveGlobalFunction globals id
        | Some _ -> errf "Function not defined: %s" id
        | None -> resolveGlobalFunction globals id

    // Applies function to arguments.
    // Could return deferred work or a partial function.
    let rec private apply globals f args =
        match f with

        // Freezes can only be applied to 0 arguments.
        // They evaluate their body with local scope captured where they were formed.
        | Freeze(locals, body) ->
            match args with
            | [] -> defer locals body
            | _ -> errf "Too many arguments (%i) provided to freeze" args.Length

        // Each lambda only takes exactly 1 argument.
        // Applying a lambda to 0 arguments is an error.
        // Applying a lambda to more than 1 argument will apply the remaining
        // arguments to the returned function. If lambda does not return another
        // function, this is an error.
        // Lambdas evaluate their body with local scope captured when they were formed.
        | Lambda(param, locals, body) ->
            match args with
            | [] -> err "Zero arguments provided to lambda"
            | [arg0] -> defer (appendLocals locals [param, arg0]) body
            | arg0 :: args1 ->
                match evalv (globals, appendLocals locals [param, arg0]) body with
                | Func f -> apply globals f args1
                | Sym s ->
                    let f = resolveFunction (globals, locals) s
                    apply globals f args1
                | _ -> errf "Too many arguments (%i) provided to lambda" args.Length

        // Defuns can be applied to anywhere between 0 and the their full parameter list.
        // An error is raised if a Defun is applied to more arguments than it takes.
        // If applied to fewer arguments than the full parameter list, a Partial is returned.
        // They do not retain local state and are usually evaluated at the root level.
        | Defun(name, paramz, body) as defun ->
            match args.Length, paramz.Length with
            | Lesser ->
                match args with
                | [] -> Done(Func defun)
                | _ -> Done(Func(Partial(defun, args)))
            | Equal ->
                defer (Map(List.zip paramz args)) body
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
                Done(f globals args)
            | Greater ->
                errf "Too many arguments (%i) provided to native \"%s\"" args.Length name

        // Applying a partial applies the original function
        // to the previous and current argument lists appended.
        | Partial(f, previousArgs) as partial ->
            match args with
            | [] -> Done(Func(partial))
            | _ -> apply globals f (List.append previousArgs args)

    // Evaluates expression, deferring work in tail position.
    and private evalw ((globals, locals) as env) = function

        // Should only get here in the case of Symbols not in operator position.
        // In this case, Symbols always evaluate without error.
        | Sym s -> Done(resolveSymbol locals s)

        // Short-circuit evaluation. Both left and right must eval to Bool.
        | AndExpr(left, right) ->
            Done(Bool(isTrue(evalv env left) && isTrue(evalv env right)))

        // Short-circuit evaluation. Both left and right must eval to Bool.
        | OrExpr(left, right) ->
            Done(Bool(isTrue(evalv env left) || isTrue(evalv env right)))

        // Condition must evaluate to Bool. Consequent and alternative are in tail position.
        | IfExpr(condition, consequent, alternative) ->
            if isTrue(evalv env condition)
                then defer locals consequent
                else defer locals alternative

        // Conditions must evaluate to Bool. Consequents are in tail position.
        | CondExpr clauses ->
            let rec evalClauses = function
                | [] -> Done Empty
                | (condition, consequent) :: rest ->
                    if isTrue(evalv env condition)
                        then defer locals consequent
                        else evalClauses rest
            evalClauses clauses

        // Body expression is in tail position.
        | LetExpr(symbol, binding, body) ->
            let value = evalv env binding
            defer (appendLocals locals [symbol, value]) body

        // Lambdas capture local scope.
        | LambdaExpr(param, body) ->
            Done(Func(Lambda(param, locals, body)))

        // Freezes capture local scope.
        | FreezeExpr body ->
            Done(Func(Freeze(locals, body)))

        // Handler expression only evaluated if body results in an error.
        // Handler expression must evaluate to a Function.
        // Handler expression is in tail position.
        | TrapExpr(body, handler) ->
            try
                Done(evalv env body)
            with
            | :? SimpleError as e ->
                let operator = evalf env handler
                apply globals operator [Err e.Message]
            | _ -> reraise()

        // Second expression is in tail position.
        | DoExpr(first, second) ->
            evalv env first |> ignore
            defer locals second

        // Evaluating a defun just takes the name, param list and body
        // and stores them in the global function scope.
        | DefunExpr(name, paramz, body) ->
            globals.Functions.[name] <- Defun(name, paramz, body)
            Done(Sym name)

        // Expression in operator position must evaluate to a Function.
        | AppExpr(f, args) ->
            let operator = evalf env f
            let operands = List.map (evalv env) args
            apply globals operator operands

        // All other expressions/values are self-evaluating.
        | expr -> Done expr

    // Does a full eval of expr, looking to get a Function.
    // 3 ways this can work:
    //   * expr can be a symbol that resolves to a function.
    //   * expr can eval to function.
    //   * expr can eval to a symbol that resolves to a function.
    and private evalf env expr =
        match expr with
        | Sym s -> resolveFunction env s
        | _ ->
            match evalv env expr with
            | Func f -> f
            | Sym s -> resolveFunction env s
            | _ -> err "Operator expression must evaluate to a function"

    // Evaluates an expression, running all deferred work.
    // Must be tail recursive. This is where tail call optimization happens.
    and private evalv ((globals, _) as env) expr =
        match evalw env expr with
        | Done value -> value
        | Pending(locals, expr) -> evalv (globals, locals) expr

    /// <summary>
    /// Evaluates an expression into a value, starting with a new, empty local scope.
    /// </summary>
    [<CompiledName "Eval">]
    let eval globals expr = evalv (globals, Map.empty) expr
