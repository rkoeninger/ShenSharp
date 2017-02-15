namespace Kl

open Extensions
open Values

module Evaluator =

    // Work that may be deferred. Used as trampolines for tail-call optimization.
    type private Work =
        | Done of Value
        | Pending of Locals * Value

    let inline private defer locals expr = Pending(locals, expr)

    // Symbols in operand position are either defined locally or they are idle.
    let private resolveSymbol locals id =
        defaultArg (Map.tryFind id locals) (Sym id)

    /// <summary>
    /// Looks up id in the global function namespace.
    /// Raises an error if function not defined.
    /// </summary>
    let resolveGlobalFunction globals id =
        match globals.Functions.GetMaybe id with
        | Some f -> f
        | None -> failwithf "Function not defined: %s" id

    // Symbols in operator position are either:
    //   * A local variable whose value is a function.
    //   * A local variable whose value is a symbol that resolves to a global function.
    //   * A symbol that resolves to a global function.
    let private resolveFunction (globals, locals) id =
        match Map.tryFind id locals with
        | Some(Func f) -> f
        | Some(Sym id) -> resolveGlobalFunction globals id
        | Some _ -> failwithf "Function not defined: %s" id
        | None -> resolveGlobalFunction globals id

    // Applies function to arguments.
    // Could return deferred work or a partial function.
    let rec private applyw globals f args =
        match f with

        // Freezes can only be applied to 0 arguments.
        // They evaluate their body with local scope captured where they were formed.
        | Freeze impl ->
            match args with
            | [] ->
                match impl with
                | InterpretedFreeze(locals, body) -> defer locals body
                | CompiledFreeze native -> Done(native globals)
            | _ -> failwithf "%O expected 0 arguments, given %i" f args.Length

        // Each lambda only takes exactly 1 argument.
        // Applying a lambda to 0 arguments is an error.
        // Applying a lambda to more than 1 argument will apply the remaining
        // arguments to the returned function. If lambda does not return another
        // function, this is an error.
        // Lambdas evaluate their body with local scope captured when they were formed.
        | Lambda impl ->
            match args with
            | [] -> failwithf "%O expected 1 arguments, given 0" f
            | [arg0] ->
                match impl with
                | InterpretedLambda(locals, param, body) -> defer (Map.add param arg0 locals) body
                | CompiledLambda native -> Done(native globals arg0)
            | arg0 :: args1 ->
                let result =
                    match impl with
                    | InterpretedLambda(locals, param, body) -> evalv (globals, Map.add param arg0 locals) body
                    | CompiledLambda native -> native globals arg0
                match result with
                | Func f -> applyw globals f args1
                | Sym s ->
                    let locals =
                        match impl with
                        | InterpretedLambda(locals, _, _) -> locals
                        | CompiledLambda _ -> Map.empty
                    let f = resolveFunction (globals, locals) s
                    applyw globals f args1
                | _ -> failwithf "%O expected 1 arguments, given %i" f args.Length

        // Defuns can be applied to anywhere between 0 and the their full parameter list.
        // An error is raised if a Defun is applied to more arguments than it takes.
        // If applied to fewer arguments than the full parameter list, a Partial is returned.
        // They do not retain local state and are usually evaluated at the root level.
        | Defun(name, arity, impl) ->
            if args.Length < arity then
                match args with
                | [] -> Done(Func f)
                | _ -> Done(Func(Partial(f, args)))
            else
                match impl with
                | InterpretedDefun(paramz, body) ->
                    if args.Length > arity
                        then failwithf "%O expected %i arguments, given %i" f arity args.Length
                        else defer (Map(List.zip paramz args)) body
                | CompiledDefun native -> Done(native globals args)

        // Applying a partial applies the original function
        // to the previous and current argument lists appended.
        | Partial(f, previousArgs) as partial ->
            match args with
            | [] -> Done(Func(partial))
            | _ -> applyw globals f (previousArgs @ args)

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
            defer (Map.add symbol value locals) body

        // Lambdas capture local scope.
        | LambdaExpr(param, body) ->
            Done(Func(Lambda(InterpretedLambda(locals, param, body))))

        // Freezes capture local scope.
        | FreezeExpr body ->
            Done(Func(Freeze(InterpretedFreeze(locals, body))))

        // Handler expression only evaluated if body results in an error.
        // Handler expression must evaluate to a Function.
        // Handler expression is in tail position.
        | TrapExpr(body, handler) ->
            try
                Done(evalv env body)
            with e ->
                let operator = evalf env handler
                applyw globals operator [Err e.Message]

        // Second expression is in tail position.
        | DoExpr(first, second) ->
            evalv env first |> ignore
            defer locals second

        // Evaluating a defun just takes the name, param list and body
        // and stores them in the global function scope.
        // Ignore attempts to redefine a primitive.
        | DefunExpr(name, paramz, body) ->
            if not(globals.PrimitiveFunctions.Contains name) then
                globals.Functions.[name] <-
                    Defun(name, List.length paramz, InterpretedDefun(paramz, body))
            Done(Sym name)

        // Expression in operator position must evaluate to a Function.
        | AppExpr(f, args) ->
            let operator = evalf env f
            let operands = List.map (evalv env) args
            applyw globals operator operands

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
            | _ -> failwith "Operator expression must evaluate to a function"

    // Evaluates an expression, running all deferred work.
    // Must be tail recursive. This is where tail call optimization happens.
    and private evalv ((globals, _) as env) expr =
        match evalw env expr with
        | Done value -> value
        | Pending(locals, expr) -> evalv (globals, locals) expr

    /// <summary>
    /// Evaluates an expression into a value, starting with a new, empty local scope.
    /// </summary>
    let eval globals expr = evalv (globals, Map.empty) expr

    /// <summary>
    /// Applies a function to a list of values.
    /// </summary>
    let apply globals f args =
        match applyw globals f args with
        | Done value -> value
        | Pending(locals, expr) -> evalv (globals, locals) expr

    /// <summary>
    /// Interprets a value as a function and applies it to a list of values.
    /// </summary>
    let vapply globals value args =
        match value with
        | Func f -> apply globals f args
        | Sym s -> apply globals (resolveGlobalFunction globals s) args
        | _ -> failwith "Operator expression must evaluate to a function"
