module Kl.Evaluator

open Extensions
open Values
open Analysis

// Work that may be deferred. Used as trampolines for tail-call optimization.
type private Work =
    | Done of Value
    | Pending of Locals * Expr

// Returns Partial if fewer args than function arity.
let rec private applyw globals f (args: Value list) =
    let argc = args.Length
    let curried v args =
        match v with
        | Func f -> applyw globals f args
        | _ -> failwith "Too many arguments passed to function"
    match f with

    // Application of Interpreted functions is deferred if not over-applied.
    | Interpreted(locals, paramz, body) ->
        if argc < paramz.Length then
            Done(Func(if argc = 0 then f else Partial(f, args)))
        elif argc > paramz.Length then
            let (args0, args1) = List.splitAt paramz.Length args
            let locals = merge locals (Map(List.zip paramz args0))
            curried (evalv (globals, locals) body) args1
        else
            Pending(merge locals (Map(List.zip paramz args)), body)

    // Compiled functions are always applied immediately.
    | Compiled(arity, native) ->
        if argc < arity then
            Done(Func(if argc = 0 then f else Partial(f, args)))
        elif argc > arity then
            let (args0, args1) = List.splitAt arity args
            curried (native globals args0) args1
        else
            Done(native globals args)

    // Applying a partial applies the original function
    // to the previous and current argument lists appended.
    | Partial(inner, args0) ->
        if argc = 0
            then Done(Func f)
            else applyw globals inner (args0 @ args)

// Evaluates expression, deferring work in tail position.
and private evalw ((globals, locals) as env) = function

    // Should only get here in the case of Symbols not in operator position.
    // Symbols in operand position are either defined locally or they are idle.
    | Constant(Sym id) -> Done(defaultArg (Map.tryFind id locals) (Sym id))

    // Other constants are self-evaluating
    | Constant value -> Done value

    // Short-circuit evaluation. Both left and right must eval to Bool.
    | Conjunction(left, right) ->
        Done(Bool(isTrue(evalv env left) && isTrue(evalv env right)))

    // Short-circuit evaluation. Both left and right must eval to Bool.
    | Disjunction(left, right) ->
        Done(Bool(isTrue(evalv env left) || isTrue(evalv env right)))

    // Condition must evaluate to Bool. Consequent and alternative are in tail position.
    | Conditional(condition, consequent, alternative) ->
        if isTrue(evalv env condition)
            then Pending(locals, consequent)
            else Pending(locals, alternative)

    // Body expression is in tail position.
    | Binding(param, value, body) ->
        let binding = evalv env value
        Pending(Map.add param binding locals, body)

    // Lambdas capture local scope.
    | Anonymous(Some param, body) ->
        Done(Func(Interpreted(locals, [param], body)))

    // Freezes capture local scope.
    | Anonymous(None, body) ->
        Done(Func(Interpreted(locals, [], body)))

    // Handler expression only evaluated if body results in an error.
    // Handler expression must evaluate to a Function.
    // Handler expression is in tail position.
    | Catch(body, handler) ->
        try
            Done(evalv env body)
        with e ->
            let operator = evalf env handler
            applyw globals operator [Err e.Message]

    // Second expression is in tail position.
    | Sequential exprs ->
        let rec evalSeq = function
            | [] -> failwith "empty seq"
            | [last] -> Pending(locals, last)
            | next :: rest ->
                evalv env next |> ignore
                evalSeq rest
        evalSeq exprs

    // Should exhibit same behavior as (set id expr)
    | Assignment((_, sref, _), expr) ->
        let value = evalv env expr
        sref.Value <- Some value
        Done value

    // Should exhibit same behavior as (value id)
    | Retrieval((id, sref, _)) ->
        match sref.Value with
        | Some value -> Done value
        | None -> failwithf "Symbol \"%s\" has no value" id

    // Evaluating a defun just takes the name, param list and body
    // and stores them in the global function scope.
    // Ignore attempts to redefine a primitive.
    | Definition((id, _, fref), paramz, body) ->
        if not(globals.PrimitiveFunctions.Contains id) then
            fref.Value <- Some(Interpreted(Map.empty, paramz, body))
        Done(Sym id)

    // Immediate lookup for global functions.
    // Should exhibit same behavior as if it was no optimized.
    | GlobalCall((id, _, fref), args) ->
        match fref.Value with
        | Some f -> applyw globals f (List.map (evalv env) args)
        | None -> failwithf "Function not defined: %s" id

    // Expression in operator position must evaluate to a Function.
    | Application(f, args) ->
        let operator = evalf env f
        let operands = List.map (evalv env) args
        applyw globals operator operands

// Does a full eval of expr, looking to get a Function.
// 3 ways this can work:
//   * expr can be a symbol that resolves to a local function.
//   * expr can be a symbol that resolves to a global function.
//   * expr can eval to function.
and private evalf ((globals, locals) as env) expr =
    match expr with
    | Constant(Sym id) ->
        match Map.tryFind id locals with
        | Some(Func f) -> f
        | Some _ -> failwithf "Function not defined: %s" id
        | None -> lookup globals id
    | _ ->
        match evalv env expr with
        | Func f -> f
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
let eval globals expr = evalv (globals, Map.empty) (parse (globals, Set.empty) expr)

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
    | _ -> failwith "Operator expression must evaluate to a function"
