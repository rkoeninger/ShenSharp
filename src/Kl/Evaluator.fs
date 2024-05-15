module Kl.Evaluator

open Values

// Inlines captured scope into function body expression tree.
let rec private scope locals = function
    | Constant(Sym id) ->
        match Map.tryFind id locals with
        | Some value -> Constant value // Local variables get substituted.
        | None -> Constant(Sym id)     // Other symbols are unaffected.

    // Bindings, Lambdas and Definintions recur excluding their paramters.
    | Binding(param, value, body) ->
        Binding(param, scope locals value, scope (Map.remove param locals) body)
    | Anonymous(Some param, body) ->
        Anonymous(Some param, scope (Map.remove param locals) body)
    | Definition(name, paramz, body) ->
        Definition(name, paramz, scope (removeAll paramz locals) body)

    // All other expressions just recur or are unaffected.
    | Conjunction(left, right) ->
        Conjunction(scope locals left, scope locals right)
    | Disjunction(left, right) ->
        Disjunction(scope locals left, scope locals right)
    | Conditional(condition, consequent, alternative) ->
        Conditional(scope locals condition, scope locals consequent, scope locals alternative)
    | Anonymous(None, body) ->
        Anonymous(None, scope locals body)
    | Catch(body, handler) ->
        Catch(scope locals body, scope locals handler)
    | Sequential(exprs, last) ->
        Sequential(List.map (scope locals) exprs, scope locals last)
    | Assignment(symbol, expr) ->
        Assignment(symbol, scope locals expr)
    | GlobalCall(symbol, args) ->
        GlobalCall(symbol, List.map (scope locals) args)
    | Application(f, args) ->
        Application(scope locals f, List.map (scope locals) args)
    | other -> other

// Parses a raw KL AST into an optimized expression tree
let rec private parse ((globals, locals) as env) = function
    | Form [Sym "and"; left; right] ->
        Conjunction(parse env left, parse env right)
    | Form [Sym "or"; left; right] ->
        Disjunction(parse env left, parse env right)
    | Form [Sym "if"; condition; consequent; alternative] ->
        Conditional(parse env condition, parse env consequent, parse env alternative)
    | CondForm clauses ->
        let rec parseClauses = function
            | [] -> 
                GlobalCall(intern globals "simple-error", [Constant(Str "No condition was true")])
            | (Sym "true", consequent) :: _ ->
                parse env consequent
            | (condition, consequent) :: rest ->
                Conditional(parse env condition, parse env consequent, parseClauses rest)
        parseClauses clauses
    | Form [Sym "let"; Sym param; value; body] ->
        Binding(param, parse env value, parse (globals, Set.add param locals) body)
    | Form [Sym "lambda"; Sym param; body] ->
        Anonymous(Some param, parse (globals, Set.add param locals) body)
    | Form [Sym "freeze"; body] ->
        Anonymous(None, parse env body)
    | Form [Sym "trap-error"; body; handler] ->
        Catch(parse env body, parse env handler)
    | Form(Sym "do" :: _) as expr ->
        let rec flattenDo = function
            | Form(Sym "do" :: exprs) -> List.collect flattenDo exprs
            | expr -> [expr]
        let exprs = List.map (parse env) (flattenDo expr)
        Sequential(most exprs, List.last exprs)
    | DefunForm(name, paramz, body) ->
        Definition(intern globals name, paramz, parse (globals, Set.union (Set.ofList paramz) locals) body)
    | Form [Sym "set"; Sym id; value] when not(Set.contains id locals) ->
        Assignment(intern globals id, parse env value)
    | Form [Sym "value"; Sym id] when not(Set.contains id locals) ->
        Retrieval(intern globals id)
    | Form [Sym "type"; expr; _] ->
        parse env expr
    | Form(Sym id :: args) when not(Set.contains id locals) ->
        GlobalCall(intern globals id, List.map (parse env) args)
    | Form(f :: args) ->
        Application(parse env f, List.map (parse env) args)
    | value -> Constant value

/// <summary>
/// An immutable map of local variable definitions.
/// </summary>
type private Locals = Map<string, Value>

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
    | Interpreted(paramz, body) ->
        if argc < paramz.Length then
            Done(Func(if argc = 0 then f else Partial(f, args)))
        elif argc > paramz.Length then
            let (args0, args1) = List.splitAt paramz.Length args
            let locals = Map(List.zip paramz args0)
            curried (evalv (globals, locals) body) args1
        else
            Pending(Map(List.zip paramz args), body)

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

    // Other constants are self-evaluating.
    | Constant value -> Done value

    // Short-circuit evaluation. Both left and right must eval to Bool.
    | Conjunction(left, right) ->
        Done(Bool(asBool(evalv env left) && asBool(evalv env right)))

    // Short-circuit evaluation. Both left and right must eval to Bool.
    | Disjunction(left, right) ->
        Done(Bool(asBool(evalv env left) || asBool(evalv env right)))

    // Condition must evaluate to Bool. Consequent and alternative are in tail position.
    | Conditional(condition, consequent, alternative) ->
        if asBool(evalv env condition)
            then Pending(locals, consequent)
            else Pending(locals, alternative)

    // Body expression is in tail position.
    | Binding(param, value, body) ->
        let binding = evalv env value
        Pending(Map.add param binding locals, body)

    // Lambdas capture local scope.
    | Anonymous(Some param, body) ->
        Done(Func(Interpreted([param], scope (Map.remove param locals) body)))

    // Freezes capture local scope.
    | Anonymous(None, body) ->
        Done(Func(Interpreted([], scope locals body)))

    // Handler expression only evaluated if body results in an error.
    // Handler expression must evaluate to a Function.
    // Handler expression is in tail position.
    | Catch(body, handler) ->
        try
            Done(evalv env body)
        with e ->
            let operator = evalf env handler
            applyw globals operator [Err e.Message]

    // Final expression is in tail position.
    | Sequential(exprs, last) ->
        List.iter (evalv env >> ignore) exprs
        Pending(locals, last)

    // Should exhibit same behavior as (set id expr)
    | Assignment(symbol, expr) ->
        let value = evalv env expr
        symbol.Val := Some value
        Done value

    // Should exhibit same behavior as (value id)
    | Retrieval(symbol) ->
        Done(getValue symbol)

    // Evaluating a defun just takes the name, param list and body
    // and stores them in the global function scope.
    // Ignore attempts to redefine a primitive.
    | Definition(symbol, paramz, body) ->
        if not(!symbol.IsProtected) then
            symbol.Fun := Some(Interpreted(paramz, body))
        Done(Sym symbol.Name)

    // Immediate lookup for global functions.
    // Should exhibit same behavior as if it was no optimized.
    | GlobalCall(symbol, args) ->
        let operator = getFunction symbol
        let operands = List.map (evalv env) args
        applyw globals operator operands

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
        | Some _ -> failwithf "Function \"%s\" not defined" id
        | None -> lookup globals id
    | _ ->
        asFunction(evalv env expr)

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
