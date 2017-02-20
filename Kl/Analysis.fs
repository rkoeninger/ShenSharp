namespace Kl

open System
open System.Collections.Generic
open Kl
open Values
open Extensions

module Analysis =

    let rec flattenDo = function
        | DoExpr(first, second) -> flattenDo first @ flattenDo second
        | klExpr -> [klExpr]

    let rec functionArity = function
        | Defun(_, arity, _) -> arity
        | Lambda _ -> 1
        | Freeze _ -> 0
        | Partial(f, _) -> functionArity f

    let nonPrimitiveSymbols globals =
        globals.Symbols
        |> Seq.map (fun (kv: KeyValuePair<_, _>) -> (kv.Key, kv.Value))
        |> Seq.filter (fst >> globals.PrimitiveSymbols.Contains >> not)
        |> Seq.toList

    let nonPrimitiveFunctions globals =
        globals.Functions
        |> Seq.map (fun (kv: KeyValuePair<_, _>) -> (kv.Key, kv.Value))
        |> Seq.filter (fst >> globals.PrimitiveFunctions.Contains >> not)
        |> Seq.toList

    let private specialSymbols = [
        "and"
        "or"
        "if"
        "cond"
        "let"
        "lambda"
        "freeze"
        "trap-error"
        "do"
    ]

    let rec populate locals = function

        // Value is either substituted or remains a symbol
        | Sym id ->
            match Map.tryFind id locals with
            | Some value -> value
            | None -> Sym id

        // Let binding param is new variable masking old one
        | LetExpr(param, binding, body) ->
            toCons [
                Sym "let"
                Sym param
                binding
                populate (Map.remove param locals) body]

        // Lambda param is new variable masking old one
        | LambdaExpr(param, body) ->
            toCons [
                Sym "lambda"
                Sym param
                populate (Map.remove param locals) body]

        // Don't substitute special symbols: 'if, 'let, etc.
        | AppExpr(Sym f, args) when List.contains f specialSymbols ->
            toCons(Sym f :: (List.map (populate locals) args))

        | Cons(x, y) -> Cons(populate locals x, populate locals y)

        // Anything else just gets passed through
        | expr -> expr

    // TODO: remove the ability to pass in global functions as symbols:
    //       (map sq [1 2 3])
    //       just do it the standard way:
    //       (map (function sq) [1 2 3])

    // TODO: do we need to do this for simple Value exprs, which then get optimized,
    //       or substitute the optimized expr?
    // TODO: locals can be function scoped since captured variables are substituted.

    // ValueExpr -> OptimizedExpr -> OptimizedSubsitutedExpr
    let rec oPopulate locals = function
        | Conditional(condition, consequent, alternative) ->
            Conditional(
                oPopulate locals condition,
                oPopulate locals consequent,
                oPopulate locals alternative)
        | Binding(x, value, body) ->
            Binding(
                x,
                oPopulate locals value,
                oPopulate locals body)
        | Sequential exprs ->
            Sequential (List.map (oPopulate locals) exprs)
        | expr -> expr

    let rec optimize ((globals, locals) as env) = function
        | Sym id ->
            match localIndex id locals with
            | Some x -> Local x
            | None -> Global(internSymbol id globals)
        | AndExpr(left, right) ->
            Conditional(optimize env left, optimize env right, Atom False)
        | OrExpr(left, right) ->
            Conditional(optimize env left, Atom True, optimize env right)
        | IfExpr(condition, consequent, alternative) ->
            Conditional(optimize env condition, optimize env consequent, optimize env alternative)
        | CondExpr clauses ->
            let rec optimizeClauses = function
                | [] -> Atom Empty
                | (condition, consequent) :: rest ->
                    Conditional(optimize env condition, optimize env consequent, optimizeClauses rest)
            optimizeClauses clauses
        | LetExpr(param, value, body) ->
            let (x, locals) = localInsert param locals
            Binding(x, optimize env value, optimize (globals, locals) body)
        | LambdaExpr(param, body) ->
            let (x, locals) = localInsert param locals
            F1(x, optimize (globals, locals) body)
        | FreezeExpr body ->
            F0(optimize env body)
        | TrapExpr(body, handler) ->
            Catch(optimize env body, optimize env handler)
        | DoExpr _ as exprs ->
            Sequential (List.map (optimize env) (flattenDo exprs))
        | AppExpr(f, args) ->
            Application(optimize env f, List.map (optimize env) args)
        | value -> Atom value
