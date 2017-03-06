module Kl.Analysis

open System
open System.Collections.Generic
open Kl
open Values
open Extensions

let rec private butLast = function
    | [] | [_] -> []
    | x :: xs -> x :: butLast xs

let rec private flattenDo = function
    | DoExpr(first, second) -> flattenDo first @ flattenDo second
    | klExpr -> [klExpr]

let rec functionArity = function
    | Interpreted(paramz, _) -> List.length paramz
    | Compiled(arity, _) -> arity
    | Partial(f, args) -> functionArity f - args.Length

let nonPrimitiveSymbols (globals: Globals) =
    let ps (kv: KeyValuePair<_, _>) =
        if !kv.Value.IsProtected
            then None
            else Option.map (fun value -> (kv.Key, value)) !kv.Value.Val
    filterSome(Seq.toList(Seq.map ps globals))

let nonPrimitiveFunctions (globals: Globals) =
    let pf (kv: KeyValuePair<_, _>) =
        if !kv.Value.IsProtected
            then None
            else Option.map (fun f -> (kv.Key, f)) !kv.Value.Func
    filterSome(Seq.toList(Seq.map pf globals))

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
    | ConsExpr [Sym "let"; Sym param; binding; body] ->
        toCons [
            Sym "let"
            Sym param
            binding
            populate (Map.remove param locals) body]

    // Lambda param is new variable masking old one
    | ConsExpr [Sym "lambda"; Sym param; body] ->
        toCons [
            Sym "lambda"
            Sym param
            populate (Map.remove param locals) body]

    // Don't substitute special symbols: 'if, 'let, etc.
    | ConsExpr(Sym f :: args) when List.contains f specialSymbols ->
        toCons(Sym f :: (List.map (populate locals) args))

    | Cons(x, y) -> Cons(populate locals x, populate locals y)

    // Anything else just gets passed through
    | expr -> expr

let rec private removeAll keys m =
    match keys with
    | [] -> m
    | k :: ks -> removeAll ks (Map.remove k m)

let rec substitute locals expr =
    let proceed = substitute locals
    match expr with
    | Constant(Sym id) ->
        match Map.tryFind id locals with
        | Some value -> Constant value
        | None -> Constant(Sym id)
    | Conjunction(left, right) ->
        Conjunction(proceed left, proceed right)
    | Disjunction(left, right) ->
        Disjunction(proceed left, proceed right)
    | Conditional(condition, consequent, alternative) ->
        Conditional(proceed condition, proceed consequent, proceed alternative)
    | Binding(param, value, body) ->
        Binding(param, proceed value, substitute (Map.remove param locals) value)
    | Anonymous(Some param, body) ->
        Anonymous(Some param, substitute (Map.remove param locals) body)
    | Anonymous(None, body) ->
        Anonymous(None, proceed body)
    | Catch(body, handler) ->
        Catch(proceed body, proceed handler)
    | Sequential(exprs, last) ->
        Sequential(List.map proceed exprs, proceed last)
    | Definition(name, paramz, body) ->
        Definition(name, paramz, substitute (removeAll paramz locals) body)
    | Assignment(symbol, expr) ->
        Assignment(symbol, proceed expr)
    | GlobalCall(symbol, args) ->
        GlobalCall(symbol, List.map proceed args)
    | Application(f, args) ->
        Application(proceed f, List.map proceed args)
    | other -> other

// TODO: substitute local variables in Exprs
// TODO: locals can be function scoped since captured variables are substituted.

let rec parse ((globals, locals) as env) = function
    | ConsExpr [Sym "and"; left; right] ->
        Conjunction(parse env left, parse env right)
    | ConsExpr [Sym "or"; left; right] ->
        Disjunction(parse env left, parse env right)
    | ConsExpr [Sym "if"; condition; consequent; alternative] ->
        Conditional(parse env condition, parse env consequent, parse env alternative)
    | CondExpr clauses ->
        let rec parseClauses = function
            | [] -> 
                GlobalCall(intern globals "simple-error", [Constant(Str "No condition was true")])
            | (Sym "true", consequent) :: _ ->
                parse env consequent
            | (condition, consequent) :: rest ->
                Conditional(parse env condition, parse env consequent, parseClauses rest)
        parseClauses clauses
    | ConsExpr [Sym "let"; Sym param; value; body] ->
        Binding(param, parse env value, parse (globals, Set.add param locals) body)
    | ConsExpr [Sym "lambda"; Sym param; body] ->
        Anonymous(Some param, parse (globals, Set.add param locals) body)
    | ConsExpr [Sym "freeze"; body] ->
        Anonymous(None, parse env body)
    | ConsExpr [Sym "trap-error"; body; handler] ->
        Catch(parse env body, parse env handler)
    | DoExpr _ as expr ->
        let exprs = List.map (parse env) (flattenDo expr)
        Sequential(butLast exprs, List.last exprs)
    | DefunExpr(name, paramz, body) ->
        Definition(intern globals name, paramz, parse (globals, Set.union (Set.ofList paramz) locals) body)
    | ConsExpr [Sym "set"; Sym id; value] when not(Set.contains id locals) ->
        Assignment(intern globals id, parse env value)
    | ConsExpr [Sym "value"; Sym id] when not(Set.contains id locals) ->
        Retrieval(intern globals id)
    | ConsExpr(Sym id :: args) when not(Set.contains id locals) ->
        GlobalCall(intern globals id, List.map (parse env) args)
    | ConsExpr(f :: args) ->
        Application(parse env f, List.map (parse env) args)
    | value -> Constant value
