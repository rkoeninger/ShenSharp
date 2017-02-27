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
        | Interpreted(_, paramz, _) -> List.length paramz
        | Compiled(arity, _) -> arity
        | Partial(f, args) -> functionArity f - args.Length

    let nonPrimitiveSymbols globals =
        globals.Symbols
        |> Seq.map (fun (kv: KeyValuePair<_, _>) -> (kv.Key, kv.Value))
        |> Seq.filter (fst >> globals.PrimitiveSymbols.Contains >> not)
        |> Seq.toList

    let rec private filterSome = function
        | [] -> []
        | (y, Some x) :: xs -> (y, x) :: filterSome xs
        | _ :: xs -> filterSome xs

    let nonPrimitiveFunctions globals =
        globals.Symbols
        |> Seq.map (fun (kv: KeyValuePair<_, _>) -> (kv.Key, kv.Value))
        |> Seq.map (fun (id, (_, _, fref)) -> (id, fref.Value))
        |> Seq.toList
        |> filterSome
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
                    GlobalCall(intern "simple-error" globals, [Constant(Str "No condition was true")])
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
            Sequential(List.map (parse env) (flattenDo expr))
        | DefunExpr(name, paramz, body) ->
            Definition(intern name globals, paramz, parse (globals, Set.union (Set.ofList paramz) locals) body)
        | ConsExpr [Sym "set"; Sym id; value] when not(Set.contains id locals) ->
            Assignment(intern id globals, parse env value)
        | ConsExpr [Sym "value"; Sym id] when not(Set.contains id locals) ->
            Retrieval(intern id globals)
        | ConsExpr(Sym id :: args) when not(Set.contains id locals) ->
            GlobalCall(intern id globals, List.map (parse env) args)
        | ConsExpr(f :: args) ->
            Application(parse env f, List.map (parse env) args)
        | value -> Constant value
