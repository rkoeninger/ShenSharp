module Kl.Analysis

open System
open System.Collections.Generic
open Kl
open Values

let rec private flattenDo = function
    | Form(Sym "do" :: exprs) -> List.collect flattenDo exprs
    | expr -> [expr]

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

let rec parse ((globals, locals) as env) = function
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
        let exprs = List.map (parse env) (flattenDo expr)
        Sequential(butLast exprs, List.last exprs)
    | DefunForm(name, paramz, body) ->
        Definition(intern globals name, paramz, parse (globals, Set.union (Set.ofList paramz) locals) body)
    | Form [Sym "set"; Sym id; value] when not(Set.contains id locals) ->
        Assignment(intern globals id, parse env value)
    | Form [Sym "value"; Sym id] when not(Set.contains id locals) ->
        Retrieval(intern globals id)
    | Form(Sym id :: args) when not(Set.contains id locals) ->
        GlobalCall(intern globals id, List.map (parse env) args)
    | Form(f :: args) ->
        Application(parse env f, List.map (parse env) args)
    | value -> Constant value
