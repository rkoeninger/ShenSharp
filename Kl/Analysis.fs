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

    // TODO: do we need to do this for simple Value exprs, which then get optimized,
    //       or substitute the optimized expr?
    // TODO: locals can be function scoped since captured variables are substituted.

    // ValueExpr -> OptimizedExpr -> OptimizedSubsitutedExpr
//    let rec oPopulate locals = function
//        | Conditional(condition, consequent, alternative) ->
//            Conditional(
//                oPopulate locals condition,
//                oPopulate locals consequent,
//                oPopulate locals alternative)
//        | Binding(x, value, body) ->
//            Binding(
//                x,
//                oPopulate locals value,
//                oPopulate locals body)
//        | Sequential exprs ->
//            Sequential (List.map (oPopulate locals) exprs)
//        | expr -> expr
//
//    let rec optimize ((globals, locals) as env) = function
//        | Sym id ->
//            match localIndex id locals with
//            | Some x -> Local x
//            | None -> Atom(Sym id)
//        | AndExpr(left, right) ->
//            Conditional(optimize env left, optimize env right, Atom False)
//        | OrExpr(left, right) ->
//            Conditional(optimize env left, Atom True, optimize env right)
//        | IfExpr(condition, consequent, alternative) ->
//            Conditional(optimize env condition, optimize env consequent, optimize env alternative)
//        | CondExpr clauses ->
//            let rec optimizeClauses = function
//                | [] -> Atom Empty
//                | (condition, consequent) :: rest ->
//                    Conditional(optimize env condition, optimize env consequent, optimizeClauses rest)
//            optimizeClauses clauses
//        | LetExpr(param, value, body) ->
//            let (x, locals) = localInsert param locals
//            Binding(x, optimize env value, optimize (globals, locals) body)
//        | LambdaExpr(param, body) ->
//            let (x, locals) = localInsert param locals
//            F1(x, optimize (globals, locals) body)
//        | FreezeExpr body ->
//            F0(optimize env body)
//        | TrapExpr(body, handler) ->
//            Catch(optimize env body, optimize env handler)
//        | DoExpr _ as exprs ->
//            Sequential (List.map (optimize env) (flattenDo exprs))
//        | AppExpr(Sym id, args) when not(List.contains id locals) ->
//            GlobalApplication(internSymbol id globals, List.map (optimize env) args)
//        | AppExpr(f, args) ->
//            Application(optimize env f, List.map (optimize env) args)
//        | value -> Atom value

    let rec parse ((globals, locals) as env) = function
        | AndExpr(left, right) ->
            Conjunction(parse env left, parse env right)
        | OrExpr(left, right) ->
            Disjunction(parse env left, parse env right)
        | IfExpr(condition, consequent, alternative) ->
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
        | LetExpr(param, value, body) ->
            Binding(param, parse env value, parse (globals, Set.add param locals) body)
        | LambdaExpr(param, body) ->
            Anonymous(Some param, parse (globals, Set.add param locals) body)
        | FreezeExpr body ->
            Anonymous(None, parse env body)
        | TrapExpr(body, handler) ->
            Catch(parse env body, parse env handler)
        | DoExpr _ as expr ->
            Sequential(List.map (parse env) (flattenDo expr))
        | DefunExpr(name, paramz, body) ->
            Definition(intern name globals, paramz, parse (globals, Set.union (Set.ofList paramz) locals) body)
        | AppExpr(Sym "set", [Sym id; value]) when not(Set.contains id locals) ->
            Assignment(intern id globals, parse env value)
        | AppExpr(Sym "value", [Sym id]) when not(Set.contains id locals) ->
            Retrieval(intern id globals)
        | AppExpr(Sym id, args) when not(Set.contains id locals) ->
            GlobalCall(intern id globals, List.map (parse env) args)
        | AppExpr(f, args) ->
            Application(parse env f, List.map (parse env) args)
        | value -> Constant value

    let rec unparse = function
        | Constant value -> value
        | Conjunction(left, right) -> toCons [Sym "and"; unparse left; unparse right]
        | Disjunction(left, right) -> toCons [Sym "or"; unparse left; unparse right]
        | Conditional(condition, consequent, alternative) -> toCons [Sym "if"; unparse condition; unparse consequent; unparse alternative]
        | Binding(param, value, body) -> toCons [Sym "let"; Sym param; unparse value; unparse body]
        | Anonymous(Some param, body) -> toCons [Sym "lambda"; Sym param; unparse body]
        | Anonymous(None, body) -> toCons [Sym "freeze"; unparse body]
        | Catch(body, handler) -> toCons [Sym "trap-error"; unparse body; unparse handler]
        | Sequential exprs ->
            let rec expandDo = function
                | [] -> failwith "empty do"
                | [expr] -> unparse expr
                | expr :: exprs -> toCons [Sym "do"; unparse expr; expandDo exprs]
            expandDo exprs
        | Definition _ -> failwith "shouldn't be a definition here"
        | Assignment((id, _, _), expr) -> toCons [Sym "set"; Sym id; unparse expr]
        | Retrieval((id, _, _)) -> toCons [Sym "value"; Sym id]
        | GlobalCall((id, _, _), args) -> toCons(Sym id :: (List.map unparse args))
        | Application(f, args) -> toCons(unparse f :: (List.map unparse args))

