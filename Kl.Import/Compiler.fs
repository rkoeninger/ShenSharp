namespace Kl.Import

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Kl
open Kl.Values
open Kl.Import.Reader
open Kl.Import.Syntax

module Compiler =

    // All symbols from KL code need to be renamed so
    // they don't conflict with generated identifiers
    // or F# keywords
    let private rename s = "kl_" + s

    type private ExprType =
        | KlValue
        | FsBoolean
        | FsUnit

    let private toType fn targetType (fsExpr, currentType) =
        match currentType, targetType with
        | x, y when x = y -> fsExpr
        | FsBoolean, KlValue -> appIdExpr fn "Bool" fsExpr
        | FsUnit, KlValue -> idExpr fn "Empty"
        | KlValue, FsBoolean -> appIdExpr fn "isTrue" fsExpr
        | _, FsUnit -> infixIdExpr fn "op_PipeRight" fsExpr (idExpr fn "ignore")
        | _, _ -> failwithf "can't convert %O to %O" currentType targetType

    let rec private flattenDo = function
        | DoExpr(first, second) -> flattenDo first @ flattenDo second
        | klExpr -> [klExpr]

    let rec private buildApp ((fn, globals, locals) as context) f args =
        match f with
        | Sym s ->
            if globals.Functions.ContainsKey s then
                // ~(rename s) globals ~args
                appIdExprN fn (rename s) [idExpr fn "globals"; listExpr fn args]
            else
                // apply globals (resolveGlobalFunction ~s) ~args
                appIdExprN fn "apply"
                    [idExpr fn "globals"
                     parens fn
                        (appIdExprN fn
                            "resolveGlobalFunction"
                            [idExpr fn "globals"
                             stringExpr fn s])
                     listExpr fn args]
        | f ->
            // vapply globals ~f ~args
            appIdExprN fn "vapply"
                [idExpr fn "globals"
                 build context f |> toType fn KlValue
                 listExpr fn args]

    and private build ((fn, globals, locals) as context) = function
        | Empty -> idExpr fn "Empty", KlValue
        | Num x -> appIdExpr fn "Num" (decimalExpr fn x), KlValue
        | Str s -> appIdExpr fn "Str" (stringExpr fn s), KlValue
        | Sym "true" -> boolExpr fn true, FsBoolean
        | Sym "false" -> boolExpr fn false, FsBoolean
        | Sym s ->
            // ``kl_symbol-name`` OR Sym "symbol-name"
            if Set.contains s locals
                then idExpr fn (rename s), KlValue
                else appIdExpr fn "Sym" (stringExpr fn s), KlValue
        | AndExpr(left, right) ->
            // ~left && ~right
            infixIdExpr fn "op_BooleanAnd"
                (parens fn (build context left  |> toType fn FsBoolean))
                (parens fn (build context right |> toType fn FsBoolean)), FsBoolean
        | OrExpr(left, right) ->
            // ~left || ~right
            infixIdExpr fn "op_BooleanOr"
                 (parens fn (build context left  |> toType fn FsBoolean))
                 (parens fn (build context right |> toType fn FsBoolean)), FsBoolean
        | IfExpr(condition, consequent, alternative) ->
            // if ~condition then ~consequent else ~alternative
            ifExpr fn
                (build context condition   |> toType fn FsBoolean)
                (build context consequent  |> toType fn KlValue)
                (build context alternative |> toType fn KlValue), KlValue
        | CondExpr clauses ->
            let rec compileClauses = function
                | [] -> idExpr fn "Empty"
                | (Sym "true", consequent) :: _ ->
                    build context consequent |> toType fn KlValue
                | (condition, consequent) :: rest ->
                    ifExpr fn
                        (build context condition  |> toType fn FsBoolean)
                        (build context consequent |> toType fn KlValue)
                        (compileClauses rest)
            compileClauses clauses, KlValue
        | LetExpr(param, binding, body) ->
            // let ~param = ~binding in ~body
            letExpr fn
                (rename param)
                (build context binding |> toType fn KlValue)
                (build (fn, globals, Set.add param locals) body |> toType fn KlValue), KlValue
        | DoExpr _ as expr ->
            let flatExpr = List.map (build context) (flattenDo expr)
            let ignoreButLast i e =
                if i < List.length flatExpr - 1
                    then e |> toType fn FsUnit
                    else fst e
            sequentialExpr fn (List.mapi ignoreButLast flatExpr), snd (List.last flatExpr)
        | LambdaExpr(param, body) ->
            // Func(Lambda(CompiledLambda(fun (globals: Globals) (~param: Value) -> ~body)))
            parens fn
                (appIdExpr fn "Func"
                    (parens fn
                        (appIdExpr fn "Lambda"
                            (parens fn
                                (appIdExpr fn "CompiledLambda"
                                    (parens fn 
                                        (lambdaExpr fn
                                            ["globals",    shortType fn "Globals"
                                             rename param, shortType fn "Value"]
                                            (build (fn, globals, Set.add param locals)
                                                body |> toType fn KlValue)))))))), KlValue
        | FreezeExpr body ->
            // Func(Freeze(CompiledFreeze(fun (globals: Globals) -> ~body)))
            parens fn
                (appIdExpr fn "Func"
                    (parens fn
                        (appIdExpr fn "Freeze"
                            (parens fn
                                (appIdExpr fn "CompiledFreeze"
                                    (parens fn 
                                        (lambdaExpr fn
                                            ["globals", shortType fn "Globals"]
                                            (build context body |> toType fn KlValue)))))))), KlValue
        | TrapExpr(body, handler) ->
            let errExpr = appIdExpr fn "Err" (longIdExpr fn ["e"; "Message"])
            let handlerExpr =
                match handler with
                | LambdaExpr(param, body) ->
                    // let ~(rename param) = Err e.Message in ~body
                    (letExpr fn
                        (rename param)
                        errExpr
                        (build (fn, globals, Set.add param locals) body |> toType fn KlValue))
                | f -> buildApp context f [errExpr]
            tryWithExpr fn (build context body |> toType fn KlValue) "e" handlerExpr, KlValue
        | DefunExpr _ -> failwith "Can't compile defun not at top level"
        | AppExpr(Sym "not", [x]) ->
            appIdExpr fn "not" (build context x |> toType fn FsBoolean), FsBoolean
        | AppExpr(Sym "=", [x; y]) ->
            // TODO: don't coerce to KlValue is they're already the same type
            infixIdExpr fn "op_Equality"
                (build context x |> toType fn KlValue)
                (build context y |> toType fn KlValue), FsBoolean
        | AppExpr(f, args) ->
            buildApp context f (List.map (build context >> toType fn KlValue) args), KlValue
        | klExpr -> failwithf "Unable to compile: %O" klExpr

    let private compileDefun fn globals = function
        // and ~(rename name) (globals: Globals) = function
        //     | [~@(map rename paramz)] -> ~body
        //     | args -> argsErr ~name ~(replicate arity "value") args
        | Defun(name, arity, InterpretedDefun(paramz, body)) ->
            letBinding fn
                (rename name)
                ["globals", shortType fn "Globals"]
                (matchLambdaExpr fn
                    [matchClause fn
                        (listPat fn (List.map (rename >> namePat fn) paramz))
                        (build (fn, globals, Set.ofList paramz) body |> toType fn KlValue)
                     matchClause fn
                        (namePat fn "args")
                        (appIdExprN fn "argsErr"
                            [stringExpr fn name
                             listExpr fn (List.replicate arity (stringExpr fn "value"))
                             idExpr fn "args"])])
        | _ -> failwith "Can't compile functions other than interpreted defuns"

    let rec private defunArity = function
        | Defun(_, arity, _) -> arity
        | Lambda _ -> 1
        | Freeze _ -> 0
        | Partial(f, _) -> defunArity f

    let private installDefun fn (name, f) =
        indexSetExpr fn
            (longIdExpr fn ["globals"; "Functions"])
            (stringExpr fn name)
            (appIdExpr fn "Defun"
                (tupleExpr fn
                    [stringExpr fn name
                     intExpr fn (defunArity f)
                     appIdExpr fn "CompiledDefun" (idExpr fn (rename name))]))

    let compile fn globals =
        let nonPrimitives =
            globals.Functions
            |> Seq.map (fun (kv: KeyValuePair<_, _>) -> (kv.Key, kv.Value))
            |> Seq.filter (fst >> globals.Primitives.Contains >> not)
        parsedFile fn [
            openDecl fn ["Kl"]
            openDecl fn ["Kl"; "Values"]
            openDecl fn ["Kl"; "Evaluator"]
            openDecl fn ["Kl"; "Builtins"]
            letMultiDecl fn (Seq.toList(Seq.map (snd >> compileDefun fn globals) nonPrimitives))
            letDecl fn
                "installer"
                ["globals", shortType fn "Globals"]
                (sequentialExpr fn (Seq.toList(Seq.map (installDefun fn) nonPrimitives)))]
