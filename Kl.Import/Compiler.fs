﻿namespace Kl.Import

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

    let private buildKlLambda fn lExpr =
        appIdExpr fn "Func"
            (appIdExpr fn "Lambda"
                (appIdExpr fn "CompiledLambda"
                    (parens fn lExpr)))

    let private buildKlFreeze fn fExpr =
        appIdExpr fn "Func"
            (appIdExpr fn "Freeze"
                (appIdExpr fn "CompiledFreeze"
                    (parens fn fExpr)))

    type private ExprType =
        | Bottom
        | KlValue
        | FsBoolean
        | FsUnit

    // TODO: needs application context to know for which function there
    //       is an argument type error
    // TODO: Values.isTrue needs to take context name
    let private convert targetType (fsExpr, currentType) =
        let fn = "file" // TODO: pass filename in
        match currentType, targetType with
        | x, y when x = y -> fsExpr
        | Bottom, _ -> fsExpr
        | FsBoolean, KlValue -> appIdExpr fn "Bool" fsExpr
        | FsUnit, KlValue -> idExpr fn "Empty"
        | KlValue, FsBoolean -> appIdExpr fn "isTrue" fsExpr
        | _, FsUnit -> infixIdExpr fn "op_PipeRight" fsExpr (idExpr fn "ignore")
        | _, _ -> failwithf "can't convert %O to %O" currentType targetType

    let private (>@) fsExprWithType targetType = convert targetType fsExprWithType

    let rec private flattenDo = function
        | DoExpr(first, second) -> flattenDo first @ flattenDo second
        | klExpr -> [klExpr]

    let rec private build ((fn, globals, locals) as context) = function
        | Empty -> idExpr fn "Empty", KlValue
        | Num x -> appIdExpr fn "Num" (decimalExpr fn x), KlValue
        | Str s -> appIdExpr fn "Str" (stringExpr fn s), KlValue
        | Sym "true" -> boolExpr fn true, FsBoolean
        | Sym "false" -> boolExpr fn false, FsBoolean
        | Sym s ->
            if Set.contains s locals
                then idExpr fn (rename s), KlValue
                else appIdExpr fn "Sym" (stringExpr fn s), KlValue
        | AndExpr(left, right) ->
            infixIdExpr fn
                "op_BooleanAnd"
                (parens fn (build context left  >@ FsBoolean))
                (parens fn (build context right >@ FsBoolean)), FsBoolean
        | OrExpr(left, right) ->
            infixIdExpr fn
                 "op_BooleanOr"
                 (parens fn (build context left  >@ FsBoolean))
                 (parens fn (build context right >@ FsBoolean)), FsBoolean
        | IfExpr(condition, consequent, alternative) ->
            ifExpr fn
                (build context condition   >@ FsBoolean)
                (build context consequent  >@ KlValue)
                (build context alternative >@ KlValue), KlValue
        | CondExpr clauses ->
            let rec compileClauses = function
                | [] -> idExpr fn "Empty"
                | (Sym "true", consequent) :: _ ->
                    build context consequent >@ KlValue
                | (condition, consequent) :: rest ->
                    ifExpr fn
                        (build context condition  >@ FsBoolean)
                        (build context consequent >@ KlValue)
                        (compileClauses rest)
            compileClauses clauses, KlValue
        | LetExpr(param, binding, body) ->
            letExpr fn
                (rename param)
                (build context binding >@ KlValue)
                (build (fn, globals, Set.add param locals) body >@ KlValue), KlValue
        | DoExpr _ as expr ->
            let flatExpr = List.map (build context) (flattenDo expr)
            let ignoreButLast i e =
                if i < List.length flatExpr - 1
                    then e >@ FsUnit
                    else fst e
            let ignoredExpr = List.mapi ignoreButLast flatExpr
            sequentialExpr fn ignoredExpr, snd (List.last flatExpr)
        | LambdaExpr(param, body) ->
            buildKlLambda fn
                (lambdaExpr fn
                    ["globals", shortType fn "Globals"; rename param, shortType fn "Value"]
                    (build (fn, globals, Set.add param locals)
                        body >@ KlValue)), KlValue
        | FreezeExpr body ->
            buildKlFreeze fn
                (lambdaExpr fn
                    ["globals", shortType fn "Globals"]
                    (build context body >@ KlValue)), KlValue
        | TrapExpr(body, LambdaExpr(param, handler)) ->
            tryWithExpr fn
                (build context body >@ KlValue)
                "e"
                (letExpr fn
                    (rename param)
                    (appIdExpr fn "Err" (longIdExpr fn ["e"; "Message"]))
                    (build (fn, globals, Set.add param locals) handler >@ KlValue)), KlValue
        | TrapExpr(body, Sym handler) ->
            tryWithExpr fn
                (build context body >@ KlValue)
                "e"
                (appExpr fn
                    (appExpr fn
                        (idExpr fn (rename handler))
                        (idExpr fn "globals"))
                    (listExpr fn [appIdExpr fn "Err" (longIdExpr fn ["e"; "Message"])])), KlValue
        | TrapExpr(body, handler) -> failwith "can't compile"
            // need (Evaluator.apply: Globals -> Function -> Value list -> Value) for this?
        | DefunExpr _ -> failwith "Can't compile defun not at top level"
        | AppExpr(Sym s, args) ->
            appExpr fn
                (appExpr fn
                    (idExpr fn (rename s)) (idExpr fn "globals"))
                    (listExpr fn (List.map (build context >> convert KlValue) args)), KlValue

        // TODO: if it's some other expression, we need Evaluator.apply
        | AppExpr(f, args) -> failwith "can't compile"

        | _ -> failwith "Unable to compile"

    let buildExpr context klExpr = build context klExpr |> fst

    // TODO: use Builtins.argsErr to get the same error messages
    let compileDefun fn globals name paramz body =
        letBinding fn
            (rename name)
            ["globals", shortType fn "Globals"
             "args", listType fn (shortType fn "Value")]
            (build (fn, globals, Set.ofList paramz) body >@ KlValue)

    let compile fn globals =
        parsedFile fn [
            openDecl fn ["Kl"]
            openDecl fn ["Kl"; "Values"]
            openDecl fn ["Kl"; "Builtins"]
            letMultiDecl fn [
                letBinding fn
                    (rename "lambda-body")
                    ["globals", shortType fn "Globals"
                     rename "X", shortType fn "Value"]
                    (build (fn, globals, Set.singleton "X")
                        (read "(lambda Y (+ X Y))") >@ KlValue)
                letBinding fn
                    (rename "trap-error-lambda")
                    ["globals", shortType fn "Globals"
                     "args", listType fn (shortType fn "Value")]
                    (build (fn, globals, Set.ofList ["X"; "Y"])
                        (read "(trap-error (cn X Y) (lambda E (error-to-string E)))") >@ KlValue)
                letBinding fn
                    (rename "trap-error-sym")
                    ["globals", shortType fn "Globals"
                     "args", listType fn (shortType fn "Value")]
                    (build (fn, globals, Set.ofList ["X"; "Y"])
                        (read "(trap-error (cn X Y) error-to-string)") >@ KlValue)
                letBinding fn
                    (rename "nested-do")
                    ["globals", shortType fn "Globals"]
                    (build (fn, globals, Set.empty)
                        (read "(do (do X Y) (do Z Q))") >@ KlValue)
                letBinding fn
                    (rename "count-down")
                    ["globals", shortType fn "Globals"
                     "args", listType fn (shortType fn "Value")]
                    (build (fn, globals, Set.empty)
                        (read "(if (= 0 X) true (count-down (- X 1)))") >@ KlValue)]]