namespace Kl.Import

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
        | KlValue
        | FsBoolean
        | FsUnit

    let private convert targetType (fsExpr, currentType) =
        let fn = "file" // TODO: pass filename in
        match currentType, targetType with
        | x, y when x = y -> fsExpr
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
            // ``kl_symbol-name`` OR Sym "symbol-name"
            if Set.contains s locals
                then idExpr fn (rename s), KlValue
                else appIdExpr fn "Sym" (stringExpr fn s), KlValue
        | AndExpr(left, right) ->
            // ~left && ~right
            infixIdExpr fn
                "op_BooleanAnd"
                (parens fn (build context left  >@ FsBoolean))
                (parens fn (build context right >@ FsBoolean)), FsBoolean
        | OrExpr(left, right) ->
            // ~left || ~right
            infixIdExpr fn
                 "op_BooleanOr"
                 (parens fn (build context left  >@ FsBoolean))
                 (parens fn (build context right >@ FsBoolean)), FsBoolean
        | IfExpr(condition, consequent, alternative) ->
            // if ~condition then ~consequent else ~alternative
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
            // let ~param = ~binding in ~body
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
            // Func(Lambda(CompiledLambda(fun (globals: Globals) (~param: Value) -> ~body)))
            buildKlLambda fn
                (lambdaExpr fn
                    ["globals", shortType fn "Globals"; rename param, shortType fn "Value"]
                    (build (fn, globals, Set.add param locals)
                        body >@ KlValue)), KlValue
        | FreezeExpr body ->
            // Func(Freeze(CompiledFreeze(fun (globals: Globals) -> ~body)))
            buildKlFreeze fn
                (lambdaExpr fn
                    ["globals", shortType fn "Globals"]
                    (build context body >@ KlValue)), KlValue
        | TrapExpr(body, handler) ->
            let errExpr = appIdExpr fn "Err" (longIdExpr fn ["e"; "Message"])
            let handlerExpr =
                match handler with
                | LambdaExpr(param, f) ->
                    // e -> let ~(rename param) = Err e.Message in ~fExpr
                    (letExpr fn
                        (rename param)
                        errExpr
                        (build (fn, globals, Set.add param locals) f >@ KlValue))
                | Sym s ->
                    if globals.Functions.ContainsKey s then
                        // apply ~(rename s) globals [~errExpr]
                        appIdExprN fn
                            (rename s)
                            [idExpr fn "globals"
                             (listExpr fn [errExpr])]
                    else
                        // apply globals (resolveGlobalFunction globals ~s) [~errExpr]
                        appIdExprN fn
                            "apply"
                            [idExpr fn "globals"
                             (parens fn
                                (appExprN fn
                                    (idExpr fn "resolveGlobalFunction")
                                    [idExpr fn "globals"
                                     (stringExpr fn s)]))
                             listExpr fn [errExpr]]
                | f ->
                    // vapply globals ~fExpr [~errExpr]
                    appIdExprN fn
                        "vapply"
                        [idExpr fn "globals"
                         (build context f >@ KlValue)
                         listExpr fn [errExpr]]
            tryWithExpr fn (build context body >@ KlValue) "e" handlerExpr, KlValue
        | DefunExpr _ -> failwith "Can't compile defun not at top level"
        | AppExpr(Sym "not", [arg]) ->
            appIdExpr fn "not" (build context arg >@ FsBoolean), FsBoolean
        | AppExpr(Sym s, args) ->
            let argsExpr = listExpr fn (List.map (build context >> convert KlValue) args)
            if globals.Functions.ContainsKey s then
                // ~(rename s) globals ~argsExpr
                appIdExprN fn (rename s) [idExpr fn "globals"; argsExpr], KlValue
            else
                // apply globals (resolveGlobalFunction globals ~s) ~argsExpr
                appIdExprN fn
                    "apply"
                    [idExpr fn "globals"
                     (parens fn
                        (appIdExprN fn
                            "resolveGlobalFunction"
                            [idExpr fn "globals"
                             (stringExpr fn s)]))
                     argsExpr], KlValue
        | AppExpr(f, args) ->
            // vapply globals ~fExpr ~argsExpr
            appIdExprN fn
                "vapply"
                [idExpr fn "globals"
                 build context f >@ KlValue
                 listExpr fn (List.map (build context >> convert KlValue) args)], KlValue
        | klExpr -> failwithf "Unable to compile: %O" klExpr

    let private compileDefun fn globals name paramz body =
        // and ~(rename name) (globals : Globals) =
        //     function
        //     | [~@(map rename paramz)] -> ~body
        //     | args -> argsErr ~name ~(replicate args.Length "value") args
        letBinding fn
            (rename name)
            ["globals", shortType fn "Globals"]
            (matchLambdaExpr fn
                [matchClause fn
                    (listPat fn (List.map (rename >> namePat fn) paramz))
                    (build (fn, globals, Set.ofList paramz) body >@ KlValue)
                 matchClause fn
                    (namePat fn "args")
                    (appExpr fn
                        (appExpr fn
                            (appIdExpr fn "argsErr" (stringExpr fn name))
                            (listExpr fn
                                (List.replicate
                                    (List.length paramz)
                                    (stringExpr fn "value"))))
                        (idExpr fn "args"))])

    let compile fn globals =
        parsedFile fn [
            openDecl fn ["Kl"]
            openDecl fn ["Kl"; "Values"]
            openDecl fn ["Kl"; "Evaluator"]
            openDecl fn ["Kl"; "Builtins"]
            // TODO: refer to Evaluator.resolveGlobalFunction or move it elsewhere and refer
            letDecl fn
                "resolveGlobalFunction"
                ["globals", shortType fn "Globals"
                 "id",      shortType fn "string"]
                (matchExpr fn
                    (appExpr fn
                        (longIdExpr fn ["globals"; "Functions"; "GetMaybe"])
                        (idExpr fn "id"))
                    [matchClause fn
                        (caseIdPat fn "Func" [namePat fn "f"])
                        (idExpr fn "f")
                     matchClause fn
                        (wildPat fn)
                        (appExpr fn
                            (appIdExpr fn "failwithf" (stringExpr fn "Function not defined: %s"))
                            (idExpr fn "id"))])
            // TODO: put this in Kl.dll somewhere and refer?
            letDecl fn
                "vapply"
                ["globals", shortType fn "Globals"
                 "f",       shortType fn "Value"
                 "args",    listType fn (shortType fn "Value")]
                (matchExpr fn
                    (idExpr fn "f")
                    [matchClause fn
                        (caseIdPat fn "Func" [namePat fn "f"])
                        (appExpr fn
                            (appExpr fn
                                (appIdExpr fn "apply" (idExpr fn "globals"))
                                (idExpr fn "f"))
                            (idExpr fn "args"))
                     matchClause fn
                        (wildPat fn)
                        (unitExpr fn)])
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
                        (read "(if (= 0 X) true (count-down (- X 1)))") >@ KlValue)
                compileDefun fn
                    globals
                    "defun-xor"
                    ["X"; "Y"]
                    (read "(and (not (and X Y)) (or X Y))")]]
