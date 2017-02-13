namespace Kl.Import

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Kl
open Kl.Values
open Reader
open Analysis
open Syntax

module Compiler =

    // All symbols from KL code need to be renamed so
    // they don't conflict with generated identifiers
    // or F# keywords
    let private rename s = "kl_" + s

    type private ExprType =
        | KlValue
        | FsBoolean
        | FsUnit

    let private simplestCommonType t0 t1 = if t0 = t1 then t0 else KlValue

    let private toType fn targetType (fsExpr, currentType) =
        match currentType, targetType with
        | x, y when x = y -> fsExpr
        | FsBoolean, KlValue -> appIdExpr fn "Bool" fsExpr
        | FsUnit, KlValue -> idExpr fn "Empty"
        | KlValue, FsBoolean -> appIdExpr fn "isTrue" fsExpr
        | _, FsUnit -> infixIdExpr fn "op_PipeRight" fsExpr (idExpr fn "ignore")
        | _, _ -> failwithf "can't convert %O to %O" currentType targetType

    let rec private buildDo ((fn, globals, locals) as context) expr =
        let flatExpr = List.map (build context) (flattenDo expr)
        let ignoreButLast i e =
            if i < List.length flatExpr - 1
                then e |> toType fn FsUnit
                else fst e
        sequentialExpr fn (List.mapi ignoreButLast flatExpr), snd (List.last flatExpr)

    and private buildApp ((fn, globals, locals) as context) f args =
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
                        (appIdExprN fn "resolveGlobalFunction"
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
            infixIdExpr fn "op_BooleanAnd"
                (parens fn (build context left  |> toType fn FsBoolean))
                (parens fn (build context right |> toType fn FsBoolean)), FsBoolean
        | OrExpr(left, right) ->
            infixIdExpr fn "op_BooleanOr"
                 (parens fn (build context left  |> toType fn FsBoolean))
                 (parens fn (build context right |> toType fn FsBoolean)), FsBoolean
        | IfExpr(condition, consequent, alternative) ->
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
            letExpr fn
                (rename param)
                (build context binding |> toType fn KlValue)
                (build (fn, globals, Set.add param locals) body |> toType fn KlValue), KlValue
        | LambdaExpr(param, body) ->
            // Func(Lambda(CompiledLambda(fun (globals: Globals) (~param: Value) -> ~body)))
            nestedAppIdExpr fn ["Func"; "Lambda"; "CompiledLambda"]
                (lambdaExpr fn
                    ["globals",    shortType fn "Globals"
                     rename param, shortType fn "Value"]
                    (build (fn, globals, Set.add param locals) body |> toType fn KlValue)), KlValue
        | FreezeExpr body ->
            // Func(Freeze(CompiledFreeze(fun (globals: Globals) -> ~body)))
            nestedAppIdExpr fn ["Func"; "Freeze"; "CompiledFreeze"]
                (lambdaExpr fn
                    ["globals", shortType fn "Globals"]
                    (build context body |> toType fn KlValue)), KlValue
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
        | DoExpr _ as expr -> buildDo context expr
        | DefunExpr _ -> failwith "Can't compile defun not at top level"
        | AppExpr(Sym "not", [x]) ->
            appIdExpr fn "not" (build context x |> toType fn FsBoolean), FsBoolean
        | AppExpr(Sym "=", [x; y]) ->
            let xExpr = build context x
            let yExpr = build context y
            let t = simplestCommonType (snd xExpr) (snd yExpr)
            infixIdExpr fn "op_Equality" (xExpr |> toType fn t) (yExpr |> toType fn t), FsBoolean
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

    let private installDefun fn (name, f) =
        indexSetExpr fn
            (longIdExpr fn ["globals"; "Functions"])
            (stringExpr fn name)
            (appIdExpr fn "Defun"
                (tupleExpr fn
                    [stringExpr fn name
                     intExpr fn (functionArity f)
                     appIdExpr fn "CompiledDefun" (idExpr fn (rename name))]))

    let rec private buildValue ((fn, globals) as context) = function
        | Empty -> idExpr fn "Empty"
        | Num x -> appIdExpr fn "Dec" (decimalExpr fn x)
        | Str s -> appIdExpr fn "Str" (stringExpr fn s)
        | Sym s -> appIdExpr fn "Sym" (stringExpr fn s)
        | Cons(x, y) -> appIdExpr fn "Cons" (tupleExpr fn [buildValue context x; buildValue context y])
        | Vec array -> arrayExpr fn (List.map (buildValue context) (Seq.toList array))
        | Err s -> appIdExpr fn "Err" (stringExpr fn s)
        | Func(Lambda(InterpretedLambda(locals, param, body))) ->
            if not(Map.isEmpty locals) then
                failwith "Can't build non-empty Locals"
            nestedAppIdExpr fn ["Func"; "Lambda"; "CompiledLambda"]
                (lambdaExpr fn
                    ["globals",    shortType fn "Globals"
                     rename param, shortType fn "Value"]
                    (build (fn, globals, Set [param]) body |> toType fn KlValue))
        | value -> failwithf "Can't build value: %A" value

    let private installSymbol ((fn, globals) as context) (name, value) =
        indexSetExpr fn
            (longIdExpr fn ["globals"; "Symbols"])
            (stringExpr fn name)
            (buildValue context value)

    let compile fn globals =
        let symbols = nonPrimitiveSymbols globals
        let defuns = nonPrimitiveFunctions globals
        parsedFile fn [
            openDecl fn ["Kl"]
            openDecl fn ["Kl"; "Values"]
            openDecl fn ["Kl"; "Evaluator"]
            openDecl fn ["Kl"; "Builtins"]
            letMultiDecl fn (List.map (snd >> compileDefun fn globals) defuns)
            letDecl fn
                "installer"
                ["globals", shortType fn "Globals"]
                (sequentialExpr fn
                    (List.append
                        (List.map (installSymbol (fn, globals)) symbols)
                        (List.map (installDefun fn) defuns)))]
