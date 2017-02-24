namespace Kl.Make

open System
open System.Collections.Generic
open Kl
open Kl.Values
open Kl.Analysis
open Reader
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

    let private param id typeName = id, shortType typeName

    let private appIgnore expr =
        infixIdExpr "op_PipeRight" expr (idExpr "ignore")

    let private appKl name args =
        parens (appIdExprN (rename name) [idExpr "globals"; listExpr args])

    let private toType targetType (fsExpr, currentType) =
        match currentType, targetType with
        | x, y when x = y -> fsExpr
        | FsBoolean, KlValue -> appIdExpr "Bool" fsExpr
        | FsUnit, KlValue -> sequentialExpr [fsExpr; idExpr "Empty"]
        | KlValue, FsBoolean -> appIdExpr "isTrue" fsExpr
        | _, FsUnit -> appIgnore fsExpr
        | _, _ -> failwithf "can't convert %O to %O" currentType targetType

    let rec private buildDo ((globals, locals) as context) expr =
        let flatExpr = List.map (build context) (flattenDo expr)
        let ignoreButLast i e =
            if i < List.length flatExpr - 1
                then e |> toType FsUnit
                else fst e
        sequentialExpr (List.mapi ignoreButLast flatExpr), snd (List.last flatExpr)

    and private buildApp ((globals, locals) as context) f args =
        match f with
        | Sym s ->
            if Set.contains s locals then
                // vapply globals ~(rename s) ~args
                appIdExprN "vapply"
                    [idExpr "globals"
                     idExpr(rename s)
                     listExpr args]
            else
                let (_, _, fref) = intern s globals
                match fref.Value with
                | Some systemf ->
                    let arity = functionArity systemf
                    if args.Length > arity then
                        // vapply globals ~(buildApp context f args0) args1
                        let (args0, args1) = List.splitAt arity args
                        appIdExprN "vapply"
                            [idExpr "globals"
                             buildApp context f args0
                             listExpr args1]
                    elif args.Length < arity then
                        // Func(Partial(Compiled(~arity, ~(rename s)), ~args))
                        appIdExpr "Func"
                            (appIdExpr "Partial"
                                (tupleExpr
                                    [appIdExpr "Compiled"
                                        (tupleExpr
                                            [intExpr arity
                                             idExpr (rename s)])
                                     listExpr args]))
                    else
                        // ~(rename s) globals ~args
                        appKl s args
                | None ->
                    // apply globals (lookup globals ~s) ~args
                    appIdExprN "apply"
                        [idExpr "globals"
                         parens
                            (appIdExprN "lookup"
                                [idExpr "globals"
                                 stringExpr s])
                         listExpr args]
        | f ->
            // vapply globals ~f ~args
            appIdExprN "vapply"
                [idExpr "globals"
                 build context f |> toType KlValue
                 listExpr args]

    and private build ((globals, locals) as context) = function
        | Empty -> idExpr "Empty", KlValue
        | Num x -> appIdExpr "Num" (decimalExpr x), KlValue
        | Str s -> appIdExpr "Str" (stringExpr s), KlValue
        | Sym "true" -> boolExpr true, FsBoolean
        | Sym "false" -> boolExpr false, FsBoolean
        | Sym s ->
            // ``kl_symbol-name`` OR Sym "symbol-name"
            if Set.contains s locals
                then idExpr (rename s), KlValue
                else appIdExpr "Sym" (stringExpr s), KlValue
        | AndExpr(left, right) ->
            infixIdExpr "op_BooleanAnd"
                (parens (build context left  |> toType FsBoolean))
                (parens (build context right |> toType FsBoolean)), FsBoolean
        | OrExpr(left, right) ->
            infixIdExpr "op_BooleanOr"
                 (parens (build context left  |> toType FsBoolean))
                 (parens (build context right |> toType FsBoolean)), FsBoolean
        | IfExpr(condition, consequent, alternative) ->
            ifExpr
                (build context condition   |> toType FsBoolean)
                (build context consequent  |> toType KlValue)
                (build context alternative |> toType KlValue), KlValue
        | CondExpr clauses ->
            let rec compileClauses = function
                | [] -> (appIdExpr "failwith" (stringExpr "No condition was true"))
                | (Sym "true", consequent) :: _ ->
                    build context consequent |> toType KlValue
                | (condition, consequent) :: rest ->
                    ifExpr
                        (build context condition  |> toType FsBoolean)
                        (build context consequent |> toType KlValue)
                        (compileClauses rest)
            compileClauses clauses, KlValue
        | LetExpr(param, binding, body) ->
            letExpr
                (rename param)
                (build context binding |> toType KlValue)
                (build (globals, Set.add param locals) body |> toType KlValue), KlValue
        | LambdaExpr(param, body) ->
            compileF globals locals [param] body, KlValue
        | FreezeExpr body ->
            compileF globals locals [] body, KlValue
        | TrapExpr(body, handler) ->
            let errExpr = appIdExpr "Err" (longIdExpr ["e"; "Message"])
            let handlerExpr =
                match handler with
                | LambdaExpr(param, body) ->
                    // let ~(rename param) = Err e.Message in ~body
                    (letExpr
                        (rename param)
                        errExpr
                        (build (globals, Set.add param locals) body |> toType KlValue))
                | f -> buildApp context f [errExpr]
            tryWithExpr (build context body |> toType KlValue) "e" handlerExpr, KlValue
        | DoExpr _ as expr -> buildDo context expr
        | DefunExpr _ -> failwith "Can't compile defun not at top level"
        | AppExpr(Sym "not", [x]) ->
            appIdExpr "not" (build context x |> toType FsBoolean), FsBoolean
        | AppExpr(Sym "=", [x; y]) ->
            let xExpr = build context x
            let yExpr = build context y
            let t = simplestCommonType (snd xExpr) (snd yExpr)
            infixIdExpr "op_Equality" (xExpr |> toType t) (yExpr |> toType t), FsBoolean
        | AppExpr(f, args) ->
            buildApp context f (List.map (build context >> toType KlValue) args), KlValue
        | klExpr -> failwithf "Unable to compile: %O" klExpr

    and private compileF globals locals paramz body =
        let arity = List.length paramz
        appIdExpr "Func"
            (appIdExpr "Compiled"
                (tupleExpr
                    [intExpr arity
                     (lambdaExpr
                         [param "globals" "Globals"]
                         (matchLambdaExpr
                             [matchClause
                                 (listPat(List.map (rename >> namePat) paramz))
                                 (build
                                     (globals, Set.union locals (Set.ofList paramz))
                                     body |> toType KlValue)
                              matchClause
                                 (namePat "args")
                                 (appIdExprN "argsErr"
                                     [stringExpr "Compiled Function" // TODO: better name
                                      listExpr(List.replicate arity (stringExpr "value"))
                                      idExpr "args"])]))]))

    let private compileDefun globals (name, f) =
        // and ~(rename name) (globals: Globals) = function
        //     | [~@(map rename paramz)] -> ~body
        //     | args -> argsErr ~name ~(replicate arity "value") args
        match f with
        | Interpreted(locals, paramz, body) ->
            if not(Map.isEmpty locals) then
                failwith "Top-level should not have locals when compiled"
            let arity = List.length paramz
            letBindingPrivate
                (rename name)
                [param "globals" "Globals"]
                (matchLambdaExpr
                    [matchClause
                        (listPat(List.map (rename >> namePat) paramz))
                        (build (globals, Set.ofList paramz) (unparse body) |> toType KlValue)
                     matchClause
                        (namePat "args")
                        (appIdExprN "argsErr"
                            [stringExpr name
                             listExpr(List.replicate arity (stringExpr "value"))
                             idExpr "args"])])
        | _ -> failwith "Can't compile functions other than interpreted defuns"

    let private installDefun (name, f) =
        // define globals ~name (Compiled(~argcount, ~(rename name)))
        appIdExprN "define"
            [idExpr "globals"
             stringExpr name
             (appIdExpr "Compiled"
                (tupleExpr
                    [intExpr(functionArity f)
                     idExpr(rename name)]))]

    let rec private buildValue (globals as context) = function
        | Empty -> idExpr "Empty"
        | Num x -> appIdExpr "Num" (decimalExpr x)
        | Str s -> appIdExpr "Str" (stringExpr s)
        | Sym s -> appIdExpr "Sym" (stringExpr s)
        | Cons(x, y) ->
            appIdExpr "Cons"
                (tupleExpr
                    [buildValue context x
                     buildValue context y])
        | Vec array ->
            appIdExpr "Vec"
                (arrayExpr(List.map (buildValue context) (Seq.toList array)))
        | Err s -> appIdExpr "Err" (stringExpr s)
        | Func(Interpreted(locals, paramz, body)) ->
            if not(Map.isEmpty locals) then
                failwith "Can't build non-empty Locals"
            compileF globals Set.empty paramz (unparse body)
        | value -> failwithf "Can't build value: %A" value

    let private installSymbol (globals as context) (name, (_, sref: Value option ref, _)) =
        match sref.Value with
        | Some value ->
            Some <|
                appIdExprN "assign"
                    [idExpr "globals"
                     stringExpr name
                     buildValue context value]
        | None -> None

    let rec private filterSome = function
        | [] -> []
        | Some x :: xs -> x :: filterSome xs
        | None :: xs -> filterSome xs

    let compile nameParts globals =
        let fn = sjoin "" nameParts
        let symbols = nonPrimitiveSymbols globals
        let defuns = nonPrimitiveFunctions globals
        let compiledNameAttr name =
            attr
                (longIdentWithDots ["CompiledName"])
                (stringExpr name)
        moduleFile nameParts
            [extnAttr]
            [openDecl ["Kl"]
             openDecl ["Kl"; "Values"]
             openDecl ["Kl"; "Evaluator"]
             openDecl ["Kl"; "Builtins"]
             openDecl ["Kl"; "Startup"]
             letMultiDecl(List.map (compileDefun globals) defuns)
             letAttrsDecl
                [compiledNameAttr "Install"]
                "install"
                [param "globals" "Globals"]
                (sequentialExpr
                    (List.concat
                        [List.map (installSymbol globals) symbols |> filterSome
                         List.map installDefun defuns
                         [idExpr "globals"]]))
             letUnitAttrsDecl
                [compiledNameAttr "NewRuntime"]
                "newRuntime"
                (appIdExpr "install" (appIdExpr "baseGlobals" unitExpr))
             letAttrsMultiParamDecl [extnAttr] "Eval"
                [param "globals" "Globals"; param "syntax" "string"]
                (appKl "eval"
                    [appKl "read"
                        [appIdExpr "pipeString" (idExpr "syntax")]])
             letAttrsMultiParamDecl [extnAttr] "Load"
                [param "globals" "Globals"; param "path" "string"]
                (appIgnore
                    (appKl "load"
                        [appIdExpr "Str" (idExpr "path")]))]
