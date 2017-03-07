﻿module internal Kl.Make.Compiler

open System
open System.Collections.Generic
open Kl
open Kl.Values
open Reader
open Syntax

// All symbols from KL code need to be renamed so
// they don't conflict with generated identifiers
// or F# keywords
let private rename = sprintf "kl_%s"

// TODO: add Function type here to do `Func`/`asFunction`
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
    | KlValue, FsBoolean -> appIdExpr "asBool" fsExpr
    | _, FsUnit -> appIgnore fsExpr
    | _, _ -> failwithf "can't convert %O to %O" currentType targetType

let rec private buildApp ((globals, locals) as context) (f: Expr) args =
    match f with
    | Constant(Sym s) ->
        if Set.contains s locals then
            // apply globals (asFunction ~(rename s)) ~args
            appIdExprN "apply"
                [idExpr "globals"
                 appIdExpr "asFunction" (idExpr(rename s))
                 listExpr args]
        else
            match !(intern globals s).Func with
            | Some systemf ->
                let arity = functionArity systemf
                if args.Length > arity then
                    // apply globals (asFunction ~(buildApp context f args0)) args1
                    let (args0, args1) = List.splitAt arity args
                    appIdExprN "apply"
                        [idExpr "globals"
                         appIdExpr "asFunction" (buildApp context f args0)
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
        // apply globals (asFunction ~f) ~args
        appIdExprN "apply"
            [idExpr "globals"
             appIdExpr "asFunction" (buildExpr context f |> toType KlValue)
             listExpr args]

and private buildExpr ((globals, locals) as context) (expr : Expr) =
    match expr with
    | Constant Empty -> idExpr "Empty", KlValue
    | Constant(Num x) -> appIdExpr "Num" (decimalExpr x), KlValue
    | Constant(Str s) -> appIdExpr "Str" (stringExpr s), KlValue
    | Constant(Bool b) -> boolExpr b, FsBoolean
    | Constant(Sym s) ->
        // ``kl_symbol-name`` OR Sym "symbol-name"
        if Set.contains s locals
            then idExpr (rename s), KlValue
            else appIdExpr "Sym" (stringExpr s), KlValue
    | Conjunction(left, right) ->
        infixIdExpr "op_BooleanAnd"
            (parens (buildExpr context left  |> toType FsBoolean))
            (parens (buildExpr context right |> toType FsBoolean)), FsBoolean
    | Disjunction(left, right) ->
        infixIdExpr "op_BooleanOr"
                (parens (buildExpr context left  |> toType FsBoolean))
                (parens (buildExpr context right |> toType FsBoolean)), FsBoolean
    | Conditional(condition, consequent, alternative) ->
        ifExpr
            (buildExpr context condition   |> toType FsBoolean)
            (buildExpr context consequent  |> toType KlValue)
            (buildExpr context alternative |> toType KlValue), KlValue
    | Binding(param, binding, body) ->
        letExpr
            (rename param)
            (buildExpr context binding |> toType KlValue)
            (buildExpr (globals, Set.add param locals) body |> toType KlValue), KlValue
    | Anonymous(param, body) ->
        compileF globals locals (Option.toList param) body, KlValue
    | Catch(body, handler) ->
        let errExpr = appIdExpr "Err" (longIdExpr ["e"; "Message"])
        let handlerExpr =
            match handler with
            | Anonymous(Some param, body) ->
                // let ~(rename param) = Err e.Message in ~body
                (letExpr
                    (rename param)
                    errExpr
                    (buildExpr (globals, Set.add param locals) body |> toType KlValue))
            | f -> buildApp context f [errExpr]
        tryWithExpr (buildExpr context body |> toType KlValue) "e" handlerExpr, KlValue
    | Sequential(exprs, last) ->
        let builtExprs = List.map (buildExpr context >> toType FsUnit) exprs
        let (lastExpr, lastType) = buildExpr context last
        sequentialExpr(builtExprs @ [lastExpr]), lastType
    | Definition _ ->
        failwith "Can't compile defun not at top level"
    | Assignment(symbol, expr) ->
        appIdExprN (rename "set")
            [idExpr "globals"
             listExpr
                [appIdExpr "Sym" (stringExpr symbol.Name)
                 buildExpr context expr |> toType KlValue]], KlValue
    | Retrieval symbol ->
        appIdExprN (rename "value")
            [idExpr "globals"
             listExpr [appIdExpr "Sym" (stringExpr symbol.Name)]], KlValue
    | GlobalCall({Name = "not"}, [expr]) ->
        appIdExpr "not" (buildExpr context expr |> toType FsBoolean), FsBoolean
    | GlobalCall({Name = "="}, [x; y]) ->
        let xExpr = buildExpr context x
        let yExpr = buildExpr context y
        let t = simplestCommonType (snd xExpr) (snd yExpr)
        infixIdExpr "op_Equality" (xExpr |> toType t) (yExpr |> toType t), FsBoolean
    | GlobalCall({Name = id}, args) ->
        buildApp context (Constant(Sym id)) (List.map (buildExpr context >> toType KlValue) args), KlValue
    | Application(f, args) ->
        buildApp context f (List.map (buildExpr context >> toType KlValue) args), KlValue
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
                            (buildExpr
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
    | Interpreted(paramz, body) ->
        let arity = List.length paramz
        letBinding
            (rename name)
            [param "globals" "Globals"]
            (matchLambdaExpr
                [matchClause
                    (listPat(List.map (rename >> namePat) paramz))
                    (buildExpr (globals, Set.ofList paramz) body |> toType KlValue)
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
    | Func(Interpreted(paramz, body)) ->
        compileF globals Set.empty paramz body
    | value -> failwithf "Can't build value: %A" value

let private installSymbol (globals as context) (id, value) =
    appIdExprN "assign" [
        idExpr "globals"
        stringExpr id
        buildValue context value]

let buildInstallationFile (name: string) globals =
    let symbols = nonPrimitiveSymbols globals
    let defuns = nonPrimitiveFunctions globals
    moduleFile (name.Split('.') |> Array.toList)
        [openDecl ["Kl"]
         openDecl ["Kl"; "Values"]
         openDecl ["Kl"; "Evaluator"]
         openDecl ["Kl"; "Builtins"]
         openDecl ["Kl"; "Startup"]
         letMultiDecl(List.map (compileDefun globals) defuns)
         letDecl
            "install"
            [param "globals" "Globals"]
            (sequentialExpr
                (List.concat
                    [List.map (installSymbol globals) symbols
                     List.map installDefun defuns
                     [idExpr "globals"]]))]

let buildMetadataFile name =
    moduleFile ["ShenSharp"; "Metadata"]
        [openDecl ["System"; "Reflection"]
         openDecl ["System"; "Runtime"; "Versioning"]
         assemblyAttrDecl
            (longIdentWithDots ["AssemblyTitle"])
            (stringExpr name)
         assemblyAttrDecl
            (longIdentWithDots ["TargetFramework"])
            (stringExpr ".NETFramework,Version=v4.5")]
