module internal Kl.Make.Compiler

open Kl
open Kl.Values
open Syntax

// All symbols from KL code need to be renamed so
// they don't conflict with generated identifiers
// or F# keywords
let private rename = sprintf "kl_%s"

type private ExprType =
    | KlValue
    | KlFunction
    | FsBoolean
    | FsUnit

let private simplestCommonType t0 t1 = if t0 = t1 then t0 else KlValue

let private appIgnore expr =
    // ~expr |> ignore
    infixIdExpr "op_PipeRight" expr (idExpr "ignore")

let private idKl name = idExpr(rename name), KlValue

let private toType targetType (fsExpr, currentType) =
    match currentType, targetType with
    | x, y when x = y -> fsExpr
    | FsBoolean, KlValue -> appIdExpr "Bool" fsExpr
    | KlFunction, KlValue -> appIdExpr "Func" fsExpr
    | FsUnit, KlValue -> sequentialExpr [fsExpr; idExpr "Empty"]
    | KlValue, FsBoolean -> appIdExpr "asBool" fsExpr
    | KlValue, KlFunction -> appIdExpr "asFunction" fsExpr
    | _, FsUnit -> appIgnore fsExpr
    | _, _ -> failwithf "can't convert %O to %O" currentType targetType

let rec private gatherDoExprs = function
    | Form [Sym "do"; x; y] -> gatherDoExprs x @ gatherDoExprs y
    | x -> [x]

let rec private gatherConsElements = function
    | Form [Sym "cons"; x; y] -> let xs, t = gatherConsElements y in x :: xs, t
    | Empty -> [], None
    | x -> [], Some x

let private appApply f args = appIdExprN "apply" [idExpr "globals"; f; listExpr args]

let rec private buildApp (name, locals) f args =
    match f with
    // apply globals (asFunction ~(rename s)) ~args
    | Sym s when Set.contains s locals ->
        appApply (idKl s |> toType KlFunction) args
    // apply globals (lookup globals ~s) ~args
    | Sym s ->
        appExprN (idKl s |> fst) [idExpr "globals"; listExpr args]
    // apply globals (asFunction ~f) ~args
    | _ ->
        appApply (buildExpr (name, locals) f |> toType KlFunction) args

and private buildExpr ((name, locals) as context) = function
    | Empty -> idExpr "Empty", KlValue
    | Num x -> appIdExpr "Num" (decimalExpr x), KlValue
    | Str s -> appIdExpr "Str" (stringExpr s), KlValue
    | Bool b -> boolExpr b, FsBoolean
    | Sym s ->
        if Set.contains s locals
            // ``kl_symbol-name``
            then idKl s
            // Sym "symbol-name"
            else appIdExpr "Sym" (stringExpr s), KlValue
    | CondForm clauses ->
        let buildClause (condition, consequent) rest =
            match condition with
            | Bool true -> consequent
            | Bool false -> rest
            | _ -> toCons [Sym "if"; condition; consequent; rest]
        toCons [Sym "simple-error"; Str "No condition was true"]
        |> List.foldBack buildClause clauses
        |> buildExpr context
    | Form [Sym "and"; left; right] ->
        infixIdExpr "op_BooleanAnd"
            (parens (buildExpr context left  |> toType FsBoolean))
            (parens (buildExpr context right |> toType FsBoolean)), FsBoolean
    | Form [Sym "or"; left; right] ->
        infixIdExpr "op_BooleanOr"
            (parens (buildExpr context left  |> toType FsBoolean))
            (parens (buildExpr context right |> toType FsBoolean)), FsBoolean
    | Form [Sym "if"; condition; consequent; alternative] ->
        ifExpr
            (buildExpr context condition   |> toType FsBoolean)
            (buildExpr context consequent  |> toType KlValue)
            (buildExpr context alternative |> toType KlValue), KlValue
    | Form [Sym "let"; Sym param; binding; body] ->
        letExpr
            (rename param)
            (buildExpr context binding |> toType KlValue)
            (buildExpr (name, Set.add param locals) body |> toType KlValue), KlValue
    | Form [Sym "lambda"; Sym param; body] ->
        buildFunction name locals [param] body, KlValue
    | Form [Sym "freeze"; body] ->
        buildFunction name locals [] body, KlValue
    | Form [Sym "trap-error"; body; handler] ->
        let errExpr = appIdExpr "Err" (longIdExpr ["e"; "Message"])
        let handlerExpr =
            match handler with
            | Form [Sym "lambda"; Sym param; body] ->
                // let ~(rename param) = Err e.Message in ~body
                letExpr
                    (rename param)
                    errExpr
                    (buildExpr (name, Set.add param locals) body |> toType KlValue)
            | _ ->
                buildApp context handler [errExpr]
        tryWithExpr (buildExpr context body |> toType KlValue) "e" handlerExpr, KlValue
    | Form [Sym "do"; _; _] as doExpr ->
        let exprs = gatherDoExprs doExpr
        let builtExprs = List.map (buildExpr context) exprs
        let mostExprs = most builtExprs |> List.map (toType FsUnit)
        let (lastExpr, lastType) = List.last builtExprs
        sequentialExpr (mostExprs @ [lastExpr]), lastType
    | Form [Sym "defun"; _; _; _] ->
        failwith "Can't compile defun not at top level"
    | Form [Sym "not"; arg] ->
        appIdExpr "not" (buildExpr context arg |> toType FsBoolean), FsBoolean
    | Form [Sym "="; left; right] ->
        let leftExpr = buildExpr context left
        let rightExpr = buildExpr context right
        let t = simplestCommonType (snd leftExpr) (snd rightExpr)
        infixIdExpr "op_Equality" (leftExpr |> toType t) (rightExpr |> toType t), FsBoolean
    | Form [Sym "cons"; _; _] as consExpr ->
        match gatherConsElements consExpr with
        | xs, Some t ->
            // toConsWithTail ~t [~@xs]
            appIdExprN "toConsWithTail"
                [
                    buildExpr context t |> toType KlValue
                    listExpr (List.map (buildExpr context >> toType KlValue) xs)
                ], KlValue
        | xs, _ ->
            // toCons [~@xs]
            appIdExpr "toCons" (listExpr (List.map (buildExpr context >> toType KlValue) xs)), KlValue
    | Form(f :: args) ->
        buildApp context f (List.map (buildExpr context >> toType KlValue) args), KlValue
    | expr ->
        failwithf "Unable to compile: %O" expr

and private buildFunction name locals paramz body =
    let arity = List.length paramz
    // Compiled(
    //      ~arity,
    //      fun globals -> function
    //          | [~@(map rename paramz)] -> ~body
    //          | args -> argsErr ("Lambda@" + ~name) ~(replicate arity "value") args)
    ((appIdExpr "Compiled"
        (tupleExpr
            [
                intExpr arity
                (lambdaExpr
                    ["globals", shortType "Globals"]
                    (matchLambdaExpr
                        [
                            matchClause
                                (listPat (List.map (rename >> namePat) paramz))
                                (buildExpr
                                    (name, Set.union locals (Set.ofList paramz))
                                    body |> toType KlValue)
                            matchClause
                                (namePat "args")
                                (appIdExprN "argsErr"
                                    [
                                        stringExpr ("Lambda@" + name)
                                        listExpr (List.replicate arity (stringExpr "value"))
                                        idExpr "args"
                                    ])
                        ]))
            ])), KlFunction) |> toType KlValue

// TODO: clean up this (ast, type) approach with a fabr type like in ShenScript

let private compileDefun = function
    // and ~(rename name) (globals: Globals) = function
    //     | [~@(map rename paramz)] -> ~body
    //     | args -> argsErr ~name ~(replicate arity "value") args
    | Form [Sym "defun"; Sym name; Form args; body] ->
        let arity = List.length args
        let argNames = List.map param args
        letBinding
            (rename name)
            ["globals", shortType "Globals"]
            (matchLambdaExpr
                [
                    matchClause
                        (listPat (List.map (rename >> namePat) argNames))
                        (buildExpr (name, Set.ofList argNames) body |> toType KlValue)
                    matchClause
                        (namePat "args")
                        (appIdExprN "argsErr"
                            [
                                stringExpr name
                                listExpr (List.replicate arity (stringExpr "value"))
                                idExpr "args"
                            ])
                ])
    | _ -> failwith "Can't compile top-level forms other than defuns"

let private installDefun = function
    // define globals ~name (Compiled(~argcount, ~(rename name)))
    | Form [Sym "defun"; Sym name; Form args; _] ->
        appIdExprN "define"
            [
                idExpr "globals"
                stringExpr name
                (appIdExpr "Compiled"
                    (tupleExpr
                        [
                            intExpr args.Length
                            idExpr (rename name)
                        ]))
            ]
    | _ -> failwith "Can't install a non-defun expression"

let buildInstallationFile name exprs =
    moduleFile (split '.' name)
        [
            openDecl ["Kl"]
            openDecl ["Kl"; "Values"]
            openDecl ["Kl"; "Evaluator"]
            openDecl ["Kl"; "Builtins"]
            openDecl ["Kl"; "Startup"]
            letMultiDecl (List.map compileDefun exprs)
            letDecl
                "install"
                ["globals", shortType "Globals"]
                (sequentialExpr
                    (List.concat
                        [
                            List.map installDefun exprs
                            [appExprN (idExpr "kl_shen.initialise") [(idExpr "globals"); (listExpr [])] |> appIgnore]
                            [idExpr "globals"]
                        ]))
        ]

let buildMetadataFile name copyright version config =
    let meta key value = assemblyAttrDecl (longIdentWithDots key) (stringExpr value)
    moduleFile ["ShenSharp"; "Metadata"]
        [
            meta ["System"; "Reflection"; "AssemblyTitle"] name
            meta ["System"; "Reflection"; "AssemblyDescription"] "Pre-compiled Shen kernel"
            meta ["System"; "Reflection"; "AssemblyProduct"] "ShenSharp"
            meta ["System"; "Reflection"; "AssemblyCopyright"] copyright
            meta ["System"; "Reflection"; "AssemblyVersion"] version
            meta ["System"; "Reflection"; "AssemblyFileVersion"] version
            meta ["System"; "Reflection"; "AssemblyInformationalVersion"] <| version.Substring(0, version.Length - 2)
            meta ["System"; "Reflection"; "AssemblyConfiguration"] config
            meta ["System"; "Runtime"; "Versioning"; "TargetFramework"] ".NETStandard,Version=v2.1"
        ]
