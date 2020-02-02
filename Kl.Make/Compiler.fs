module internal Kl.Make.Compiler

open Kl
open Kl.Values
open Syntax

let private most = function
    | _ :: xs -> xs
    | [] -> []

let private split (sep: char) (s: string) = s.Split sep |> Array.toList

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

let private buildGlobalLookup s = parens (appIdExprN "lookup" [idExpr "globals"; stringExpr s])

let rec private buildApp (name, locals) f args =
    let fExpr =
        match f with
        // apply globals (asFunction ~(rename s)) ~args
        | Sym s when Set.contains s locals -> idKl s |> toType KlFunction
        // apply globals (lookup globals ~s) ~args
        | Sym s -> buildGlobalLookup s
        // apply globals (asFunction ~f) ~args
        | _ -> buildExpr (name, locals) f |> toType KlFunction
    appIdExprN "apply" [idExpr "globals"; fExpr; listExpr args]

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
        let buildClause condition consequent rest =
            match condition with
            | Bool true -> consequent
            | Bool false -> rest
            | _ -> toCons [Sym "if"; condition; consequent; rest]
        toCons [Sym "simple-error"; Str "No condition was true"]
        |> List.foldBack (fun (condition, consequent) rest -> buildClause condition consequent rest) clauses
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
        buildFunction name locals [param] body
    | Form [Sym "freeze"; body] ->
        buildFunction name locals [] body
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
    | Form [Sym "="; x; y] ->
        let xExpr = buildExpr context x
        let yExpr = buildExpr context y
        let t = simplestCommonType (snd xExpr) (snd yExpr)
        infixIdExpr "op_Equality" (xExpr |> toType t) (yExpr |> toType t), FsBoolean
    | Form [Sym "cons"; _; _] as consExpr ->
        // toCons [x0; x1; x2 ... xN]
        match gatherConsElements consExpr with
        | xs, Some t ->
            appIdExprN "toConsWithTail"
                [
                    buildExpr context t |> fst
                    listExpr (List.map (buildExpr context >> fst) xs)
                ], KlValue
        | xs, _ ->
            appIdExpr "toCons" (listExpr (List.map (buildExpr context >> fst) xs)), KlValue
    | Form(f :: args) ->
        buildApp context f (List.map (buildExpr context >> toType KlValue) args), KlValue
    | klExpr ->
        failwithf "Unable to compile: %O" klExpr

and private buildFunction name locals paramz body =
    let arity = List.length paramz
    // Compiled(
    //      ~arity,
    //      fun globals -> function
    //          | [~@paramz] -> ~body
    //          | args -> argsErr "Lambda@~name" ["value"; ...] args
    // )
    (appIdExpr "Compiled"
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
            ])), KlFunction

let private argName = function
    | Sym name -> name
    | _ -> failwith "defun arg name must be a symbol"

let private compileDefun = function
    // and ~(rename name) (globals: Globals) = function
    //     | [~@(map rename paramz)] -> ~body
    //     | args -> argsErr ~name ~(replicate arity "value") args
    | Form [Sym "defun"; Sym name; Form args; body] ->
        let arity = List.length args
        let argNames = List.map argName args
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
                            [appExpr (idExpr "kl_shen.initialise") unitExpr]
                            [idExpr "globals"]
                        ]))
        ]

let buildMetadataFile name =
    let meta key value = assemblyAttrDecl (longIdentWithDots key) (stringExpr value)
    moduleFile ["ShenSharp"; "Metadata"]
        [
            meta ["System"; "Reflection"; "AssemblyTitle"] name
            meta ["System"; "Runtime"; "Versioning"; "TargetFramework"] ".NETStandard,Version=v2.1"
        ]
