namespace KlCompiler

open Kl

module Compiler =

    let private primitiveNames =
        Map.ofList [
            "intern",          "klIntern"
            "pos",             "klStringPos"
            "tlstr",           "klStringTail"
            "cn",              "klStringConcat"
            "str",             "klToString"
            "string?",         "klIsString"
            "n->string",       "klIntToString"
            "string->n",       "klStringToInt"
            "set",             "klSet"
            "value",           "klValue"
            "simple-error",    "klSimpleError"
            "error-to-string", "klErrorToString"
            "cons",            "klNewCons"
            "hd",              "klHead"
            "tl",              "klTail"
            "cons?",           "klIsCons"
            "=",               "klEquals"
            "type",            "klType"
            "eval-kl",         "klEval"
            "absvector",       "klNewVector"
            "<-address",       "klReadVector"
            "address->",       "klWriteVector"
            "absvector?",      "klIsVector"
            "write-byte",      "klWriteByte"
            "read-byte",       "klReadByte"
            "open",            "klOpen"
            "close",           "klClose"
            "get-time",        "klGetTime"
            "+",               "klAdd"
            "-",               "klSubtract"
            "*",               "klMultiply"
            "/",               "klDivide"
            ">",               "klGreaterThan"
            "<",               "klLessThan"
            ">=",              "klGreaterThanEqual"
            "<=",              "klLessThanEqual"
            "number?",         "klIsNumber"
        ]

    let private escape s =
        let escapeChar ch =
            match ch with
            | '\n' -> "\\n"
            | '\r' -> "\\r"
            | '\t' -> "\\t"
            | _ -> ch.ToString()
        String.collect escapeChar s

    let globalsParam = "(globals)", FsType.Of("Globals")

    let private buildLambda context param body =
        FsExpr.App(
            FsExpr.Id "Func",
            [FsExpr.App(
                FsExpr.Id "Native",
                [FsExpr.Tuple(
                    [FsExpr.String (sprintf "%s/%s" context "lambda")
                     FsExpr.Int32 1
                     FsExpr.Lambda(
                        false,
                        [globalsParam],
                         FsExpr.Lambda(
                            false,
                            ["args", FsType.ListOf(FsType.Of("Value"))],
                            body))])])])
                            
    let private buildFreeze context body =
        FsExpr.App(
            FsExpr.Id "Func",
            [FsExpr.App(
                FsExpr.Id "Native",
                [FsExpr.Tuple(
                    [FsExpr.String (sprintf "%s/%s" context "freeze")
                     FsExpr.Int32 0
                     FsExpr.Lambda(
                        false,
                        [globalsParam],
                         FsExpr.Lambda(
                            false,
                            ["args", FsType.ListOf(FsType.Of("Value"))],
                            body))])])])

    let rec build context defs expr =
        let seBool synExpr = FsExpr.App(FsExpr.LongId ["Values"; "vbool"], [synExpr])

        match expr with
        | Empty -> FsExpr.Id "Empty"
        | Bool b -> FsExpr.App(FsExpr.Id "Bool", [FsExpr.Bool b])
        | Int i -> FsExpr.App(FsExpr.Id "Int", [FsExpr.Int32 i])
        | Dec d -> FsExpr.App(FsExpr.Id "Dec", [FsExpr.Decimal d])
        | Str s -> FsExpr.App(FsExpr.Id "Str", [FsExpr.String (escape s)])

        // This should only be symbols that are not at the head of an application
        | Sym s ->
            if Set.contains s defs
                then FsExpr.Id s
                else FsExpr.App(FsExpr.Id "Sym", [FsExpr.String s])
        | Cons(Sym "and", Cons(left, Cons(right, Empty))) ->
            FsExpr.If(seBool(build context defs left), build context defs right, build context defs (Bool false))
        | Cons(Sym "or", Cons(left, Cons(right, Empty))) ->
            FsExpr.If(seBool(build context defs left), build context defs (Bool true), build context defs right)
        | Cons(Sym "if", Cons(condition, Cons(ifTrue, Cons(ifFalse, Empty)))) ->
            FsExpr.If(seBool(build context defs condition), build context defs ifTrue, build context defs ifFalse)
        | Cons(Sym "cond", clauses) ->
            let rec buildClauses clauses =
                match clauses with
                | Cons(Bool false, Cons(_, rest)) -> buildClauses rest
                | Cons(Bool true, Cons(ifTrue, _)) -> build context defs ifTrue
                | Cons(condition, Cons(ifTrue, rest)) ->
                    FsExpr.If(
                        seBool(build context defs condition),
                        build context defs ifTrue,
                        buildClauses rest)
                | Empty -> FsFail.With("No condition was true")
                | _ -> failwith "Unexpected value - cannot compile"
            buildClauses clauses
        | Cons(Sym "let", Cons(Sym symbol, Cons(binding, Cons(body, Empty)))) ->
            FsExpr.Let([FsBinding.Of(symbol, build context defs binding)], build context (Set.add symbol defs) body)
        | Cons(Sym "lambda", Cons(Sym symbol, Cons(body, Empty))) ->
            buildLambda context symbol (build context (Set.add symbol defs) body)
        | Cons(Sym "freeze", Cons(body, Empty)) ->
            buildFreeze context (build context defs body)
        | Cons(Sym "trap-error", Cons(body, Cons(handler, Empty))) ->
            FsExpr.Try(
                build context defs body,
                [FsMatchClause.Of(FsPat.Name("SimpleError", FsPat.Name("e")), FsExpr.Id "Empty")])
        | Cons(f, args) ->
            let builtArgs = List.map (build context defs) (Values.toList args)
            match f with
            | Sym op ->
                let builtOp =
                    match Map.tryFind op primitiveNames with
                    | Some(pop) -> FsExpr.LongId ["Builtins"; pop]
                    | _ when Values.isVar op -> FsExpr.Id op
                    | _ -> FsExpr.Id op
                FsExpr.App(builtOp, [FsExpr.Id (fst globalsParam); FsExpr.List(builtArgs)])
            | e -> FsExpr.App(build context defs e, [FsExpr.Id (fst globalsParam); FsExpr.List(builtArgs)])
        | _ -> failwith "Unrecognized expression"

    let private topLevelBuild expr =
        match expr with
        | Cons(Sym symbol, Cons(paramz, Cons(body, Empty))) ->
            let paramList = List.map Values.vsym (Values.toList paramz)
            let typedParamz = [globalsParam; "args", FsType.ListOf(FsType.Of("Value"))]
            FsModule.SingleLet(
                symbol,
                typedParamz,
                FsExpr.Match(FsExpr.Id "args",
                    [FsMatchClause.Of(FsPat.List(List.map FsPat.SimpleName paramList), build symbol (Set.ofList paramList) body)
                     FsMatchClause.Of(FsPat.Wild, FsFail.With("Wrong number or type of arguments"))]))
        | expr -> failwith "not a defun expr"

    let private buildInit exprs =
        let rec buildSeq exprs =
            match exprs with
            | expr :: rest ->
                FsExpr.ConsSequential(
                    FsExpr.App(FsExpr.Id "ignore", [expr]),
                    buildSeq rest)
            | [] -> FsExpr.Unit
        FsModule.SingleLet(
            "init",
            [globalsParam],
            buildSeq exprs)

    let buildModule exprs =
        let isDefun = function
            | Cons(Sym "defun", _) -> true
            | _ -> false
        let isOtherApp = function
            | Cons(_, _) -> true
            | _ -> false
        let decls = exprs |> List.filter isDefun |> List.map topLevelBuild
        let init = exprs |> List.filter isOtherApp |> List.map (build "_root/" Set.empty) |> buildInit
        let members = List.append decls [init]
        let openKl = FsModule.Open ["Kl"]
        FsFile.Of("Shen", [FsModule.Of("Shen", List.Cons(openKl, members))])
        
    let lambdaFex name paramz body =
        let mat = Match(Id "_args", [Lst(List.map (fun p -> Id("_" + p)) paramz), body; Wild, App(Id "err", [Strex "args"])])
        App(Id "Func", [App(Id "Native", [Tup [Strex name; Intex (List.length paramz); Lamb("_globals", "Globals", Lamb("_args", "Value list", mat))]])])
    let rec buildFex ctx defs expr =
        match expr with
        | Empty -> Id "Empty"
        | Bool b -> App(Id "Bool", [Boolex b])
        | Int i -> App(Id "Int", [Intex i])
        | Dec d -> App(Id "Dec", [Decex d])
        | Str s -> App(Id "Str", [Strex s])
        | Sym s ->
            if not(Set.contains s defs) then
                App(Id "Sym", [Strex s])
            else
                Id("_" + s)
        | Cons(Sym "and", Cons(left, Cons(right, Empty))) ->
            App(Id "Bool", [Infix("&&", App(Id "vbool", [buildFex ctx defs left]), App(Id "vbool", [buildFex ctx defs right]))])
        | Cons(Sym "or", Cons(left, Cons(right, Empty))) ->
            App(Id "Bool", [Infix("||", App(Id "vbool", [buildFex ctx defs left]), App(Id "vbool", [buildFex ctx defs right]))])
        | Cons(Sym "if", Cons(condition, Cons(consequent, Cons(alternative, Empty)))) ->
            If(App(Id "vbool", [buildFex ctx defs condition]),
               buildFex ctx defs consequent,
               buildFex ctx defs alternative)
        | Cons(Sym "cond", clauses) ->
            let rec buildClauses clauses =
                match clauses with
                | Empty -> App(Id "err", [Strex "No condition was true"])
                | Cons(condition, Cons(consequent, rest)) ->
                    If(App(Id "vbool", [buildFex ctx defs condition]),
                       buildFex ctx defs consequent,
                       buildClauses rest)
                | _ -> failwith "Unexpected value - cannot compile"
            buildClauses clauses
        | Cons(Sym "let", Cons(Sym symbol, Cons(binding, Cons(body, Empty)))) ->
            Let("_" + symbol, buildFex ctx defs binding, buildFex ctx (Set.add symbol defs) body)
        | Cons(Sym "lambda", Cons(Sym param, Cons(body, Empty))) -> lambdaFex (ctx + "@lambda") [param] (buildFex ctx (Set.add param defs) body)
            // Func(Native("", 1, fun _globals -> fun _args -> match _args with | [param] -> body | _ -> err "args"))
        | Cons(Sym "freeze", body) -> lambdaFex (ctx + "@freeze") [] (buildFex ctx defs body)
            // Func(Native("", 0, fun _globals -> fun _args -> match _args with | [] -> body | _ -> err "args"))
        | Cons(Sym "trap-error", Cons(body, Cons(handler, Empty))) -> Try(buildFex ctx defs body, buildFex ctx defs handler)
        | Cons(Sym s, args) ->
            let builtArgs = Lst(List.map (buildFex ctx defs) (Values.toList args))
            if Set.contains s defs then
                App(Id "applyc", [Id "_globals"; App(Id "vfunc", [Id("_" + s)]); builtArgs])
                // applyc _globals (vfunc V12345) [args]
            else if primitiveNames.ContainsKey s then
                App(Id primitiveNames.[s], [Id "_globals"; builtArgs])
                // klPrim _globals [args]
            else
                App(Id s, [Id "_globals"; builtArgs])
                // ``shen.whatever`` _globals [args]
        | Cons(f, args) ->
            let builtArgs = Lst(List.map (buildFex ctx defs) (Values.toList args))
            let builtF = buildFex ctx defs f
            App(Id "applyc", [Id "_globals"; App(Id "vfunc", [builtF]); builtArgs])
            // applyc _globals (vfunc f) [args]
