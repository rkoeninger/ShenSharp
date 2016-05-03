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
        | EmptyExpr -> FsExpr.Id "Empty"
        | BoolExpr b -> FsExpr.App(FsExpr.Id "Bool", [FsExpr.Bool b])
        | IntExpr i -> FsExpr.App(FsExpr.Id "Int", [FsExpr.Int32 i])
        | DecExpr d -> FsExpr.App(FsExpr.Id "Dec", [FsExpr.Decimal d])
        | StrExpr s -> FsExpr.App(FsExpr.Id "Str", [FsExpr.String (escape s)])

        // This should only be symbols that are not at the head of an application
        | SymExpr s ->
            if Set.contains s defs
                then FsExpr.Id s
                else FsExpr.App(FsExpr.Id "Sym", [FsExpr.String s])
        | AndExpr(left, right) ->
            FsExpr.If(seBool(build context defs left), build context defs right, build context defs (BoolExpr false))
        | OrExpr(left, right) ->
            FsExpr.If(seBool(build context defs left), build context defs (BoolExpr true), build context defs right)
        | IfExpr(condition, ifTrue, ifFalse) ->
            FsExpr.If(seBool(build context defs condition), build context defs ifTrue, build context defs ifFalse)
        | CondExpr(clauses) ->
            let rec buildClauses clauses =
                match clauses with
                | (BoolExpr false, _) :: rest -> buildClauses rest
                | (BoolExpr true, ifTrue) :: _ -> build context defs ifTrue
                | (condition, ifTrue) :: rest ->
                    FsExpr.If(
                        seBool(build context defs condition),
                        build context defs ifTrue,
                        buildClauses rest)
                | [] -> FsFail.With("No condition was true")
            buildClauses clauses
        | LetExpr(symbol, binding, body) ->
            FsExpr.Let([FsBinding.Of(symbol, build context defs binding)], build context (Set.add symbol defs) body)
        | LambdaExpr(symbol, body) ->
            buildLambda context symbol (build context (Set.add symbol defs) body)
        | FreezeExpr(expr) ->
            buildFreeze context (build context defs expr)
        | TrapExpr(_, body, handler) ->
            FsExpr.Try(
                build context defs body,
                [FsMatchClause.Of(FsPat.Name("SimpleError", FsPat.Name("e")), FsExpr.Id "Empty")])
        | AppExpr(_, f, args) ->
            let builtArgs = List.map (build context defs) args
            match f with
            | SymExpr op ->
                let builtOp =
                    match Map.tryFind op primitiveNames with
                    | Some(pop) -> FsExpr.LongId ["Builtins"; pop]
                    | _ when Values.isVar op -> FsExpr.Id op
                    | _ -> FsExpr.Id op
                FsExpr.App(builtOp, [FsExpr.Id (fst globalsParam); FsExpr.List(builtArgs)])
            | e -> FsExpr.App(build context defs e, [FsExpr.Id (fst globalsParam); FsExpr.List(builtArgs)])

    let private topLevelBuild expr =
        match expr with
        | DefunExpr(symbol, paramz, body) ->
            let typedParamz = [globalsParam; "args", FsType.ListOf(FsType.Of("Value"))]
            FsModule.SingleLet(
                symbol,
                typedParamz,
                FsExpr.Match(FsExpr.Id "args",
                    [FsMatchClause.Of(FsPat.List(List.map FsPat.SimpleName paramz), build symbol (Set.ofList paramz) body)
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
            | DefunExpr _ -> true
            | _ -> false
        let isOtherApp = function
            | OtherExpr(AppExpr _) -> true
            | _ -> false
        let other = function
            | OtherExpr e -> e
            | _ -> failwith "not other"
        let decls = exprs |> List.filter isDefun |> List.map topLevelBuild
        let init = exprs |> List.filter isOtherApp |> List.map other |> List.map (build "_root/" Set.empty) |> buildInit
        let members = List.append decls [init]
        let openKl = FsModule.Open ["Kl"]
        FsFile.Of("Shen", [FsModule.Of("Shen", List.Cons(openKl, members))])
        
    let lambdaFex name paramz body =
        let mat = Match(Id "_args", [Lst(List.map Id paramz), body; Wild, App(Id "err", [Strex "args"])])
        App(Id "Func", [App(Id "Native", [Tup [Strex name; Intex (List.length paramz); Lamb("_globals", "Globals", Lamb("_args", "Value list", mat))]])])
    let rec buildFex ctx defs expr =
        match expr with
        | EmptyExpr -> SafeId "Empty"
        | BoolExpr b -> App(SafeId "Bool", [Boolex b])
        | IntExpr i -> App(SafeId "Int", [Intex i])
        | DecExpr d -> App(SafeId "Dec", [Decex d])
        | StrExpr s -> App(SafeId "Str", [Strex s])
        | SymExpr s ->
            if not(Set.contains s defs) then
                App(SafeId "Sym", [Strex s])
            else if primitiveNames.ContainsKey s then
                Id primitiveNames.[s]
            else
                Id s
        | AndExpr(left, right) -> App(SafeId "Bool", [Infix("&&", App(SafeId "vbool", [buildFex ctx defs left]), App(SafeId "vbool", [buildFex ctx defs right]))])
        | OrExpr(left, right) -> App(SafeId "Bool", [Infix("||", App(SafeId "vbool", [buildFex ctx defs left]), App(SafeId "vbool", [buildFex ctx defs right]))])
        | IfExpr(condition, consequent, alternative) ->
            If(App(SafeId "vbool", [buildFex ctx defs condition]),
               buildFex ctx defs consequent,
               buildFex ctx defs alternative)
        | CondExpr clauses ->
            let rec buildClauses clauses =
                match clauses with
                | [] -> App(SafeId "err", [Strex "No condition was true"])
                | (condition, consequent) :: rest ->
                    If(App(SafeId "vbool", [buildFex ctx defs condition]),
                       buildFex ctx defs consequent,
                       buildClauses rest)
            buildClauses clauses
        | LetExpr(symbol, binding, body) -> Let(symbol, buildFex ctx defs binding, buildFex ctx (Set.add symbol defs) body)
        | LambdaExpr(param, body) -> lambdaFex (ctx + "@lambda") [param] (buildFex ctx (Set.add param defs) body)
            // Func(Native("", 1, fun _globals -> fun _args -> match _args with | [param] -> body | _ -> err "args"))
        | FreezeExpr body -> lambdaFex (ctx + "@freeze") [] (buildFex ctx defs body)
            // Func(Native("", 0, fun _globals -> fun _args -> match _args with | [] -> body | _ -> err "args"))
        | TrapExpr(_, body, handler) -> Try(buildFex ctx defs body, buildFex ctx defs handler)
        | AppExpr(_, SymExpr s, args) ->
            let builtArgs = Lst(List.map (buildFex ctx defs) args)
            if Set.contains s defs then
                App(SafeId "applyc", [SafeId "_globals"; App(SafeId "vfunc", [Id s]); builtArgs])
                // applyc _globals (vfunc V12345) [args]
            else if primitiveNames.ContainsKey s then
                App(SafeId primitiveNames.[s], [SafeId "_globals"; builtArgs])
                // klPrim _globals [args]
            else
                App(Id s, [SafeId "_globals"; builtArgs])
                // ``shen.whatever`` _globals [args]
        | AppExpr(_, f, args) ->
            let builtArgs = Lst(List.map (buildFex ctx defs) args)
            let builtF = buildFex ctx defs f
            App(SafeId "applyc", [SafeId "_globals"; App(SafeId "vfunc", [builtF]); builtArgs])
            // applyc _globals (vfunc f) [args]
