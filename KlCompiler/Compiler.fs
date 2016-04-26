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

    let private buildLambda param body =
        FsExpr.App(
            FsExpr.Id "Func",
            [FsExpr.App(
                FsExpr.Id "Primitive",
                [FsExpr.Tuple(
                    [FsExpr.String "anonymous" // TODO: track context (surrounding defun name) to gen name
                     FsExpr.Int32 1
                     FsExpr.Lambda(
                        false,
                        ["envGlobals", FsType.Of("Globals")],
                         FsExpr.Lambda(
                            false,
                            ["args", FsType.ListOf(FsType.Of("Value"))],
                            body))])])])
                            
    let private buildFreeze body =
        FsExpr.App(
            FsExpr.Id "Func",
            [FsExpr.App(
                FsExpr.Id "Primitive",
                [FsExpr.Tuple(
                    [FsExpr.String "anonymous" // TODO: track context (surrounding defun name) to gen name
                     FsExpr.Int32 0
                     FsExpr.Lambda(
                        false,
                        ["envGlobals", FsType.Of("Globals")],
                         FsExpr.Lambda(
                            false,
                            ["args", FsType.ListOf(FsType.Of("Value"))],
                            body))])])])

    let rec build expr =
        let seBool synExpr = FsExpr.App(FsExpr.LongId ["Values"; "vbool"], [synExpr])

        match expr with
        | EmptyExpr -> FsExpr.Id "Empty"
        | BoolExpr b -> FsExpr.App(FsExpr.Id "Bool", [FsExpr.Bool b])
        | IntExpr i -> FsExpr.App(FsExpr.Id "Int", [FsExpr.Int32 i])
        | DecimalExpr d -> FsExpr.App(FsExpr.Id "Dec", [FsExpr.Decimal d])
        | StringExpr s -> FsExpr.App(FsExpr.Id "Str", [FsExpr.String (escape s)])
        | SymbolExpr s ->
            // TODO: need to maintain a set of local variables so we know what's an idle symbol
            if Values.isVar s
                then FsExpr.Id s
                else FsExpr.App(FsExpr.Id "Sym", [FsExpr.String s])
        | AndExpr(left, right) ->
            FsExpr.If(seBool(build left), build right, build(BoolExpr false))
        | OrExpr(left, right) ->
            FsExpr.If(seBool(build left), build(BoolExpr true), build right)
        | IfExpr(condition, ifTrue, ifFalse) ->
            FsExpr.If(seBool(build condition), build ifTrue, build ifFalse)
        | CondExpr(clauses) ->
            let rec buildClauses clauses =
                match clauses with
                | (BoolExpr false, _) :: rest -> buildClauses rest
                | (BoolExpr true, ifTrue) :: _ -> build ifTrue
                | (condition, ifTrue) :: rest -> FsExpr.If(seBool(build condition), build ifTrue, buildClauses rest)
                | [] -> FsFail.With("No condition was true")
            buildClauses clauses
        | LetExpr(symbol, binding, body) ->
            FsExpr.Let([FsBinding.Of(symbol, build binding)], build body)
        | LambdaExpr(symbol, body) ->
            buildLambda symbol (build body)
        | FreezeExpr(expr) ->
            buildFreeze(build expr)
        | TrapExpr(_, body, handler) ->
            FsExpr.Try(
                build body,
                [FsMatchClause.Of(FsPat.Name("SimpleError", FsPat.Name("e")), FsExpr.Id "Empty")])
        | AppExpr(_, f, args) ->
            match f with
            | SymbolExpr op ->
                let builtArgs = List.map build args
                let builtOp =
                    match Map.tryFind op primitiveNames with
                    | Some(pop) -> FsExpr.LongId ["Builtins"; pop]
                    | _ when Values.isVar op -> FsExpr.Id op
                    | _ -> FsExpr.LongId ["KlImpl"; op]
                FsExpr.App(builtOp, [FsExpr.Id "envGlobals"; FsExpr.List(builtArgs)])
            | _ -> failwith "application expression or special form must start with symbol"
    let topLevelBuild expr =
        match expr with
        | DefunExpr(symbol, paramz, body) ->
            let typedParamz = ["envGlobals", FsType.Of("Globals"); "args", FsType.ListOf(FsType.Of("Value"))]
            FsModule.SingleLet(
                symbol,
                typedParamz,
                FsExpr.Match(FsExpr.Id "args",
                    [FsMatchClause.Of(FsPat.List(List.map FsPat.SimpleName paramz), build body)
                     FsMatchClause.Of(FsPat.Wild, FsFail.With("Wrong number or type of arguments"))]))
        | expr -> failwith "not a defun expr"
    let buildInit exprs =
        let rec buildSeq exprs =
            match exprs with
            | expr :: rest ->
                FsExpr.ConsSequential(
                    FsExpr.App(FsExpr.Id "ignore", [expr]),
                    buildSeq rest)
            | [] -> FsExpr.Unit
        FsModule.SingleLet(
            "init",
            ["envGlobals", FsType.Of("Globals")],
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
        let init = exprs |> List.filter isOtherApp |> List.map other |> List.map build |> buildInit
        let members = List.append decls [init]
        let openKl = FsModule.Open ["Kl"]
        FsFile.Of("KlImpl", [FsModule.Of("KlImpl", List.Cons(openKl, members))])
