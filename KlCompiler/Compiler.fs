namespace KlCompiler

open Kl

module Compiler =
    let rec build expr =
        let klToFsId (klId:string) =
            klId.Replace("?", "_P_")
                .Replace("<", "_LT_")
                .Replace(">", "_GT_")
                .Replace("-", "_")
                .Replace(".", "_DOT_")
                .Replace("+", "_PLUS_")
                .Replace("*", "_STAR_")
                .TrimEnd('_')
        let builtin id = FsExpr.LongId ["Builtins"; id]
        let seBool synExpr = FsExpr.App(builtin "vBool", [synExpr])
        let seResult synExpr = FsExpr.App(FsExpr.Id "Completed", [FsExpr.App(FsExpr.Id "ValueResult", [synExpr])])
        let escape s =
            let escapeChar ch =
                match ch with
                | '\n' -> "\\n"
                | '\r' -> "\\r"
                | '\t' -> "\\t"
                | _ -> ch.ToString()
            String.collect escapeChar s
        let isVar (s: string) = System.Char.IsUpper(s.Chars 0)
        let klFunction argCount lambda =
            FsExpr.App(
                FsExpr.LongId ["Value"; "FunctionValue"],
                [FsExpr.App(
                    FsExpr.LongId ["Function"; "func"],
                    [FsConst.String("Anonymous")
                     FsConst.Int32(argCount)
                     FsExpr.List([])
                     FsExpr.Lambda(
                        false,
                        Some("envGlobals", FsType.Of("Globals")),
                        lambda)])])
        match expr with
        | EmptyExpr -> FsExpr.LongId ["Value"; "EmptyValue"]
        | BoolExpr b -> FsExpr.App(FsExpr.LongId ["Value"; "BoolValue"], [FsConst.Bool b])
        | IntExpr i -> FsExpr.App(FsExpr.LongId ["Value"; "IntValue"], [FsConst.Int32 i])
        | DecimalExpr d -> FsExpr.App(FsExpr.LongId ["Value"; "DecimalValue"], [FsConst.Decimal d])
        | StringExpr s -> FsExpr.App(FsExpr.LongId ["Value"; "StringValue"], [FsConst.String (escape s)])
        | SymbolExpr s ->
            if isVar s
                then FsExpr.Id (klToFsId s)
                else FsExpr.App(FsExpr.LongId ["Value"; "SymbolValue"], [FsConst.String s])
        | AndExpr(left, right) -> FsExpr.Infix(build left |> seBool, FsExpr.Id("op_BooleanAnd"), build right |> seBool)
        | OrExpr(left, right) -> FsExpr.Infix(build left |> seBool, FsExpr.Id("op_BooleanOr"), build right |> seBool)
        | IfExpr(condition, ifTrue, ifFalse) -> FsExpr.If(build condition |> seBool, build ifTrue, build ifFalse)
        | CondExpr(clauses) ->
            let rec buildClauses = function
                | (BoolExpr false, _) :: rest -> buildClauses rest
                | (BoolExpr true, ifTrue) :: _ -> build ifTrue
                | (condition, ifTrue) :: rest -> FsExpr.If(build condition |> seBool, build ifTrue, buildClauses rest)
                | [] -> FsFail.With("No condition was true")
            buildClauses clauses
        | LetExpr(symbol, binding, body) -> FsExpr.Let([FsBinding.Of(symbol, build binding)], build body)
        | LambdaExpr(symbol, body) ->
            klFunction 1 (FsExpr.Lambda(false, Some(symbol, FsType.Of("Value")), seResult (build body)))
        | FreezeExpr(expr) ->
            klFunction 1 (FsExpr.Lambda(false, Some("args", FsType.ListOf(FsType.Of("Value"))), seResult (build expr)))
        | TrapExpr(_, t, c) ->
            FsExpr.App(FsExpr.LongId ["Builtins"; "trapError"], [build t; build c])
        | AppExpr(_, f, args) ->
            let primitiveOp op =
                match op with
                | "intern"          -> Some(builtin "klIntern")
                | "pos"             -> Some(builtin "klStringPos")
                | "tlstr"           -> Some(builtin "klStringTail")
                | "cn"              -> Some(builtin "klStringConcat")
                | "str"             -> Some(builtin "klToString")
                | "string?"         -> Some(builtin "klIsString")
                | "n->string"       -> Some(builtin "klIntToString")
                | "string->n"       -> Some(builtin "klStringToInt")
                | "set"             -> Some(builtin "klSet")
                | "value"           -> Some(builtin "klValue")
                | "simple-error"    -> Some(builtin "klSimpleError")
                | "error-to-string" -> Some(builtin "klErrorToString")
                | "cons"            -> Some(builtin "klNewCons")
                | "hd"              -> Some(builtin "klHead")
                | "tl"              -> Some(builtin "klTail")
                | "cons?"           -> Some(builtin "klIsCons")
                | "="               -> Some(builtin "klEquals")
                | "type"            -> Some(builtin "klType")
                | "eval-kl"         -> Some(builtin "klEval")
                | "absvector"       -> Some(builtin "klNewVector")
                | "<-address"       -> Some(builtin "klReadVector")
                | "address->"       -> Some(builtin "klWriteVector")
                | "absvector?"      -> Some(builtin "klIsVector")
                | "write-byte"      -> Some(builtin "klWriteByte")
                | "read-byte"       -> Some(builtin "klReadByte")
                | "open"            -> Some(builtin "klOpen")
                | "close"           -> Some(builtin "klClose")
                | "get-time"        -> Some(builtin "klGetTime")
                | "+"               -> Some(builtin "klAdd")
                | "-"               -> Some(builtin "klSubtract")
                | "*"               -> Some(builtin "klMultiply")
                | "/"               -> Some(builtin "klDivide")
                | ">"               -> Some(builtin "klGreaterThan")
                | "<"               -> Some(builtin "klLessThan")
                | ">="              -> Some(builtin "klGreaterThanEqual")
                | "<="              -> Some(builtin "klLessThanEqual")
                | "number?"         -> Some(builtin "klIsNumber")
                | _                 -> None
            match f with
            | SymbolExpr op ->
                let builtArgs = [FsExpr.Id "envGlobals"; FsExpr.List(List.map build args)]
                match primitiveOp op with
                | Some(pop) -> FsExpr.App(pop, builtArgs)
                | _ when isVar op -> FsExpr.App(FsExpr.Id op, builtArgs)
                | _ -> FsExpr.App(FsExpr.LongId ["KlImpl"; op], builtArgs)
            | _ -> failwith "application expression or special form must start with symbol"
    let topLevelBuild expr =
        match expr with
        | DefunExpr(symbol, paramz, body) ->
            let typedParamz = ["envGlobals", FsType.Of("Globals"); "args", FsType.ListOf(FsType.Of("Value"))]
            FsModule.SingleLet(
                symbol,
                typedParamz,
                FsExpr.Match(FsExpr.Id "args",
                    [FsMatchClause.Of(FsPat.List(List.map FsPat.Name paramz), build body)
                     FsMatchClause.Of(FsPat.Wild, FsFail.With("Wrong number or type of arguments"))]))
        | expr -> failwith "not a defun expr"
    let buildInit exprs =
        let rec buildSeq exprs =
            match exprs with
            | expr :: rest -> FsExpr.ConsSequential(expr, buildSeq rest)
            | [] -> FsConst.Unit
        FsModule.SingleLet("init", ["envGlobals", FsType.Of("Globals")], buildSeq exprs)
    let buildModule exprs =
        let isDefun = function
            | DefunExpr _ -> true
            | _ -> false
        let isOther = function
            | OtherExpr _ -> true
            | _ -> false
        let other = function
            | OtherExpr e -> e
            | _ -> failwith "not other"
        let decls = exprs |> List.filter isDefun |> List.map topLevelBuild
        let init = exprs |> List.filter isOther |> List.map other |> List.map build |> buildInit
        let members = List.append decls [init]
        let openKl = FsModule.Open ["Kl"]
        FsFile.Of("KlImpl", [FsModule.Of("KlImpl", List.Cons(openKl, members))])
