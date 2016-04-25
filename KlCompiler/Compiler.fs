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
        let seBool synExpr = FsExpr.App(FsExpr.LongId ["Values"; "vbool"], [synExpr])
        let seResult synExpr = FsExpr.App(FsExpr.Id "Done", [FsExpr.App(FsExpr.Id "Ok", [synExpr])])
        let escape s =
            let escapeChar ch =
                match ch with
                | '\n' -> "\\n"
                | '\r' -> "\\r"
                | '\t' -> "\\t"
                | _ -> ch.ToString()
            String.collect escapeChar s
        let isVar (s: string) = System.Char.IsUpper(s.Chars 0)
        let lambda param body =
            FsExpr.App(
                FsExpr.Id "Ok",
                [FsExpr.App(
                    FsExpr.LongId ["Value"; "FunctionValue"],
                    [FsExpr.App(
                        FsExpr.Id "Primitive",
                        [FsExpr.Tuple(
                            [FsExpr.String "anonymous" // TODO: track context (surrounding defun name) to gen name
                             FsExpr.Int32 1
                             FsExpr.Lambda(
                                false,
                                ["envGlobals", FsType.Of("Globals")
                                 "args", FsType.ListOf(FsType.Of("Value"))],
                                body)])])])])
        let freeze body =
            FsExpr.App(
                FsExpr.Id "Ok",
                [FsExpr.App(
                    FsExpr.LongId ["Value"; "FunctionValue"],
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
                                    body))])])])])
        let buildIf condition ifTrue ifFalse =
            // match ~condition with
            // | Ok(BoolValue true) -> ~ifTrue
            // | Ok(BoolValue false) -> ifFalse
            // | Ok _ -> Err "if expects boolean conditional"
            // | Err message -> Err message
            FsExpr.Match(
                condition,
                [FsMatchClause.Of(
                    FsPat.Name("Ok", FsPat.Name("BoolValue", FsPat.Const(FsConst.Bool true))),
                    ifTrue)
                 FsMatchClause.Of(
                    FsPat.Name("Ok", FsPat.Name("BoolValue", FsPat.Const(FsConst.Bool false))),
                    ifFalse)
                 FsMatchClause.Of(
                    FsPat.Name("Ok", FsPat.Wild),
                    FsExpr.App(FsExpr.Id "Err", [FsExpr.String "if expects boolean conditional"]))
                 FsMatchClause.Of(
                    FsPat.Name("Err", FsPat.Name("message")),
                    FsExpr.App(FsExpr.Id "Err", [FsExpr.Id "message"]))])
        
        let rec buildOps operator operands operandIndex =
            match operands with
            | [] ->
                let opArgs = List.map (fun i -> FsExpr.Id("temp" + (string i))) (Seq.toList(seq { 0 .. operandIndex }))
                FsExpr.App(operator, [FsExpr.Id "envGlobals"; FsExpr.List opArgs])
            | operand :: rest ->
                FsExpr.Match(
                    operand,
                    [FsMatchClause.Of(
                        FsPat.Name("Ok", [FsPat.Name("temp" + (string operandIndex))]),
                        buildOps operator rest (operandIndex + 1))
                     FsMatchClause.Of(
                        FsPat.Name("Err", [FsPat.Name("message")]),
                        FsExpr.App(FsExpr.Id "Err", [FsExpr.Id "message"]))])

        let buildApp operator operands operandIndex =
            FsExpr.Match(
                operator,
                [FsMatchClause.Of(
                    FsPat.Name("Ok", [FsPat.Name("tempOp")]),
                    buildOps (FsExpr.Id "tempOp") operands (operandIndex + 1))
                 FsMatchClause.Of(
                    FsPat.Name("Err", [FsPat.Name("message")]),
                    FsExpr.App(FsExpr.Id "Err", [FsExpr.Id "message"]))])

        match expr with
        | EmptyExpr -> FsExpr.App(FsExpr.Id "Ok", [FsExpr.Id "EmptyValue"])
        | BoolExpr b -> FsExpr.App(FsExpr.Id "Ok", [FsExpr.App(FsExpr.Id "BoolValue", [FsExpr.Bool b])])
        | IntExpr i -> FsExpr.App(FsExpr.LongId ["Value"; "IntValue"], [FsExpr.Int32 i])
        | DecimalExpr d -> FsExpr.App(FsExpr.LongId ["Value"; "DecimalValue"], [FsExpr.Decimal d])
        | StringExpr s -> FsExpr.App(FsExpr.Id "Ok", [FsExpr.App(FsExpr.Id "StringValue", [FsExpr.String (escape s)])])
        | SymbolExpr s ->
            // TODO: need to maintain a set of local variables so we know what's an idle symbol
            if isVar s
                then FsExpr.Id (klToFsId s)
                else FsExpr.App(FsExpr.LongId ["Value"; "SymbolValue"], [FsExpr.String s])
        | AndExpr(left, right) -> buildIf (build left) (build right) (build (BoolExpr false))
        | OrExpr(left, right) -> buildIf (build left) (build (BoolExpr true)) (build right)
        | IfExpr(condition, ifTrue, ifFalse) -> buildIf (build condition) (build ifTrue) (build ifFalse)
        | CondExpr(clauses) ->
            let rec buildClauses clauses =
                match clauses with
                | (BoolExpr false, _) :: rest -> buildClauses rest
                | (BoolExpr true, ifTrue) :: _ -> build ifTrue
                | (condition, ifTrue) :: rest -> buildIf (build condition) (build ifTrue) (buildClauses rest)
                | [] -> FsFail.With("No condition was true")
            buildClauses clauses
        | LetExpr(symbol, binding, body) -> FsExpr.Let([FsBinding.Of(symbol, build binding)], build body)
        | LambdaExpr(symbol, body) ->
            lambda symbol (build body)
        | FreezeExpr(expr) ->
            freeze (build expr)
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
                let builtArgs = List.map build args
                let builtOp =
                    match primitiveOp op with
                    | Some(pop) -> pop
                    | _ when isVar op -> FsExpr.Id op
                    | _ -> FsExpr.LongId ["KlImpl"; op]
                buildApp builtOp builtArgs 0
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
            | expr :: rest -> FsExpr.ConsSequential(expr, buildSeq rest)
            | [] -> FsExpr.Unit
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
