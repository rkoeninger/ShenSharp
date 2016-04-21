namespace KlCompiler

open Kl
open Fantomas
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

module FsAst =
    let defaultRange = mkFileIndexRange 50 (mkPos 100 100) (mkPos 200 200)
    let longIdDots ids = 
        let ident id = new Ident(id, defaultRange)
        LongIdentWithDots.LongIdentWithDots(
            List.map ident ids,
            List.replicate ((ids.Length) - 1) defaultRange)

type FsFile =

    static member Of(ns: string, modules: SynModuleOrNamespace list) =
        ParsedInput.ImplFile(
            ParsedImplFileInput(
                "filename",
                false,
                QualifiedNameOfFile(new Ident(ns, FsAst.defaultRange)),
                [],
                [],
                modules,
                false))

type FsConst =

    static member Int32(x: int) =
        SynExpr.Const(SynConst.Int32(x), FsAst.defaultRange)

    static member String(x: string) =
        SynExpr.Const(SynConst.String(x, FsAst.defaultRange), FsAst.defaultRange)

    static member Unit = SynExpr.Const(SynConst.Unit, FsAst.defaultRange)
        
type FsType =

    static member ListOf(typeArg: SynType) =
        SynType.App(SynType.LongIdent(FsAst.longIdDots ["list"]), None, [typeArg], [FsAst.defaultRange], None, true, FsAst.defaultRange)
    
    static member Of(typeName: string) =
        SynType.LongIdent(
            LongIdentWithDots.LongIdentWithDots(
                [new Ident(typeName, FsAst.defaultRange)],
                []))

type FsModule =

    static member Of(name: string, members: SynModuleDecls) =
        SynModuleOrNamespace.SynModuleOrNamespace(
            [new Ident(name, FsAst.defaultRange)],
            true,
            members,
            PreXmlDocEmpty,
            [],
            None,
            FsAst.defaultRange)

    static member Let(bindings: SynBinding list) =
        SynModuleDecl.Let(false, bindings, FsAst.defaultRange)

    static member SingleLet(name: string, args: (string * SynType) list, body: SynExpr) =
        let eachArg (nm, _) = [SynArgInfo.SynArgInfo([], false, Some(new Ident(nm, FsAst.defaultRange)))]
        let argInfos = List.map eachArg args
        let valData =
            SynValData.SynValData(
                None,
                SynValInfo.SynValInfo(
                    argInfos,
                    SynArgInfo.SynArgInfo([], false, None)),
                None)
        let eachCtorArg (nm, typ) =
            SynPat.Paren(
                SynPat.Typed(
                    SynPat.Named(
                        SynPat.Wild(FsAst.defaultRange),
                        new Ident(nm, FsAst.defaultRange),
                        false,
                        None,
                        FsAst.defaultRange),
                    typ,
                    FsAst.defaultRange),
                FsAst.defaultRange)
        let ctorArgs = List.map eachCtorArg args
        let namePat =
            SynPat.LongIdent(
                LongIdentWithDots.LongIdentWithDots([new Ident(name, FsAst.defaultRange)], []),
                None,
                None,
                SynConstructorArgs.Pats(ctorArgs),
                None,
                FsAst.defaultRange)
        let returnType = FsType.Of("KlValue")
        let returnInfo = SynBindingReturnInfo.SynBindingReturnInfo(returnType, FsAst.defaultRange, [])
        let binding =
            SynBinding.Binding(
                None,
                SynBindingKind.NormalBinding,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                valData,
                namePat,
                Some returnInfo,
                body,
                FsAst.defaultRange,
                SequencePointInfoForBinding.NoSequencePointAtLetBinding)
        SynModuleDecl.Let(false, [binding], FsAst.defaultRange)

    static member LetRec(bindings: SynBinding list) =
        SynModuleDecl.Let(true, bindings, FsAst.defaultRange)

type FsExpr =

    static member Paren(expr: SynExpr) =
        SynExpr.Paren(expr, FsAst.defaultRange, None, FsAst.defaultRange)

    static member Id(id: string) =
        SynExpr.Ident(new Ident(id, FsAst.defaultRange))

    static member LongId(ids: string list) =
        let ident id = new Ident(id, FsAst.defaultRange)
        SynExpr.LongIdent(
            false,
            LongIdentWithDots.LongIdentWithDots(
                List.map ident ids,
                List.replicate ((ids.Length) - 1) FsAst.defaultRange),
            None,
            FsAst.defaultRange)

    static member ConsSequential(expr0: SynExpr, expr1: SynExpr) =
        SynExpr.Sequential(SequencePointsAtSeq, true, expr0, expr1, FsAst.defaultRange)

    static member Let(bindings: SynBinding list, body: SynExpr) =
        SynExpr.LetOrUse(false, false, bindings, body, FsAst.defaultRange)
        
    static member LetRec(bindings: SynBinding list, body: SynExpr) =
        SynExpr.LetOrUse(true, false, bindings, body, FsAst.defaultRange)

    static member Const(constant: SynConst) =
        SynExpr.Const(constant, FsAst.defaultRange)

    static member Tuple(exprs: SynExpr list) =
        FsExpr.Paren(SynExpr.Tuple(exprs, List.replicate exprs.Length FsAst.defaultRange, FsAst.defaultRange))

    static member List(exprs: SynExpr list) =
        SynExpr.ArrayOrList(false, exprs, FsAst.defaultRange)

    static member Lambda(isInnerLambda: bool, arg: (string * SynType) option, body: SynExpr) =
        let pats =
            match arg with
            | Some(nm, typ) ->
                SynSimplePats.SimplePats(
                    [SynSimplePat.Typed(
                        SynSimplePat.Id(
                            new Ident(nm, FsAst.defaultRange),
                            None,
                            false,
                            false,
                            false,
                            FsAst.defaultRange),
                        typ,
                        FsAst.defaultRange)],
                    FsAst.defaultRange)
            | None ->
                SynSimplePats.SimplePats([], FsAst.defaultRange)
        SynExpr.Paren(
            SynExpr.Lambda(
                false,
                isInnerLambda,
                pats,
                body,
                FsAst.defaultRange),
            FsAst.defaultRange,
            None,
            FsAst.defaultRange)

    static member If(condition: SynExpr, ifTrue: SynExpr, ifFalse: SynExpr) =
        SynExpr.IfThenElse(
            condition,
            ifTrue,
            Some ifFalse,
            SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding,
            false,
            FsAst.defaultRange,
            FsAst.defaultRange)

    static member App(f0: SynExpr, args0: SynExpr list) =
        let rec buildApply f args =
            match args with
            | single :: [] ->
                SynExpr.App(
                    ExprAtomicFlag.NonAtomic,
                    false,
                    f,
                    single,
                    FsAst.defaultRange)
            | first :: rest ->
                buildApply (SynExpr.App(ExprAtomicFlag.NonAtomic, false, f, first, FsAst.defaultRange)) rest
            | _ -> failwith "must have at least one argument"
        FsExpr.Paren(buildApply f0 args0)

    static member Constructor(typeName: string, arg: SynExpr) =
        FsExpr.Paren(SynExpr.New(false, FsType.Of(typeName), arg, FsAst.defaultRange))

    static member Infix(lhs: SynExpr, op: SynExpr, rhs: SynExpr) =
        FsExpr.App(op, [lhs; rhs])

    static member Match(key: SynExpr, clauses: SynMatchClause list) =
        SynExpr.Match(
            SequencePointInfoForBinding.SequencePointAtBinding FsAst.defaultRange,
            key,
            clauses,
            false,
            FsAst.defaultRange)

type FsPat =

    static member Name(nm: string) =
        SynPat.LongIdent(
            LongIdentWithDots.LongIdentWithDots([new Ident(nm, FsAst.defaultRange)], []),
            None,
            None,
            SynConstructorArgs.Pats([]),
            None,
            FsAst.defaultRange)

    static member List(items: SynPat list) =
        SynPat.ArrayOrList(false, items, FsAst.defaultRange)

    static member Wild = SynPat.Wild(FsAst.defaultRange)

type FsMatchClause =

    static member Of(pat: SynPat, body: SynExpr) =
        SynMatchClause.Clause(
            pat,
            None,
            body,
            FsAst.defaultRange,
            SequencePointInfoForTarget.SequencePointAtTarget)

type FsBinding =

    static member Of(symbol: string, body: SynExpr) =
        SynBinding.Binding(
            None,
            SynBindingKind.NormalBinding,
            false,
            false,
            [],
            PreXmlDoc.Empty,
            SynValData.SynValData(
                None,
                SynValInfo.SynValInfo([], SynArgInfo.SynArgInfo([], false, None)),
                None),
            SynPat.Named(
                SynPat.Wild FsAst.defaultRange,
                new Ident(symbol, FsAst.defaultRange),
                false,
                None,
                FsAst.defaultRange),
            None,
            body,
            FsAst.defaultRange,
            SequencePointInfoForBinding.NoSequencePointAtLetBinding)

type FsFail =

    static member With(s: string) =
        FsExpr.App(FsExpr.Id("failwith"), [FsConst.String(s)])

module KlCompiler =
    let sscs = new SimpleSourceCodeServices()
    let checker = FSharpChecker.Create()
    let getUntypedTree (file, input) = 
        // Get compiler options for the 'project' implied by a single script file
        let projOptions = 
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        // Run the first phase (untyped parsing) of the compiler
        let parseFileResults = 
            checker.ParseFileInProject(file, input, projOptions) 
            |> Async.RunSynchronously

        match parseFileResults.ParseTree with
        | Some tree -> tree
        | None -> failwith "Something went wrong during parsing!"
    let idExpr = FsExpr.Id
    let longIdExpr ids =
        let ident id = new Ident(id, FsAst.defaultRange)
        SynExpr.LongIdent(
            false,
            LongIdentWithDots.LongIdentWithDots(
                List.map ident ids,
                List.replicate ((ids.Length) - 1) FsAst.defaultRange),
            None,
            FsAst.defaultRange)
    let rec build expr = // Throws on DefunExpr
        let klToFsId (klId:string) =
            klId.Replace("?", "_P_")
                .Replace("<", "_LT_")
                .Replace(">", "_GT_")
                .Replace("-", "_")
                .Replace(".", "_DOT_")
                .Replace("+", "_PLUS_")
                .Replace("*", "_STAR_")
                .TrimEnd('_')
        let builtin id = longIdExpr ["KlBuiltins"; id]
        let seBool synExpr = FsExpr.App(builtin "vBool", [synExpr])
        let seResult synExpr = FsExpr.App(idExpr "Completed", [FsExpr.App(idExpr "ValueResult", [synExpr])])
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
                longIdExpr ["KlValue"; "FunctionValue"],
                [FsExpr.App(
                    longIdExpr ["Function"; "func"],
                    [FsConst.String("Anonymous")
                     FsConst.Int32(argCount)
                     FsExpr.List([])
                     FsExpr.Lambda(
                        false,
                        Some("envGlobals", FsType.Of("Globals")),
                        lambda)])])
        match expr with
        | EmptyExpr -> longIdExpr ["KlValue"; "EmptyValue"]
        | BoolExpr b -> FsExpr.App(longIdExpr ["KlValue"; "BoolValue"], [SynExpr.Const(SynConst.Bool b, range.Zero)])
        | IntExpr i -> FsExpr.App(longIdExpr ["KlValue"; "IntValue"], [SynExpr.Const(SynConst.Int32 i, range.Zero)])
        | DecimalExpr d -> FsExpr.App(longIdExpr ["KlValue"; "DecimalValue"], [SynExpr.Const(SynConst.Decimal d, range.Zero)])
        | StringExpr s -> FsExpr.App(longIdExpr ["KlValue"; "StringValue"], [FsConst.String (escape s)])
        | SymbolExpr s ->
            if isVar s
                then idExpr (klToFsId s)
                else FsExpr.App(longIdExpr ["KlValue"; "SymbolValue"], [FsConst.String s])
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
        | LambdaExpr(symbol, body) -> //failwith "lambda not impl"
            klFunction 1 (FsExpr.Lambda(false, Some(symbol, FsType.Of("KlValue")), seResult (build body)))
        | DefunExpr(symbol, paramz, body) -> failwith "Defun expr must be at top level"
        | FreezeExpr(expr) -> //failwith "freeze not impl"
            klFunction 1 (FsExpr.Lambda(false, Some("args", FsType.ListOf(FsType.Of("KlValue"))), seResult (build expr)))
        | TrapExpr(_, t, c) -> //failwith "trap not impl"
            FsExpr.App(longIdExpr["KlBuiltins"; "trapError"], [build t; build c])
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
                let builtArgs = [idExpr "envGlobals"; FsExpr.List(List.map build args)]
                match primitiveOp op with
                | Some(pop) -> FsExpr.App(pop, builtArgs)
                | _ when isVar op -> FsExpr.App(idExpr op, builtArgs)
                | _ -> FsExpr.App(longIdExpr["KlImpl"; op], builtArgs)
            | _ -> failwith "application expression or special form must start with symbol"
    let topLevelBuild expr =
        match expr with
        | DefunExpr(symbol, paramz, body) ->
            let typedParamz = ["envGlobals", FsType.Of("Globals"); "args", FsType.ListOf(FsType.Of("KlValue"))]
            FsModule.SingleLet(
                symbol,
                typedParamz,
                FsExpr.Match(idExpr "args",
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
        let isApp = function
            | AppExpr _ -> true
            | _ -> false
        let decls = exprs |> List.filter isDefun |> List.map topLevelBuild
        let init = exprs |> List.filter isApp |> List.map build |> buildInit
        let members = List.append decls [init]
        let openKl = SynModuleDecl.Open(LongIdentWithDots.LongIdentWithDots([new Ident("Kl", FsAst.defaultRange)], []), FsAst.defaultRange)
        FsFile.Of("KlImpl", [FsModule.Of("KlImpl", List.Cons(openKl, members))])
