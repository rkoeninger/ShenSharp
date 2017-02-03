namespace Kl.Import

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Kl
open Kl.Expressions

module Compiler =

    let private nullRange = mkFileIndexRange 50 (mkPos 100 100) (mkPos 200 200)

    let private bool b = SynExpr.Const(SynConst.Bool b, nullRange)
    let private num x = SynExpr.Const(SynConst.Decimal x, nullRange)
    let private str s = SynExpr.Const(SynConst.String(s, nullRange), nullRange)
    let private id name = SynExpr.Ident(new Ident(name, nullRange))
    let private parens e = SynExpr.Paren(e, nullRange, None, nullRange)
    let private tuple vals = SynExpr.Tuple(vals, List.replicate (List.length vals - 1) nullRange, nullRange)
    let private list vals = SynExpr.ArrayOrList(true, vals, nullRange)

    let private app fExpr argExpr =
        SynExpr.App(
            ExprAtomicFlag.NonAtomic,
            false,
            fExpr,
            argExpr,
            nullRange)

    let private infix op left right =
        SynExpr.App(
            ExprAtomicFlag.NonAtomic,
            false,
            SynExpr.App(
                ExprAtomicFlag.NonAtomic,
                true,
                op,
                left,
                nullRange),
            right,
            nullRange)

    let private branch condition consequent alternative =
        SynExpr.IfThenElse(
            condition,
            consequent,
            Some alternative,
            SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding,
            false,
            nullRange,
            nullRange)

    let private var name value body =
        SynExpr.LetOrUse(
            false,
            false,
            [SynBinding.Binding(
                None,
                SynBindingKind.NormalBinding,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                SynValData.SynValData(
                    None,
                    SynValInfo.SynValInfo(
                        [],
                        SynArgInfo.SynArgInfo(
                            [],
                            false,
                            None)),
                    None),
                SynPat.Named(
                    SynPat.Wild nullRange,
                    new Ident(name, nullRange),
                    false,
                    None,
                    nullRange),
                None,
                value,
                nullRange,
                SequencePointInfoForBinding.NoSequencePointAtLetBinding)],
            body,
            nullRange)

    let private tryWith body e handler =
        SynExpr.TryWith(
            body,
            nullRange,
            [SynMatchClause.Clause(
                SynPat.Paren(
                    SynPat.LongIdent(
                        LongIdentWithDots.LongIdentWithDots(
                            [new Ident(e, nullRange)],
                            []),
                        None,
                        None,
                        SynConstructorArgs.Pats [],
                        None,
                        nullRange),
                    nullRange),
                None,
                handler,
                nullRange,
                SequencePointInfoForTarget.SequencePointAtTarget)],
            nullRange,
            nullRange,
            SequencePointInfoForTry.SequencePointAtTry nullRange,
            SequencePointInfoForWith.SequencePointAtWith nullRange)

    type private ExprType =
        | Bottom
        | KlValue
        | FsBoolean
        | FsUnit

    // needs application context to know for which function there
    // is an argument type error
    let private convert targetType (fsExpr, currentType) =
        match currentType, targetType with
        | x, y when x = y -> fsExpr
        | Bottom, _ -> fsExpr
        | FsBoolean, KlValue -> app (id "Bool") fsExpr
        | FsUnit, KlValue -> id "Empty"
        | KlValue, FsBoolean -> app (id "isTrue") fsExpr
        | _, FsUnit -> infix (id "|>") fsExpr (id "ignore")
        | _, _ -> failwithf "can't convert %O to %O" currentType targetType

    let private (|>>) fsExprWithType targetType = convert targetType fsExprWithType

    // The type of the last expression is the type of the whole sequence
    let private dos exprsWithTypes =
        let rec buildDos = function
            | [exprWithType] -> exprWithType
            | exprWithType :: rest ->
                let rest, restType = buildDos rest
                SynExpr.Sequential(
                    SequencePointInfoForSeq.SequencePointsAtSeq,
                    false,
                    exprWithType |>> FsUnit,
                    rest,
                    nullRange), restType
            | [] -> failwith "do must have at least 2 statements"
        let seqExpr, lastType = buildDos exprsWithTypes
        SynExpr.Do(seqExpr, nullRange), lastType

    let private rename s = "kl_" + s

    let rec private flattenDo = function
        | DoExpr(first, second) -> flattenDo first @ flattenDo second
        | klExpr -> [klExpr]

    // TODO: make sure error messages and conditions are
    //       consistent with Evaluator
    //       needs application context for this
    let rec private build ((globals, locals) as context) = function
        | Empty -> id "Empty", KlValue
        | Num x -> app (id "Num") (num x), KlValue
        | Str s -> app (id "Str") (str s), KlValue
        | Sym "true" -> bool true, FsBoolean
        | Sym "false" -> bool false, FsBoolean
        | Sym s ->
            if Set.contains s locals
                then id (rename s), KlValue
                else app (id "Sym") (str s), KlValue
        | AndExpr(left, right) ->
            infix (id "&&")
                  (build context left  |>> FsBoolean)
                  (build context right |>> FsBoolean), FsBoolean
        | OrExpr(left, right) ->
            infix (id "||")
                  (build context left  |>> FsBoolean)
                  (build context right |>> FsBoolean), FsBoolean

        // TODO: Find a way to avoid converting result types to KlValue
        | IfExpr(condition, consequent, alternative) ->
            branch (build context condition   |>> FsBoolean)
                   (build context consequent  |>> KlValue)
                   (build context alternative |>> KlValue), KlValue
        | CondExpr clauses ->
            let rec compileClauses = function
                | [] -> id "Empty" // TODO: is this the right way?
                                   // needs to be consistent with Evaluator
                | (Sym "true", consequent) :: _ -> build context consequent |>> KlValue
                | (condition, consequent) :: rest ->
                    branch (build context condition  |>> FsBoolean)
                           (build context consequent |>> KlValue)
                           (compileClauses rest)
            compileClauses clauses, KlValue
        | LetExpr(param, binding, body) ->
            // TODO: might be able to put let on its own line instead of part of expr, depends on context
            // TODO: need to optimize types
            var param
                (build context binding |>> KlValue)
                (build (globals, Set.add param locals) body |>> KlValue), KlValue
        | DoExpr _ as doExpr -> dos (List.map (build context) (flattenDo doExpr))
        | LambdaExpr(param, body) -> failwith "can't compile"
            // Globals -> Value -> Value
            // app (id "Func")
            //     (app (id "CompiledLambda") (lamb ["_gs"; param] (build context body |>> KlValue)))
        | FreezeExpr body -> failwith "can't compile"
            // Globals -> Value
            // app (id "Func")
            //     (app (id "CompiledFreeze") (lamb ["_gs"] (build context body |>> KlValue)))
        | TrapExpr(body, handler) -> failwith "can't compile" // try...with, need to join branch types
        | DefunExpr _ -> failwith "can't compile defun in expr position" // or can we?

        | AppExpr(Sym s, args) ->
            app (app (id (rename s)) (id "_gs"))
                (list (List.map (build context >> convert KlValue) args)), KlValue

        // TODO: if it's some other expression, we need an apply function
        | AppExpr(f, args) -> failwith "can't compile"

        | _ -> failwith "Unable to compile"

    let private compileDefun globals name paramz body = () // Defun -> module let binding

    let compile globals = () // return a module definition
