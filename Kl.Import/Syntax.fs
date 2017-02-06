namespace Kl.Import

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

module Syntax =

    // Picked large values for line, col because there will be an unpredictable
    // ArrayIndexOutOfBoundsException if the numbers are too small
    let private range fn = mkRange fn (mkPos 512 512) (mkPos 1024 1024)
    let ident fn s = new Ident(s, range fn)
    let longIdent fn parts = List.map (ident fn) parts
    let longIdentWithDots fn parts =
        LongIdentWithDots.LongIdentWithDots(
            List.map (ident fn) parts,
            List.replicate (List.length parts - 1) (range fn))
    let longType fn parts = SynType.LongIdent(longIdentWithDots fn parts)
    let namePat fn s = SynPat.Named(SynPat.Wild(range fn), ident fn s, false, None, range fn)
    let typedPat fn pat synType = SynPat.Paren(SynPat.Typed(pat, synType, range fn), range fn)
    let simpleBinding fn pat value =
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
            pat,
            None,
            value,
            range fn,
            SequencePointInfoForBinding.NoSequencePointAtLetBinding)
    let boolExpr fn b = SynExpr.Const(SynConst.Bool b, range fn)
    let decimalExpr fn n = SynExpr.Const(SynConst.Decimal n, range fn)
    let stringExpr fn s = SynExpr.Const(SynConst.String(s, range fn), range fn)
    let idExpr fn s = SynExpr.Ident(ident fn s)
    let parenExpr fn expr = SynExpr.Paren(expr, range fn, None, range fn)
    let listExpr fn vals = SynExpr.ArrayOrList(false, vals, range fn)
    let appExpr fn f arg = SynExpr.App(ExprAtomicFlag.NonAtomic, false, f, arg, range fn)
    let infixExpr fn op left right =
        SynExpr.App(
            ExprAtomicFlag.NonAtomic,
            false,
            SynExpr.App(ExprAtomicFlag.NonAtomic, true, op, left, range fn),
            right,
            range fn)
    let ifExpr fn condition consequent alternative =
        SynExpr.IfThenElse(
            condition,
            consequent,
            Some alternative,
            SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding,
            false,
            range fn,
            range fn)
    let letExpr fn symbol value body =
        SynExpr.LetOrUse(
            false,
            false,
            [simpleBinding fn symbol value],
            body,
            range fn)
    let tryWithExpr fn body e handler =
        SynExpr.TryWith(
            body,
            range fn,
            [SynMatchClause.Clause(
                namePat fn e,
                None,
                handler,
                range fn,
                SequencePointInfoForTarget.SequencePointAtTarget)],
            range fn,
            range fn,
            SequencePointInfoForTry.SequencePointAtTry(range fn),
            SequencePointInfoForWith.SequencePointAtWith(range fn))
    let rec sequentialExpr fn = function
        | [] -> failwith "sequential cannot be empty"
        | [expr] -> expr
        | expr :: rest ->
            SynExpr.Sequential(
                SequencePointInfoForSeq.SequencePointsAtSeq,
                false,
                expr,
                sequentialExpr fn rest,
                range fn)
    let doExpr fn expr = SynExpr.Do(expr, range fn)
    let openDecl fn parts = SynModuleDecl.Open(longIdentWithDots fn parts, range fn)
    let letDecl fn name paramz body =
        SynModuleDecl.Let(false,
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
                        List.map
                            (fun s -> [SynArgInfo.SynArgInfo([], false, Some(ident fn s))])
                            paramz,
                        SynArgInfo.SynArgInfo([], false, None)),
                    None),
                SynPat.LongIdent(
                    longIdentWithDots fn [name],
                    None,
                    None,
                    SynConstructorArgs.Pats(
                        List.map
                            (fun s -> typedPat fn (namePat fn s) (longType fn ["Kl"; "Value"]))
                            paramz),
                    None,
                    range fn),
                None,
                body,
                range fn,
                SequencePointInfoForBinding.SequencePointAtBinding(range fn))],
            range fn)
    let parsedFile fn decls =
        ParsedInput.ImplFile(
            ParsedImplFileInput.ParsedImplFileInput(
                fn + ".fs",
                false,
                QualifiedNameOfFile.QualifiedNameOfFile(ident fn fn),
                [],
                [],
                [SynModuleOrNamespace.SynModuleOrNamespace(
                    [ident fn fn],
                    false,
                    true,
                    decls,
                    PreXmlDoc.Empty,
                    [],
                    None,
                    range fn)],
                (false, false)))
