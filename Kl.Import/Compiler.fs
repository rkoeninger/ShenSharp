namespace Kl.Import

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Kl
open Kl.Expressions

module Compiler =

    type ExprType =
        | Bottom
        | KlValue
        | FsBoolean
        | FsDecimal
        | FsString
        | FsUnit

    let nullRange = mkFileIndexRange 50 (mkPos 100 100) (mkPos 200 200)

    let private id name = SynExpr.Ident(new Ident(name, nullRange))

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

    let private parens e = SynExpr.Paren(e, nullRange, None, nullRange)

    let private branch condition consequent alternative =
        SynExpr.IfThenElse(
            condition,
            consequent,
            Some alternative,
            SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding,
            false,
            nullRange,
            nullRange)

    // needs application context to know for which function there
    // is an argument type error
    let private convert targetType (fsExpr, currentType) =
        match currentType, targetType with
        | x, y when x = y -> fsExpr
        | Bottom, _ -> fsExpr
        | FsBoolean, KlValue -> app (id "Bool") fsExpr
        | FsDecimal, KlValue -> app (id "Dec") fsExpr
        | FsString, KlValue -> app (id "Str") fsExpr
        | FsUnit, KlValue -> id "Empty"
        | KlValue, FsBoolean -> app (id "isTrue") fsExpr
        | KlValue, FsDecimal -> app (id "asDecimal") fsExpr
        | KlValue, FsString -> app (id "asString") fsExpr
        | _, FsUnit -> infix (id "|>") fsExpr (id "ignore")
        | _, _ -> failwithf "can't convert %O to %O" currentType targetType

    let rec private flattenDo = function
        | DoExpr(first, second) -> List.append (flattenDo first) (flattenDo second)
        | klExpr -> [klExpr]

    // needs lexical scope and application context
    let rec private build globals klExpr = // Value -> FsExpr, ExprType
        match klExpr with
        | Empty -> () // id "Empty", KlValue
        | Num x -> () // SynExpr.Const(SynConst.Decimal x, nullRange), FsDecimal
        | Str s -> () // SynExpr.Const(SynConst.String x, nullRange), FsString
        | Sym "true" -> () // SynExpr.Const(SynConst.Boolean true, nullRange), FsBoolean
        | Sym "false" -> () // SynExpr.Const(SynConst.Boolean false, nullRange), FsBoolean
        | Sym s -> () // idle symbol: App(UnionCase("Value", "Sym"), Const(String(x))), KlValue
                      // local definition: Id(ConvertNameToFs(s)), KlValue
        | AndExpr(left, right) -> ()
            // infix (id "&&")
            //       (convert FsBoolean (build globals left))
            //       (convert FsBoolean (build globals right))
            //   , FsBoolean
        | OrExpr(left, right) -> ()
        // obscure optimizations - does this actually happen?
        | IfExpr(Sym "true", consequent, _) -> build globals consequent
        | IfExpr(Sym "false", _, alternative) -> build globals alternative
        | IfExpr(condition, consequent, alternative) -> ()
            // branch (convert FsBoolean (build condition))
            //        (build consequent)
            //        (build alternative)
            // need to join branch types, depends on context - just make KlValue?
        | CondExpr clauses ->
            let compileClauses = function
                | [] -> () // id "Empty"
                | (Sym "true", consequent) :: _ -> () // consequent - optimization
                | (Sym "false", _) :: rest -> () // rest - this doesn't happen
                | (condition, consequent) :: rest -> () // if condition then consequent else rest
            compileClauses clauses // need to join branch types, depends on context
        | LetExpr(param, binding, body) -> () // might be able to put let on its own line instead of
                                              // part of expr, depends on context
        | DoExpr _ as doExpr -> () // flattenDo doExpr // ignore result of all but last
        | LambdaExpr(param, body) -> () // build CompiledLambda: Globals -> Value -> Value
        | FreezeExpr body -> () // build CompiledFreeze: Globals -> Value
        | TrapExpr(body, handler) -> () // try...with, need to join branch types
        | DefunExpr _ -> failwith "can't compile defun in expr position" // or can we?
        | AppExpr(Sym "+", [x; y]) -> () // (convert FsDecimal x + convert FsDecimal y), FsDecimal
        | AppExpr(Sym "intern", [x]) -> () // App(UnionCase("Value", "Sym"), convert FsString x)
        | AppExpr(Sym "cn", [x; y]) -> ()
            // (build x |> convert FsString) + (build y |> convert FsString), FsString
        | AppExpr(Sym "/", [x; y]) -> ()
            // should throw DivisionByZeroException as usual,
            // trap-error needs to catch all exceptions,
            // klDivide should not have special case for /0
        // | Expr [Sym "cons"; x; y] -> ()
        | AppExpr(Sym "cons", [x; y]) -> ()
            // app (id "Cons") (parens (tuple2 (build x |> convert KlValue) (build y |> convert KlValue)))
        | AppExpr(f, args) -> () // ``f`` globals [arg0; arg1], KlValue
        | _ -> failwith "Unable to compile"

    let private compileDefun globals name paramz body = () // Defun -> module let binding

    let compile globals = () // return a module definition
