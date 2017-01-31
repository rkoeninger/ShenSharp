namespace Kl.Import

open Kl
open Kl.Expressions

module Compiler =

    type ExprType =
        | KlValue
        | FsBoolean
        | FsDecimal
        | FsString
        | FsIdentifier

    // needs application context to know for which function there
    // is an argument type error
    let private convert fsExpr currentType targetType =
        match currentType, targetType with
        | FsBoolean, KlValue -> () // App(Id("Bool"), fsExpr)
        | FsDecimal, KlValue -> () // App(Id("Dec"), fsExpr)
        | FsString, KlValue -> () // App(Id("Str"), fsExpr)
        | KlValue, FsBoolean -> () // App(Id("isTrue"), fsExpr)
        | KlValue, FsDecimal -> () // App(Id("asDecimal"), fsExpr)
        | KlValue, FsString -> () // App(Id("asString"), fsExpr)
        | x, y when x = y -> () // fsExpr // do nothing
        | _, _ -> failwithf "can't convert %O to %O" currentType targetType

    let rec private unnestDo = function
        | DoExpr(first, second) -> List.append (unnestDo first) (unnestDo second)
        | klExpr -> [klExpr]

    // needs lexical scope and application context
    let rec private compileExpr globals klExpr = // Value -> FsExpr, ExprType
        match klExpr with
        | Empty -> () // Id("Empty"), KlValue
        | Num x -> () // Const(Decimal(x)), FsDecimal
        | Str s -> () // Const(String(x)), FsString
        | Sym "true" -> () // Const(Boolean(true)), FsBoolean
        | Sym "false" -> () // Const(Boolean(false)), FsBoolean
        | Sym s -> () // idle symbol: App(UnionCase("Value", "Sym"), Const(String(x))), KlValue
                      // local definition: Id(ConvertNameToFs(s)), FsIdentifier
        | AndExpr(left, right) -> () // left && right, FsBoolean
        | OrExpr(left, right) -> () // left || right, FsBoolean
        // obscure optimizations - does this actually happen?
        | IfExpr(Sym "true", consequent, _) -> compileExpr globals consequent
        | IfExpr(Sym "false", _, alternative) -> compileExpr globals alternative
        | IfExpr(condition, consequent, alternative) -> ()
            // if ConvertToFsBool(condition) then consequent else alternative
            // need to join branch types, depends on context
        | CondExpr clauses ->
            let compileClauses = function
                | [] -> () // Id("Empty")
                | (Sym "true", consequent) :: _ -> () // consequent - optimization
                | (Sym "false", _) :: rest -> () // rest - this doesn't happen
                | (condition, consequent) :: rest -> () // if condition then consequent else rest
            compileClauses clauses // need to join branch types, depends on context
        | LetExpr(param, binding, body) -> () // might be able to put let on its own line instead of
                                              // part of expr, depends on context
        | DoExpr _ as doExpr -> () // unnestDo doExpr // ignore result of all but last
        | LambdaExpr(param, body) -> () // build CompiledLambda: Globals -> Value -> Value
        | FreezeExpr body -> () // build CompiledFreeze: Globals -> Value
        | TrapExpr(body, handler) -> () // try...with, need to join branch types
        | DefunExpr _ -> failwith "can't compile defun in expr position" // or can we?
        | AppExpr(Sym "+", [x; y]) -> () // (convert FsDecimal x + convert FsDecimal y), FsDecimal
        | AppExpr(Sym "intern", [x]) -> () // App(UnionCase("Value", "Sym"), convert FsString x)
        | AppExpr(f, args) -> () // ``f`` globals [arg0; arg1], KlValue
        | _ -> failwith "Unable to compile"

    let private compileDefun globals name paramz body = () // Defun -> module let binding

    let compile globals = () // return a module definition
