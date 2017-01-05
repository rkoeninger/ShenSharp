namespace Kl

open Values
open ExpressionPatterns

module Optimizer =

    let simplify expr =
        match expr with

        // Eta-reduction on lambda declaration
        | LambdaExpr(param, AppExpr(f, [Sym arg])) when param = arg -> f
        | LambdaExpr(param, AppExpr(f, (Last(Sym arg) as args))) when param = arg -> Cons(f, toCons(butLast args))
        | _ -> expr
