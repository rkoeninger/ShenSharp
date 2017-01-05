namespace Kl

open ExpressionPatterns

module Optimizer =

    let simplify expr =
        match expr with

        // Eta-reduction on lambda declaration
        | LambdaExpr(param0, AppExpr(f, [Sym param1])) when param0 = param1 -> f
        | _ -> expr
