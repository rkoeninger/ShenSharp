namespace Kl

open Values

module Expressions =
    let private sequenceOption xs =
        let combine x xs = Option.bind (fun v -> Option.map (fun vs -> v :: vs) xs) x
        List.foldBack combine xs (Some [])

    let rec private toListOption cons =
        match cons with
        | Empty -> Some []
        | Cons(x, y) -> Option.map (fun xs -> x :: xs) (toListOption y)
        | _ -> None

    let private (|Expr|_|) = toListOption

    let (|AndExpr|_|) = function
        | Expr [Sym "and"; left; right] -> Some(left, right)
        | _ -> None

    let AndExpr(left, right) = toCons [Sym "and"; left; right]

    let (|OrExpr|_|) = function
        | Expr [Sym "or"; left; right] -> Some(left, right)
        | _ -> None

    let OrExpr(left, right) = toCons [Sym "or"; left; right]

    let (|IfExpr|_|) = function
        | Expr [Sym "if"; condition; consequent; alternative] -> Some(condition, consequent, alternative)
        | _ -> None

    let IfExpr(condition, consequent, alternative) = toCons [Sym "if"; condition; consequent; alternative]

    let private condClause = function
        | Expr [x; y] -> Some(x, y)
        | _ -> None

    let private (|CondClauses|_|) = List.map condClause >> sequenceOption

    let (|CondExpr|_|) = function
        | Expr(Sym "cond" :: CondClauses clauses) -> Some clauses
        | _ -> None

    let CondExpr clauses = Cons(Sym "cond", toCons clauses)

    let (|LetExpr|_|) = function
        | Expr [Sym "let"; Sym symbol; binding; body] -> Some(symbol, binding, body)
        | _ -> None

    let LetExpr(symbol, binding, body) = toCons [Sym "let"; Sym symbol; binding; body]

    let (|LambdaExpr|_|) = function
        | Expr [Sym "lambda"; Sym symbol; body] -> Some(symbol, body)
        | _ -> None

    let LambdaExpr(symbol, body) = toCons [Sym "lambda"; Sym symbol; body]

    let (|FreezeExpr|_|) = function
        | Expr [Sym "freeze"; body] -> Some body
        | _ -> None

    let FreezeExpr body = toCons [Sym "freeze"; body]

    let (|TrapExpr|_|) = function
        | Expr [Sym "trap-error"; body; handler] -> Some(body, handler)
        | _ -> None

    let TrapExpr(body, handler) = toCons [Sym "trap-error"; body; handler]

    let private param = function
        | Sym s -> Some s
        | _ -> None

    let private (|ParamList|_|) = toListOption >> Option.bind (List.map param >> sequenceOption)

    let (|DefunExpr|_|) = function
        | Expr [Sym "defun"; Sym name; ParamList paramz; body] -> Some(name, paramz, body)
        | _ -> None

    let DefunExpr(name, paramz, body) =
        toCons [Sym "defun"; Sym name; toCons(List.map Sym paramz); body]

    let (|DoExpr|_|) = function
        | Expr [Sym "do"; first; second] -> Some(first, second)
        | _ -> None

    let DoExpr(first, second) = toCons [Sym "do"; first; second]

    let (|AppExpr|_|) = function
        | Expr(f :: args) -> Some(f, args)
        | _ -> None

    let AppExpr(f, args) = toCons(f :: args)
