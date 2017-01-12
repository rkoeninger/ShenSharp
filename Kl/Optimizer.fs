namespace Kl

open Values
open ExpressionPatterns

module Optimizer =

    let simplify expr =
        match expr with

        // Eta-reduction on lambda declaration
        //| LambdaExpr(param, AppExpr(f, [Sym arg])) when param = arg -> f
        //| LambdaExpr(param, AppExpr(f, (Last(Sym arg) as args))) when param = arg -> Cons(f, toCons(butLast args))

        // Pre-empt tail call optimization
        | DefunExpr(name, paramz, body) ->
            let rec isTailRecursive expr =
                match expr with
                | IfExpr(_, consequent, alternative) -> isTailRecursive consequent || isTailRecursive alternative
                | CondExpr clauses -> List.exists (snd >> isTailRecursive) clauses
                | LetExpr(_, _, body) -> isTailRecursive body
                | DoExpr(_, second) -> isTailRecursive second
                | AppExpr(Sym id, _) when id = name -> true
                | _ -> false
            if isTailRecursive body then
                let IfExpr(condition, consequent, alternative) = toCons [Sym "if"; condition; consequent; alternative]
                let CondExpr clauses = toCons(Sym "cond" :: List.map (fun (condition, consequent) -> toCons [condition; consequent]) clauses)
                let LetExpr(symbol, binding, body) = toCons [Sym "let"; Sym symbol; binding; body]
                let DefunExpr(name, paramz, body) = toCons [Sym "defun"; Sym name; toCons (List.map Sym paramz); body]
                let DoExpr(first, second) = toCons [Sym "do"; first; second]
                let AppExpr(f, args) = toCons(f :: args)
                let FreezeExpr expr = toCons [Sym "freeze"; expr]
                let LambdaExpr(param, body) = toCons [Sym "lambda"; Sym param; body]
                let LoopExpr(init, f) = toCons [Sym "loop"; init; f]
                let rec buildConses = function
                    | [] -> Empty
                    | x :: xs -> AppExpr(Sym "cons", [x; buildConses xs])
                let rec buildLambdas args body =
                    match args with
                    | [] -> body
                    | arg :: args -> LambdaExpr(arg, buildLambdas args body)
                let rec intoLoop expr =
                    match expr with
                    | IfExpr(condition, consequent, alternative) ->
                        IfExpr(condition, intoLoop consequent, intoLoop alternative)
                    | CondExpr clauses ->
                        let mapClause (condition, consequent) = (condition, intoLoop consequent)
                        CondExpr(List.map mapClause clauses)
                    | LetExpr(symbol, binding, body) ->
                        LetExpr(symbol, binding, intoLoop body)
                    | DoExpr(first, second) ->
                        DoExpr(first, intoLoop second)
                    | AppExpr(Sym id, args) when id = name ->
                        // (cons true (cons (cons arg0 (cons arg1 ())) ()))
                        AppExpr(Sym "cons", [Bool true; buildConses args])
                    | _ ->
                        // (cons false (cons result ()))
                        AppExpr(Sym "cons", [Bool false; AppExpr(Sym "cons", [expr; Empty])])
                if paramz.IsEmpty then
                    // (defun name ()
                    //     (loop
                    //         ()
                    //         (freeze transformed-body)))
//                    DefunExpr(name, paramz,
//                        LoopExpr(
//                            Empty,
//                            FreezeExpr(intoLoop body)))
                    failwith "zero-arg defuns not supported"
                else
                    // (defun name (A0 A1 A2)
                    //     (loop
                    //         (cons A0 (cons A1 (cons A2 ())))
                    //         (lambda A0 (lambda A1 (lambda A2
                    //             transformed-body)))))
//                    DefunExpr(name, paramz,
//                        LoopExpr(
//                            buildConses (List.map Sym paramz),
//                            buildLambdas paramz (intoLoop body)))
                    DefunExpr(name, paramz, toCons [
                                                Sym "loop"
                                                toCons (List.map Sym paramz)
                                                toCons (List.map Sym paramz)
                                                intoLoop body])
            else
                body

        | _ -> expr
