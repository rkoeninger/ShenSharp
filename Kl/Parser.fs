namespace Kl

open Extensions

/// <summary>The parser turns a token tree into an expression tree.</summary>
module Parser =

    /// <summary>Parse a token into an expression.</summary>
    /// <exception>Throws on invalid cond clauses and defuns with parameters that are not symbols.</exception>
    let rec parse pos token =
        match token with

        // Literals just get passed through
        | BoolToken b -> BoolExpr b
        | NumberToken n ->
            match n with
            | Integral -> IntExpr(int n)
            | Fractional -> DecimalExpr n
        | StringToken s -> StringExpr s
        | SymbolToken s -> SymbolExpr s

        // ()
        | ComboToken [] -> EmptyExpr

        // (and ~left ~right)
        | ComboToken [(SymbolToken "and"); left; right] ->
            AndExpr (parse Head left, parse pos right)

        // (or ~left ~right)
        | ComboToken [(SymbolToken "or");  left; right] ->
            OrExpr (parse Head left, parse pos right)

        // (if ~condition ~consequent ~alternative)
        | ComboToken [(SymbolToken "if"); condition; consequent; alternative] ->
            IfExpr (parse Head condition, parse pos consequent, parse pos alternative)

        // (cond ~@clauses)
        | ComboToken (SymbolToken "cond" :: clauses) ->
            let buildClause token =
                match token with
                // (~condition ~consequent)
                | ComboToken [condition; consequent] -> (parse Head condition, parse pos consequent)
                | _ -> failwith "Invalid cond clause"
            List.map buildClause clauses |> CondExpr

        // (let ~name ~binding ~body)
        | ComboToken [(SymbolToken "let"); (SymbolToken name); binding; body] ->
            LetExpr (name, parse Head binding, parse pos body)

        // (lambda ~arg ~body)
        | ComboToken [(SymbolToken "lambda"); (SymbolToken arg); body] ->
            LambdaExpr (arg, parse Tail body)

        // (defun ~name ~paramz ~body)
        | ComboToken [(SymbolToken "defun"); (SymbolToken name); (ComboToken paramz); body] ->
            let tSymbol t =
                match t with
                | (SymbolToken s) -> s
                | _ -> failwith "Defun parameters must be symbols"
            DefunExpr (name, List.map tSymbol paramz, parse Tail body)

        // (freeze ~expr)
        | ComboToken [(SymbolToken "freeze"); expr] ->
            FreezeExpr (parse Tail expr)

        // (trap-error ~body ~handler)
        | ComboToken [(SymbolToken "trap-error"); body; handler] ->
            TrapExpr (pos, parse Head body, parse pos handler)

        // (~f ~@args)
        | ComboToken (f :: args) ->
            AppExpr (pos, parse Head f, List.map (parse Head) args)
