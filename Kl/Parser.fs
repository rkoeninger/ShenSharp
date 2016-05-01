namespace Kl

open Extensions

/// <summary>
/// The parser turns a token tree into an expression tree.
/// </summary>
module Parser =

    /// <summary>
    /// Parse a token into an expression.
    /// </summary>
    /// <exception>
    /// Throws on invalid cond clauses and defuns with parameters that are not symbols.
    /// </exception>
    let rec parse pos token =
        match token with

        // Literals just get passed through
        | BoolToken b -> BoolExpr b
        | IntToken i  -> IntExpr i
        | DecToken d  -> DecExpr d
        | StrToken s  -> StrExpr s
        | SymToken s  -> SymExpr s

        // ()
        | ComboToken [] -> EmptyExpr

        // (and ~left ~right)
        | ComboToken [(SymToken "and"); left; right] ->
            AndExpr (parse Head left, parse pos right)

        // (or ~left ~right)
        | ComboToken [(SymToken "or");  left; right] ->
            OrExpr (parse Head left, parse pos right)

        // (if ~condition ~consequent ~alternative)
        | ComboToken [(SymToken "if"); condition; consequent; alternative] ->
            IfExpr (parse Head condition, parse pos consequent, parse pos alternative)

        // (cond ~@clauses)
        | ComboToken (SymToken "cond" :: clauses) ->
            let buildClause token =
                match token with
                // (~condition ~consequent)
                | ComboToken [condition; consequent] -> (parse Head condition, parse pos consequent)
                | _ -> failwith "Invalid cond clause"
            List.map buildClause clauses |> CondExpr

        // (let ~name ~binding ~body)
        | ComboToken [(SymToken "let"); (SymToken name); binding; body] ->
            LetExpr (name, parse Head binding, parse pos body)

        // (lambda ~arg ~body)
        | ComboToken [(SymToken "lambda"); (SymToken arg); body] ->
            LambdaExpr (arg, parse Tail body)

        // (freeze ~expr)
        | ComboToken [(SymToken "freeze"); expr] ->
            FreezeExpr (parse Tail expr)

        // (trap-error ~body ~handler)
        | ComboToken [(SymToken "trap-error"); body; handler] ->
            TrapExpr (pos, parse Head body, parse pos handler)

        // (defun ...)
        | ComboToken(SymToken "defun" :: _) ->
            failwith "defun expressions cannot appear below the root level"

        // (~f ~@args)
        | ComboToken (f :: args) ->
            AppExpr (pos, parse Head f, List.map (parse Head) args)
            
    /// <summary>
    /// Parse a token into a root-level expression.
    /// </summary>
    /// <exception>
    /// Throws on invalid cond clauses and defuns with parameters that are not symbols.
    /// </exception>
    let rootParse token =
        match token with

        // (defun ~name ~paramz ~body)
        | ComboToken [SymToken "defun"; SymToken name; ComboToken paramz; body] ->
            let paramName t =
                match t with
                | SymToken s -> s
                | _ -> failwith "Defun parameters must be symbols"
            DefunExpr (name, List.map paramName paramz, parse Tail body)

        // Any other expr
        | _ -> OtherExpr(parse Head token)

    /// <summary>
    /// Converts an expression back into a token.
    /// </summary>
    let rec unparse expr =
        match expr with
        | BoolExpr b -> BoolToken b
        | IntExpr i -> IntToken i
        | DecExpr d -> DecToken d
        | StrExpr s -> StrToken s
        | SymExpr s -> SymToken s
        | EmptyExpr -> ComboToken []
        | AndExpr(left, right) ->
            ComboToken [SymToken "and"; unparse left; unparse right]
        | OrExpr(left, right) ->
            ComboToken [SymToken "or"; unparse left; unparse right]
        | IfExpr(condition, consequent, alternative) ->
            ComboToken [SymToken "if"; unparse condition; unparse consequent; unparse alternative]
        | CondExpr(clauses) ->
            let unparseClause (condition, consequent) = ComboToken [unparse condition; unparse consequent]
            ComboToken (List.Cons(SymToken "cond", List.map unparseClause clauses))
        | LetExpr(symbol, binding, value) ->
            ComboToken [SymToken "let"; SymToken symbol; unparse binding; unparse value]
        | LambdaExpr(param, body) ->
            ComboToken [SymToken "lambda"; SymToken param; unparse body]
        | FreezeExpr body ->
            ComboToken [SymToken "freeze"; unparse body]
        | TrapExpr(_, body, handler) ->
            ComboToken [SymToken "trap-error"; unparse body; unparse handler]
        | AppExpr(_, f, args) ->
            ComboToken (List.Cons(unparse f, List.map unparse args))

    /// <summary>
    /// Converts a root expression back into a token.
    /// </summary>
    let rootUnparse expr =
        match expr with
        | DefunExpr(name, paramz, body) ->
            ComboToken [SymToken "defun"; SymToken name; ComboToken(List.map SymToken paramz); unparse body]
        | OtherExpr expr -> unparse expr
