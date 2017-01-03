namespace Kl

open FParsec

/// <summary>The Reader parses KL source code into a value.</summary>
/// <remarks>Reader is strict about spacing. It will not handle extra spaces inside of parens.</remarks>
module Reader =

    let rec private consList list =
        match list with
        | [] -> Empty
        | x :: xs -> Cons(x, consList xs)

    let private pValue, pValueRef = createParserForwardedToRef<Value, unit>()
    let private pBool = (stringReturn "true" (Bool true)) <|> (stringReturn "false" (Bool false))
    let private pInt = regex "[-+]?[0-9]*" |>> (int >> Int)
    let private pDec = regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" |>> (decimal >> Dec)
    let private pSym = regex "[^\\s\\x28\\x29]+" |>> Sym
    let private pStr = between (pchar '"') (pchar '"') (manySatisfy ((<>) '"')) |>> Str
    let private pList = between (pchar '(') (pchar ')') (sepBy pValue spaces1) |>> consList
    let private pValues = spaces >>. (many (pValue .>> spaces))
    do pValueRef := choice [pBool; pInt; pDec; pSym; pStr; pList]
    
    /// <summary>Read first complete KL source expression into value.</summary>
    /// <exception>Throws when syntax is invalid.</exception>
    let read s =
        match run pValue s with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error

    /// <summary>Read all KL source expressions into a list of values.</summary>
    /// <exception>Throws when syntax is invalid.</exception>
    let tokenizeAll s =
        match run pValues s with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error
