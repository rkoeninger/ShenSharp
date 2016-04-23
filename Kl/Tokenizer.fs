namespace Kl

open FParsec

/// <summary>The tokenizer turns KL source code into a token tree.</summary>
/// <remarks>Tokenizer is strict about spacing. It will not handle extra spaces inside of parens.</remarks>
module Tokenizer =

    let private pToken, pTokenRef = createParserForwardedToRef<KlToken, unit>()
    let private pBool = (stringReturn "true" (BoolToken true)) <|> (stringReturn "false" (BoolToken false))
    let private pNumber = regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" |>> (decimal >> NumberToken)
    let private stringLiteral = between (pchar '"') (pchar '"') (manySatisfy ((<>) '"'))
    let private pString = stringLiteral |>> StringToken
    let private pSymbol = regex "[^\\s\\x28\\x29]+" |>> SymbolToken
    let private pCombo = between (pchar '(') (pchar ')') (sepBy pToken spaces1) |>> ComboToken
    let private pTokens = spaces >>. (many (pToken .>> spaces))
    do pTokenRef := choice [pBool; pNumber; pString; pSymbol; pCombo]

    /// <summary>Tokenize first complete KL source expression into token tree.</summary>
    /// <exception>Throws when syntax is invalid.</exception>
    let tokenize s =
        match run pToken s with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error

    /// <summary>Tokenize all KL source expressions into a list of token trees.</summary>
    /// <exception>Throws when syntax is invalid.</exception>
    let tokenizeAll s =
        match run pTokens s with
        | Success(result, _, _) -> result
        | Failure(error, _, _) -> failwith error
