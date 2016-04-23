namespace Kl

open FParsec

// Tokenizer is strict about spacing. It will not handle extra spaces inside of parens.
module KlTokenizer =
    let pKlToken, pKlTokenRef = createParserForwardedToRef<KlToken, unit>()
    let pKlBool = (stringReturn "true" (BoolToken true)) <|> (stringReturn "false" (BoolToken false))
    let pKlNumber = regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" |>> (decimal >> NumberToken)
    let stringLiteral = between (pchar '"') (pchar '"') (manySatisfy ((<>) '"'))
    let pKlString = stringLiteral |>> StringToken
    let pKlSymbol = regex "[^\\s\\x28\\x29]+" |>> SymbolToken
    let pKlCombo = between (pchar '(') (pchar ')') (sepBy pKlToken spaces1) |>> ComboToken
    do pKlTokenRef := choice [pKlBool; pKlNumber; pKlString; pKlSymbol; pKlCombo]
    let tokenize s = run pKlToken s |> function
                                       | Success(result, _, _) -> result
                                       | Failure(error, _, _) -> failwith error
    let pKlTokens = spaces >>. (many (pKlToken .>> spaces))
    let tokenizeAll s = run pKlTokens s |> function
                                           | Success(result, _, _) -> result
                                           | Failure(error, _, _) -> failwith error
