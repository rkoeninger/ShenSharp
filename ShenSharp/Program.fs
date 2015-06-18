#light

namespace ShenPOF

open FParsec

type KlToken = BoolToken   of bool
             | NumberToken of float
             | StringToken of string
             | SymbolToken of string
             | ComboToken  of KlToken list

(* Tokenizer is strict about spacing. It will not handle extra spaces inside of parens. *)
module KlTokenizer =
    let stringLiteral =
        let escape = anyOf "\"\\/bfnrt" |>> function
                            | 'b' -> "\b"
                            | 'f' -> "\u000C"
                            | 'n' -> "\n"
                            | 'r' -> "\r"
                            | 't' -> "\t"
                            | c   -> string c
        let unicodeEscape =
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9
            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char
                |> string)
        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')
        between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)
    let pKlToken, pKlTokenRef = createParserForwardedToRef<KlToken, unit>()
    let pKlBool = (stringReturn "true" (BoolToken true)) <|> (stringReturn "false" (BoolToken false))
    let pKlNumber = pfloat |>> NumberToken
    let pKlString = stringLiteral |>> StringToken
    let pKlSymbol = regex "[a-zA-Z_]\\w*" |>> SymbolToken
    let pKlCombo = between (pchar '(') (pchar ')') (sepBy pKlToken spaces1) |>> ComboToken
    do pKlTokenRef := choice [pKlBool; pKlNumber; pKlString; pKlSymbol; pKlCombo]
    let tokenize = pKlToken

type KlExpr = EmptyExpr
            | BoolExpr   of bool
            | NumberExpr of float
            | StringExpr of string
            | SymbolExpr of string
            | AndExpr    of KlExpr * KlExpr
            | OrExpr     of KlExpr * KlExpr
            | IfExpr     of KlExpr * KlExpr * KlExpr
            | CondExpr   of (KlExpr * KlExpr) list
            | LetExpr    of string * KlExpr * KlExpr
            | LambdaExpr of string * KlExpr
            | DefunExpr  of string * string list * KlExpr
            | FreezeExpr of KlExpr
            | TrapExpr   of KlExpr * KlExpr
            | AppExpr    of KlExpr * KlExpr list

exception SymbolExpected
exception InvalidCondClause

module KlParser =
    let sym = function
        | (SymbolToken s) -> s
        | _ -> raise SymbolExpected
    let rec parse = function
        | ComboToken [] -> EmptyExpr
        | BoolToken b -> BoolExpr b
        | NumberToken n -> NumberExpr n
        | StringToken s -> StringExpr s
        | SymbolToken s -> SymbolExpr s
        | ComboToken [(SymbolToken "and"); left; right] -> AndExpr (parse left, parse right)
        | ComboToken [(SymbolToken "or"); left; right] -> OrExpr (parse left, parse right)
        | ComboToken [(SymbolToken "if"); c; t; e] -> IfExpr (parse c, parse t, parse e)
        | ComboToken (SymbolToken "cond" :: clauses) -> CondExpr (List.map
                                                                     (function
                                                                         | (ComboToken [x; y]) -> (parse x, parse y)
                                                                         | _ -> raise InvalidCondClause)
                                                                     clauses)
        | ComboToken [(SymbolToken "let"); (SymbolToken s); v; e] -> LetExpr (s, parse v, parse e)
        | ComboToken [(SymbolToken "lambda"); (SymbolToken arg); body] -> LambdaExpr (arg, parse body)
        | ComboToken [(SymbolToken "defun"); (SymbolToken name); (ComboToken paramz); body] -> DefunExpr (name, List.map sym paramz, parse body)
        | ComboToken [(SymbolToken "freeze"); e] -> FreezeExpr (parse e)
        | ComboToken [(SymbolToken "trap-error"); body; handler] -> TrapExpr (parse body, parse handler)
        | ComboToken (f :: args) -> AppExpr (parse f, List.map parse args)


type KlValue = EmptyValue
             | BoolValue     of bool
             | NumberValue   of float
             | StringValue   of string
             | SymbolValue   of string
             | FunctionValue of string list * (KlValue list -> KlValue)
             | VectorValue   of KlValue list
             | ConsValue     of KlValue * KlValue

type Context = System.Collections.Generic.Dictionary<string, KlValue>

(* Thunks are used to implement tail calls. They should be be visible in KL code. *)
type Thunk = Thunk of (unit -> KlValue)

type Position = Head | Tail

exception BoolExpected
exception FunctionExpected

module KlEvaluator =
    let boolTrue = BoolValue true
    let boolFalse = BoolValue false
    let getBool = function
        | BoolValue b -> b
        | _ -> raise BoolExpected
    let getFunc context value =
        match value with
        | FunctionValue (paramz, f) -> f
        | _ -> raise FunctionExpected
    let append context defs =
        context
    let rec eval context expr =
        match expr with
        | EmptyExpr           -> EmptyValue
        | BoolExpr b          -> BoolValue b
        | NumberExpr n        -> NumberValue n
        | StringExpr s        -> StringValue s
        | SymbolExpr s        -> SymbolValue s
        | AndExpr (l, r)      -> if eval context l |> getBool then eval context r else boolFalse
        | OrExpr (l, r)       -> if eval context l |> getBool then boolTrue else eval context r
        | IfExpr (c, t, e)    -> if eval context c |> getBool then eval context t else eval context e
        | CondExpr (clauses)  -> snd (List.fold
                                          (fun (found, value) (condition, consequence) ->
                                              if found || (eval context condition |> getBool |> not) then
                                                  (found, value)
                                              else
                                                  (true, eval context consequence))
                                          (false, EmptyValue)
                                          clauses)
        | LetExpr (s, v, e)   -> eval (append context (s, eval context v)) e
        | LambdaExpr (s, e)   -> FunctionValue ([s], fun values -> eval (append context [s, values.[0]]) e)
        | DefunExpr (s, a, e) -> EmptyValue
        | FreezeExpr e        -> FunctionValue ([], fun values -> eval context e)
        | TrapExpr (t, c)     -> EmptyValue
        | AppExpr (f, args)   -> eval context f |> getFunc context <| List.map (eval context) args

module KlCompiler =
    let rec compiler = ""
