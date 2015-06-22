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
    let tokenize s = run pKlToken s |> function
                                       | Success(result, _, _) -> result
                                       | Failure(error, _, _) -> raise <| new System.Exception(error)

type KlExpr = EmptyExpr
            | BoolExpr   of bool
            | NumberExpr of float
            | StringExpr of string
            | SymbolExpr of string
            | AndExpr    of KlExpr * KlExpr                 // (Bool, Bool) -> Bool
            | OrExpr     of KlExpr * KlExpr                 // (Bool, Bool) -> Bool
            | IfExpr     of KlExpr * KlExpr * KlExpr        // (Bool, a, a) -> a
            | CondExpr   of (KlExpr * KlExpr) list          // (Bool, a) -> a
            | LetExpr    of string * KlExpr * KlExpr        // (Symbol, a, Expr) -> a
            | LambdaExpr of string * KlExpr                 // (Symbol, a) -> (Value -> a)
            | DefunExpr  of string * string list * KlExpr   // (Symbol, [Symbol], Expr) -> ([Value] -> a)
            | FreezeExpr of KlExpr                          // Expr -> (() -> Value)
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

type Context = System.Collections.Generic.Dictionary<string, KlValue>
and Closure = { Context : Context; Paramz : string list; Body : KlExpr }
and Function(arity : int, f : KlValue list -> KlValue) =
    member this.Arity = arity
    member this.Apply(args : KlValue list) = f args
and KlValue = EmptyValue
            | BoolValue     of bool
            | NumberValue   of float
            | StringValue   of string
            | SymbolValue   of string
            | FunctionValue of Function
            | ClosureValue  of Closure
            | VectorValue   of KlValue array
            | ConsValue     of KlValue * KlValue
            | ErrorValue    of string
            | StreamValue   of System.IO.Stream

(* Thunks are used to implement tail calls.
   Position is used to identify if an expression is a tail call candidate.
   They should not be visible in KL code.
   Using the type system to separate levels of the runtime.
   Some values are only for the runtime, some are only for the program.
   In some cases, never the twain shall meet. *)
type Thunk = Thunk of (unit -> KlValue)
type Position = Head | Tail

exception BoolExpected
exception FunctionExpected
exception NoClauseMatched

module KlEvaluator =
    let getBool = function
        | BoolValue b -> b
        | _ -> raise BoolExpected
    let rec getFunc (context : Context) (value : KlValue) =
        match value with
        | FunctionValue f -> f.Apply
        | SymbolValue s -> context.[s] |> getFunc context
        | _ -> raise FunctionExpected
    let closureV context paramz body = ClosureValue { Context = context; Paramz = paramz; Body = body }
    let append (context : Context) (defs : (string * KlValue) list) =
        for (key, value) in defs do
            context.Add(key, value)
        context
    let rec eval context expr =
        match expr with
        | EmptyExpr           -> EmptyValue
        | BoolExpr b          -> BoolValue b
        | NumberExpr n        -> NumberValue n
        | StringExpr s        -> StringValue s
        | SymbolExpr s        -> SymbolValue s
        | AndExpr (l, r)      -> if eval context l |> getBool then eval context r else BoolValue false
        | OrExpr (l, r)       -> if eval context l |> getBool then BoolValue true else eval context r
        | IfExpr (c, t, e)    -> if eval context c |> getBool then eval context t else eval context e
        | CondExpr (clauses)  -> let rec evalClauses = function
                                             | (condition, consequence) :: rest ->
                                                 if eval context condition |> getBool then
                                                     eval context consequence
                                                 else
                                                     evalClauses rest
                                             | [] -> raise NoClauseMatched
                                 evalClauses clauses
        | LetExpr (s, v, e)   -> eval (append context [(s, eval context v)]) e
        | LambdaExpr (s, e)   -> closureV context [s] e
        | DefunExpr (s, a, e) -> EmptyValue
        | FreezeExpr e        -> closureV context [] e
        | TrapExpr (t, c)     -> EmptyValue
        | AppExpr (f, args)   -> eval context f |> getFunc context <| List.map (eval context) args

module KlCompiler =
    let rec compiler = fun (x : KlExpr) -> "fsharp code"
