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
    let stringLiteral = between (pchar '"') (pchar '"') (manySatisfy ((<>) '"'))
    let pKlToken, pKlTokenRef = createParserForwardedToRef<KlToken, unit>()
    let pKlBool = (stringReturn "true" (BoolToken true)) <|> (stringReturn "false" (BoolToken false))
    let pKlNumber = pfloat |>> NumberToken
    let pKlString = stringLiteral |>> StringToken
    let pKlSymbol = regex "[a-zA-Z0-9\\x2B\\x2D\\x2F\\x3E\\x3F\\x5F]+" |>> SymbolToken
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

//There are 12 basic types in Shen.
//1. symbols .... abc hi-there, The_browncow_jumped_over_the_moon
//2. strings ..... any characters enclosed in "s
//3. numbers .... all objects closed under +, /, -, * 
//4. booleans ... true, false
//5. streams
//6. exceptions 
//7. vectors
//8. functions
//9. lists
//10. tuples
//11. closures
//12. continuations

// TODO are there multiple numeric types in KL/Shen?
// what is the result of `(/ 1 2)`? is it 0 or 0.5?

type Context = System.Collections.Generic.Dictionary<string, KlValue>
and Closure = { Paramz : string list; Body : KlExpr }
and Function(arity : int, f : KlValue list -> Result) =
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
and Thunk = Thunk of (unit -> KlValue)
and Position = Head | Tail
and Uncaught = Uncaught of string
and Result = ValueResult of KlValue
           | ErrorResult of Uncaught
           //| ThunkResult of Thunk

// both Uncaught/ErrorValue only occur as a result of (simple-error "")
// any other errors in KL code are raw exceptions that crash the runtime (div by zero, etc)

exception BoolExpected
exception FunctionExpected
exception NoClauseMatched
exception TooManyArgs

module KlEvaluator =
    let bind f = function
        | ValueResult v -> f v
        | y -> y
    let getBool = function
        | BoolValue b -> b
        | _ -> raise BoolExpected
    let closureV context paramz body = ClosureValue { Paramz = paramz; Body = body }
    let append (context : Context) (defs : (string * KlValue) list) =
        for (key, value) in defs do
            context.Add(key, value)
        context 
    let rec eval context expr =
        let rec apply (arity : int) (f : KlValue list -> Result) (args : KlValue list) : Result =
            if args.Length > arity
            then raise TooManyArgs
            else if args.Length < arity
                then let newArity = arity - args.Length
                     new Function(newArity, List.append args >> apply arity f) |> FunctionValue |> ValueResult
                else f args
        let rec getFunc (context : Context) (value : KlValue) =
            match value with
            | FunctionValue f -> (f.Arity, f.Apply)
            | ClosureValue c -> (c.Paramz.Length, fun args -> eval (append context (List.zip c.Paramz args)) c.Body)
            | SymbolValue s -> context.[s] |> getFunc context
            | _ -> raise FunctionExpected
        let evalcc = eval context // evalcc = "eval in the current context"
        match expr with
        | EmptyExpr    -> EmptyValue |> ValueResult
        | BoolExpr b   -> BoolValue b |> ValueResult
        | NumberExpr n -> NumberValue n |> ValueResult
        | StringExpr s -> StringValue s |> ValueResult
        | SymbolExpr s -> match context.TryGetValue(s) with // TODO what exactly is the prescribed evaluation of a symbol?
                                                            // when the first expr in an App, it gets cast as a func and applied
                                                            // when not in App position, it gets eval'd
                                                                                       // if already defined, return value
                                                                                       // if not already defined, return symbol
                                                            // but in Shen.Net (C#), i had it return a symbol and a symbol
                                                            // could get applied as a function (it implemented IFunction)
                                                            // and would cast it's value to IFunction
                                                            // and the symbol would apply it's args to it's assigned value
                          | (true, result) -> result |> ValueResult
                          | _ -> SymbolValue s |> ValueResult
        | AndExpr (left, right) ->
            match evalcc left with
            | ValueResult (BoolValue true) -> evalcc right
            | ValueResult (BoolValue false) -> false |> BoolValue |> ValueResult
            | ValueResult _ -> raise BoolExpected
            | e -> e
        | OrExpr (left, right) ->
            match evalcc left with
            | ValueResult (BoolValue true) -> true |> BoolValue |> ValueResult
            | ValueResult (BoolValue false) -> evalcc right
            | ValueResult _ -> raise BoolExpected
            | e -> e
        | IfExpr (condition, consequent, alternative) ->
            match evalcc condition with
            | ValueResult (BoolValue true) -> evalcc consequent
            | ValueResult (BoolValue false) -> evalcc alternative
            | ValueResult _ -> raise BoolExpected
            | e -> e
        | CondExpr clauses ->
            let rec evalClauses = function
                | (condition, consequent) :: rest ->
                    match evalcc condition with
                    | ValueResult (BoolValue true) -> evalcc consequent
                    | ValueResult (BoolValue false) -> evalClauses rest
                    | ValueResult _ -> raise BoolExpected
                    | e -> e
                | [] -> raise NoClauseMatched
            evalClauses clauses
        | LetExpr (symbol, binding, body) ->
            match evalcc binding with
            | ValueResult v -> eval (append context [(symbol, v)]) body
            | e -> e
        | LambdaExpr (param, body) -> closureV context [param] body |> ValueResult
        | DefunExpr (name, paramz, body) -> let f = closureV context paramz body
                                            context.Add(name, f)
                                            ValueResult f
        | FreezeExpr expr -> closureV context [] expr |> ValueResult
        | TrapExpr (body, handler) -> 
            match evalcc body with
            | ValueResult _ as v -> v
            | ErrorResult (Uncaught e) -> match evalcc handler with
                                          | ValueResult v -> let (arity, func) = getFunc context v
                                                             apply arity func [ErrorValue e]
                                          | er -> er
        | AppExpr (f, args) ->
            match evalcc f with
            | ValueResult v -> let (arity, func) = getFunc context v
                               let rec evalArgs (args : KlExpr list) (vals : KlValue list) : Choice<KlValue list, Result> =
                                   match args with
                                   | [] -> Choice1Of2 vals
                                   | arg :: args -> match evalcc arg with
                                                    | ValueResult v -> evalArgs args (List.append vals [v])
                                                    | e -> Choice2Of2 e
                               match evalArgs args [] with
                               | Choice1Of2 vals -> apply arity func vals
                               | Choice2Of2 e -> e
            | e -> e

exception InvalidArgs

module KlBuiltins =
    let getBool = function
        | BoolValue b -> b
        | _ -> raise BoolExpected
    let newContext kvs = System.Linq.Enumerable.ToDictionary(kvs, fst, snd)
    let emptyContext : Context = newContext Seq.empty
    let klIntern = function
        | [StringValue s] -> SymbolValue s
        | _ -> raise InvalidArgs
    let klStringPos = function
        | [StringValue s; NumberValue index] -> s.[int index] |> string |> StringValue
        | _ -> raise InvalidArgs
    let klStringTail = function
        | [StringValue s] -> s.Substring(1) |> StringValue
        | _ -> raise InvalidArgs
    let klStringConcat = function
        | [StringValue x; StringValue y] -> x + y |> StringValue
        | _ -> raise InvalidArgs
    let rec str = function
        | EmptyValue -> "()"
        | BoolValue b -> if b then "true" else "false"
        | NumberValue n -> n.ToString()
        | StringValue s -> "\"" + s + "\""
        | SymbolValue s -> s
        | ConsValue (head, tail) -> let headStr = str head
                                    let tailStr = str tail
                                    sprintf "(cons %s %s)" headStr tailStr
        | VectorValue value -> sprintf "(@v%s)" (System.String.Join("", (Array.map (fun s -> " " + str s) value)))
        | ErrorValue message -> sprintf "(simple-error \"%s\")" message
        | FunctionValue f -> sprintf "<Function %s>" (f.ToString())
        | ClosureValue c -> sprintf "<Closure %s>" (c.ToString())
        | StreamValue s -> sprintf "<Stream %s>" (s.ToString())
    let rec klToString = function
        | [x : KlValue] -> x |> str |> StringValue
        | _ -> raise InvalidArgs
    let klIsString = function
        | [StringValue _] -> BoolValue true
        | [_] -> BoolValue false
        | _ -> raise InvalidArgs
    let klIntToString = function
        | [NumberValue n] -> int n |> char |> string |> StringValue
        | _ -> raise InvalidArgs
    let klStringToInt = function
        | [StringValue s] -> s.[0] |> int |> float |> NumberValue
        | _ -> raise InvalidArgs
    let klSet (context : Context) = function
        | [SymbolValue s; x] -> context.[s] <- x
        | _ -> raise InvalidArgs
    let klValue (context : Context) = function
        | [SymbolValue s] -> context.[s]
        | _ -> raise InvalidArgs
    let klSimpleError = function
        | [StringValue s] -> ErrorResult (Uncaught s)
        | _ -> raise InvalidArgs
    let klErrorToString = function
        | [ErrorValue s] -> StringValue s
        | _ -> raise InvalidArgs
    let klNewCons = function
        | [x; y] -> ConsValue (x, y)
        | _ -> raise InvalidArgs
    let klHead = function
        | [ConsValue (x, _)] -> x
        | _ -> raise InvalidArgs
    let klTail = function
        | [ConsValue (_, y)] -> y
        | _ -> raise InvalidArgs
    let klIsCons = function
        | [ConsValue _] -> BoolValue true
        | [_] -> BoolValue false
        | _ -> raise InvalidArgs
    let rec klEq = function
        | EmptyValue, EmptyValue       -> true
        | BoolValue x, BoolValue y     -> x = y
        | NumberValue x, NumberValue y -> x = y
        | StringValue x, StringValue y -> x = y
        | SymbolValue x, SymbolValue y -> x = y
        | StreamValue x, StreamValue y -> x = y
        | ErrorValue x, ErrorValue y   -> x = y
        | ConsValue (x1, x2), ConsValue (y1, y2) -> klEq (x1, y1) && klEq (x2, y2)
        | VectorValue x, VectorValue y -> ((x.Length = y.Length) && (Array.zip x y |> Array.fold (fun r args -> r && klEq args) true))
        | (_, _) -> false
    let klEquals = function
        | [x; y] -> klEq (x, y) |> BoolValue
        | _ -> raise InvalidArgs
    let rec klConsToToken = function
        | EmptyValue -> ComboToken []
        | BoolValue b -> BoolToken b
        | NumberValue n -> NumberToken n
        | StringValue s -> StringToken s
        | SymbolValue s -> SymbolToken s
        | ConsValue _ as cons -> let sequ = Seq.unfold (function
                                                        | ConsValue (head, tail) -> Some(klConsToToken head, tail)
                                                        | EmptyValue -> None
                                                        | _ -> raise InvalidArgs)
                                                       cons
                                 sequ |> Seq.toList |> ComboToken
        | x -> invalidArg "x" (x.ToString())
    let klEval context = function
        | [v] -> klConsToToken v |> KlParser.parse |> KlEvaluator.eval context
        | _ -> raise InvalidArgs
    let klType = function
        | [x; _] -> x // TODO label the type of an expression (what does that mean?)
        | _ -> raise InvalidArgs
    let klNewVector = function
        | [NumberValue length] -> Array.create (int length) EmptyValue |> VectorValue
        | _ -> raise InvalidArgs
    let klReadVector = function
        | [VectorValue vector; NumberValue index] -> vector.[int index]
        | _ -> raise InvalidArgs
    let klWriteVector = function
        | [VectorValue vector; NumberValue index; value] -> vector.[int index] <- value
                                                            VectorValue vector
        | _ -> raise InvalidArgs
    let klIsVector = function
        | [VectorValue _] -> BoolValue true
        | [_] -> BoolValue false
        | _ -> raise InvalidArgs
    let klWriteByte = function
        | [NumberValue number; StreamValue stream] ->
            let i = int number
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.WriteByte(b)
                     NumberValue (float b)
                else raise InvalidArgs
        | _ -> raise InvalidArgs
    let klReadByte = function
        | [StreamValue stream] -> stream.ReadByte() |> float |> NumberValue
        | _ -> raise InvalidArgs
    let klOpen = function
        | [StringValue path; SymbolValue "in"] -> System.IO.File.OpenRead(path) :> System.IO.Stream |> StreamValue
        | [StringValue path; SymbolValue "out"] -> System.IO.File.OpenWrite(path) :> System.IO.Stream |> StreamValue
        | _ -> raise InvalidArgs
    let klClose = function
        | [StreamValue s] -> s.Close()
                             EmptyValue
        | _ -> raise InvalidArgs
    let epoch = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let startTime = System.DateTime.UtcNow
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let klGetTime = function
        | [SymbolValue "run"] -> stopwatch.ElapsedTicks * 100L |> float |> NumberValue // TODO run time in picoseconds?
        | [SymbolValue "unix"] -> (System.DateTime.UtcNow - epoch).TotalSeconds |> float |> NumberValue
        | _ -> raise InvalidArgs
    let op f wrapper = function
        | [NumberValue x; NumberValue y] -> f x y |> wrapper
        | _ -> raise InvalidArgs
    let klAdd              = op (+)  NumberValue
    let klSubtract         = op (-)  NumberValue
    let klMultiply         = op (*)  NumberValue
    let klDivide           = op (/)  NumberValue
    let klGreaterThan      = op (>)  BoolValue
    let klLessThan         = op (<)  BoolValue
    let klGreaterThanEqual = op (>=) BoolValue
    let klLessThanEqual    = op (<=) BoolValue
    let klIsNumber = function
        | [NumberValue _] -> BoolValue true
        | [_] -> BoolValue false
        | _ -> raise InvalidArgs
    let func arity f = FunctionValue (new Function(arity, f >> ValueResult))
    let baseContext : Context =
        let c = newContext [
                    "intern",          func 1 klIntern;
                    "pos",             func 2 klStringPos;
                    "strtl",           func 1 klStringTail;
                    "cn",              func 2 klStringConcat;
                    "str",             func 1 klToString;
                    "string?",         func 1 klIsString;
                    "n->string",       func 1 klIntToString;
                    "string->n",       func 1 klStringToInt;
                    "error-to-string", func 1 klErrorToString;
                    "cons",            func 2 klNewCons;
                    "hd",              func 1 klHead;
                    "tl",              func 1 klTail;
                    "cons?",           func 1 klIsCons;
                    "=",               func 2 klEquals;
                    "absvector",       func 1 klNewVector;
                    "<-address",       func 2 klReadVector;
                    "address->",       func 3 klWriteVector;
                    "absvector?",      func 1 klIsVector;
                    "write-byte",      func 2 klWriteByte;
                    "read-byte",       func 1 klReadByte;
                    "open",            func 2 klOpen;
                    "close",           func 1 klClose;
                    "get-time",        func 1 klGetTime;
                    "+",               func 2 klAdd;
                    "-",               func 2 klSubtract;
                    "*",               func 2 klMultiply;
                    "/",               func 2 klDivide;
                    ">",               func 2 klGreaterThan;
                    "<",               func 2 klLessThan;
                    ">=",              func 2 klGreaterThanEqual;
                    "<=",              func 2 klLessThanEqual;
                    "number?",         func 1 klIsNumber
                ]
        c.Add("eval-kl", FunctionValue (new Function(1, klEval c)))
        c.Add("simple-error", FunctionValue (new Function(1, klSimpleError)))
        c

module KlCompiler =
    let rec compiler = fun (x : KlExpr) -> "fsharp code"
