#light

namespace ShenPOF

open FParsec
open FSharpx.Option

type KlToken = BoolToken   of bool
             | NumberToken of decimal
             | StringToken of string
             | SymbolToken of string
             | ComboToken  of KlToken list

(* Tokenizer is strict about spacing. It will not handle extra spaces inside of parens. *)
module KlTokenizer =
    let pKlToken, pKlTokenRef = createParserForwardedToRef<KlToken, unit>()
    let pKlBool = (stringReturn "true" (BoolToken true)) <|> (stringReturn "false" (BoolToken false))
    let pKlNumber = pfloat |>> (decimal >> NumberToken)
    let stringLiteral = between (pchar '"') (pchar '"') (manySatisfy ((<>) '"'))
    let pKlString = stringLiteral |>> StringToken
    let pKlSymbol = regex "[a-zA-Z0-9\\x2B\\x2D\\x2F\\x3D\\x3E\\x3F\\x5F]+" |>> SymbolToken
    let pKlCombo = between (pchar '(') (pchar ')') (sepBy pKlToken spaces1) |>> ComboToken
    do pKlTokenRef := choice [pKlBool; pKlNumber; pKlString; pKlSymbol; pKlCombo]
    let tokenize s = run pKlToken s |> function
                                       | Success(result, _, _) -> result
                                       | Failure(error, _, _) -> raise <| new System.Exception(error)

type Position = Head | Tail
type KlExpr = EmptyExpr
            | BoolExpr   of bool
            | NumberExpr of decimal
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
            | TrapExpr   of Position * KlExpr * KlExpr
            | AppExpr    of Position * KlExpr * KlExpr list

exception SymbolExpected
exception InvalidCondClause

module KlParser =
    let tSymbol = function
        | (SymbolToken s) -> s
        | _ -> raise SymbolExpected
    let rec parse pos = function
        | ComboToken [] -> EmptyExpr
        | BoolToken b -> BoolExpr b
        | NumberToken n -> NumberExpr n
        | StringToken s -> StringExpr s
        | SymbolToken s -> SymbolExpr s
        | ComboToken [(SymbolToken "and"); left; right] -> AndExpr (parse Head left, parse pos right)
        | ComboToken [(SymbolToken "or");  left; right] -> OrExpr  (parse Head left, parse pos right)
        | ComboToken [(SymbolToken "if"); condition; consequent; alternative] ->
            IfExpr (parse Head condition, parse pos consequent, parse pos alternative)
        | ComboToken (SymbolToken "cond" :: clauses) ->
            clauses |> List.map (function | ComboToken [condition; consequent] -> (parse Head condition, parse pos consequent)
                                          | _ -> raise InvalidCondClause)
                    |> CondExpr
        | ComboToken [(SymbolToken "let"); (SymbolToken name); binding; body] -> LetExpr (name, parse Head binding, parse pos body)
        | ComboToken [(SymbolToken "lambda"); (SymbolToken arg); body] -> LambdaExpr (arg, parse Tail body)
        | ComboToken [(SymbolToken "defun"); (SymbolToken name); (ComboToken paramz); body] -> DefunExpr (name, List.map tSymbol paramz, parse Tail body)
        | ComboToken [(SymbolToken "freeze"); expr] -> FreezeExpr (parse Tail expr)
        | ComboToken [(SymbolToken "trap-error"); body; handler] -> TrapExpr (pos, parse Head body, parse pos handler)
        | ComboToken (f :: args) -> AppExpr (pos, parse Head f, List.map (parse Head) args)

// TODO are there multiple numeric types in KL/Shen?
// what is the result of `(/ 1 2)`? is it 0 or 0.5?

type Globals = System.Collections.Generic.Dictionary<string, KlValue>
and Locals = Map<string, KlValue> list
and Env = Globals * Locals
and Function(arity : int, f : KlValue list -> Result) =
    member this.Arity = arity
    member this.Apply(args : KlValue list) = f args
and KlValue = EmptyValue
            | BoolValue     of bool
            | NumberValue   of decimal
            | StringValue   of string
            | SymbolValue   of string
            | FunctionValue of Function
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
and Thunk(cont : unit -> Result) =
    member this.Run() =
        match cont () with
        | ThunkResult thunk -> thunk.Run()
        | result -> result
and Uncaught = Uncaught of string
and Result = ValueResult of KlValue
           | ErrorResult of Uncaught
           | ThunkResult of Thunk
// TODO break the distinction between Pending and Complete(Value|Error) into another level of DU?
// TODO use `inherits` to re-use DU cases and prevent nested boxing?

// both Uncaught/ErrorValue only occur as a result of (simple-error "")
// any other errors in KL code are raw exceptions that crash the runtime (div by zero, etc)

exception BoolExpected
exception FunctionExpected
exception UnexpectedThunk
exception NoClauseMatched
exception TooManyArgs

module KlEvaluator =
    let vBool = function
        | BoolValue b -> b
        | _ -> raise BoolExpected
    let boolR = BoolValue >> ValueResult
    let trueR = boolR true
    let falseR = boolR false
    let thunkR f = new Thunk(f) |> ThunkResult
    let funcR arity f = new Function(arity, f) |> FunctionValue |> ValueResult
    let append (globals, locals) defs = globals, List.Cons(Map.ofList defs, locals)
    let append1 env k v = append env [(k, v)]
    let closure eval env (paramz : string list) body =
        new Function(paramz.Length, fun args -> eval (append env (List.zip paramz args)) body) |> FunctionValue
    let vFunc ((globals, _) as env : Env) value =
        match value with
        | FunctionValue f -> (f.Arity, f.Apply)
        | SymbolValue s -> match globals.[s] with
                           | FunctionValue f -> (f.Arity, f.Apply)
                           | _ -> raise FunctionExpected
        | _ -> raise FunctionExpected
    let go = function
        | ThunkResult thunk -> thunk.Run()
        | result -> result
    let rec apply (pos : Position) (arity : int) (f : KlValue list -> Result) (args : KlValue list) : Result =
        if args.Length > arity
        then raise TooManyArgs
        else if args.Length < arity
             then funcR (arity - args.Length) (List.append args >> apply pos arity f)
             else match pos with
                  | Head -> f args |> go
                  | Tail -> thunkR (fun () -> f args)
    let rec bindR result f =
        match go result with
        | ValueResult value -> f value
        | ErrorResult _ as error -> error
        | _ -> raise UnexpectedThunk
    let resolve (_, locals : Locals) symbolName =
        Seq.map (Map.tryFind symbolName) locals
        |> Seq.tryFind Option.isSome
        |> concat
        |> getOrElse (SymbolValue symbolName)
    let rec eval env = function
        | EmptyExpr    -> EmptyValue |> ValueResult
        | BoolExpr b   -> boolR b
        | NumberExpr n -> NumberValue n |> ValueResult
        | StringExpr s -> StringValue s |> ValueResult
        | SymbolExpr s -> resolve env s |> ValueResult
        | AndExpr (left, right) -> eval env left |> bindR <| (vBool >> (fun b -> if b then eval env right else falseR))
        | OrExpr  (left, right) -> eval env left |> bindR <| (vBool >> (fun b -> if b then trueR else eval env right))
        | IfExpr (condition, consequent, alternative) ->
            eval env condition |> bindR <| (vBool >> (fun b -> eval env <| if b then consequent else alternative))
        | CondExpr clauses ->
            let rec evalClauses = function
                | (condition, consequent) :: rest ->
                    eval env condition |> bindR <| (vBool >> (fun b -> if b then eval env consequent else evalClauses rest))
                | [] -> raise NoClauseMatched
            evalClauses clauses
        | LetExpr (symbol, binding, body) ->
            eval env binding |> bindR <| (fun v -> eval (append1 env symbol v) body)
        | LambdaExpr (param, body) -> closure eval env [param] body |> ValueResult
        | DefunExpr (name, paramz, body) -> let f = closure eval env paramz body // TODO not really a closure?
                                            let (globals : Globals, _) = env
                                            globals.Add(name, f)
                                            ValueResult f
        | FreezeExpr expr -> closure eval env [] expr |> ValueResult
        | TrapExpr (pos, body, handler) ->
            match eval env body |> go with
            | ErrorResult (Uncaught e) ->
                eval env handler |> bindR <| (fun v -> let (arity, func) = vFunc env v
                                                       apply pos arity func [ErrorValue e])
            | _ as r -> go r
        | AppExpr (pos, f, args) ->
            eval env f |> bindR <| (fun v -> let (arity, func) = vFunc env v
                                             let rec evalArgs (args : KlExpr list) (vals : KlValue list) : Choice<KlValue list, Result> =
                                                 match args with
                                                 | [] -> Choice1Of2 vals
                                                 | arg :: args -> match eval env arg |> go with
                                                                  | ValueResult v -> evalArgs args (List.append vals [v])
                                                                  | e -> Choice2Of2 e
                                             FSharpx.Choice.choice (apply pos arity func) (fun x -> x) (evalArgs args []))

exception InvalidArgs

module KlBuiltins =
    let newEnv kvs = System.Linq.Enumerable.ToDictionary(kvs, fst, snd), []
    let emptyEnv () = newEnv Seq.empty
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
    let rec klStr = function
        | EmptyValue -> "()"
        | BoolValue b -> if b then "true" else "false"
        | NumberValue n -> n.ToString()
        | StringValue s -> "\"" + s + "\""
        | SymbolValue s -> s
        | ConsValue (head, tail) -> sprintf "(cons %s %s)" (klStr head) (klStr tail)
        | VectorValue value -> sprintf "(@v%s)" (System.String.Join("", (Array.map (fun s -> " " + klStr s) value)))
        | ErrorValue message -> sprintf "(simple-error \"%s\")" message
        | FunctionValue f -> sprintf "<Function %s>" (f.ToString())
        | StreamValue s -> sprintf "<Stream %s>" (s.ToString())
    let rec klToString = function
        | [x : KlValue] -> x |> klStr |> StringValue
        | _ -> raise InvalidArgs
    let klIsString = function
        | [StringValue _] -> BoolValue true
        | [_] -> BoolValue false
        | _ -> raise InvalidArgs
    let klIntToString = function
        | [NumberValue n] -> int n |> char |> string |> StringValue
        | _ -> raise InvalidArgs
    let klStringToInt = function
        | [StringValue s] -> s.[0] |> int |> decimal |> NumberValue
        | _ -> raise InvalidArgs
    let klSet (globals : Globals, _) = function
        | [SymbolValue s; x] -> globals.[s] <- x
        | _ -> raise InvalidArgs
    let klValue (globals : Globals, _) = function
        | [SymbolValue s] -> globals.[s]
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
        //| FunctionValue x, FunctionValue y -> x = y
        | ErrorValue x, ErrorValue y   -> x = y
        | ConsValue (x1, x2), ConsValue (y1, y2) -> klEq (x1, y1) && klEq (x2, y2)
        | VectorValue xs, VectorValue ys -> xs.Length = ys.Length && Array.forall2 (=) xs ys
        | (_, _) -> false
    let klEquals = function
        | [x; y] -> klEq (x, y) |> BoolValue
        | _ -> raise InvalidArgs
    let rec klValueToToken = function
        | EmptyValue -> ComboToken []
        | BoolValue b -> BoolToken b
        | NumberValue n -> NumberToken n
        | StringValue s -> StringToken s
        | SymbolValue s -> SymbolToken s
        | ConsValue _ as cons ->
            let generator = function | ConsValue (head, tail) -> Some(klValueToToken head, tail)
                                     | EmptyValue -> None
                                     | _ -> raise InvalidArgs
            cons |> Seq.unfold generator |> Seq.toList |> ComboToken
        | x -> invalidArg "_" <| x.ToString()
    let klEval env = function
        | [v] -> klValueToToken v |> KlParser.parse Head |> KlEvaluator.eval env
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
                     b |> decimal |> NumberValue
                else raise InvalidArgs
        | _ -> raise InvalidArgs
    let klReadByte = function
        | [StreamValue stream] -> stream.ReadByte() |> decimal |> NumberValue
        | _ -> raise InvalidArgs
    let klOpen = function
        | [StringValue path; SymbolValue "in"] -> System.IO.File.OpenRead(path) :> System.IO.Stream |> StreamValue
        | [StringValue path; SymbolValue "out"] -> System.IO.File.OpenWrite(path) :> System.IO.Stream |> StreamValue
        | _ -> raise InvalidArgs
    let klClose = function
        | [StreamValue stream] -> stream.Close()
                                  EmptyValue
        | _ -> raise InvalidArgs
    let epoch = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let startTime = System.DateTime.UtcNow
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let klGetTime = function
        | [SymbolValue "run"] -> stopwatch.ElapsedTicks * 100L |> decimal |> NumberValue // TODO run time in picoseconds?
        | [SymbolValue "unix"] -> (System.DateTime.UtcNow - epoch).TotalSeconds |> decimal |> NumberValue
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
    let baseEnv () =
        let (globals, _) as env =
            newEnv [
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
                "number?",         func 1 klIsNumber]
        globals.Add("eval-kl", FunctionValue (new Function(1, klEval env)))
        globals.Add("simple-error", FunctionValue (new Function(1, klSimpleError)))
        env

module KlCompiler =
    let rec compiler = fun (x : KlExpr) -> "fsharp code"
