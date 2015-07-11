namespace Kl

open FParsec

type KlToken = BoolToken   of bool
             | NumberToken of decimal // TODO numbers broken into ints and floats
             | StringToken of string
             | SymbolToken of string
             | ComboToken  of KlToken list

(* Tokenizer is strict about spacing. It will not handle extra spaces inside of parens. *)
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

type Position = Head | Tail
type KlExpr = EmptyExpr
            | BoolExpr   of bool
            | NumberExpr of decimal
            | StringExpr of string
            | SymbolExpr of string
            | AndExpr    of KlExpr * KlExpr                 // (Bool, Bool) -> Bool
            | OrExpr     of KlExpr * KlExpr                 // (Bool, Bool) -> Bool
            | IfExpr     of KlExpr * KlExpr * KlExpr        // (Bool, a, a) -> a
            | CondExpr   of (KlExpr * KlExpr) list          // [(Bool, a)] -> a
            | LetExpr    of string * KlExpr * KlExpr        // (Symbol, a, Expr) -> a
            | LambdaExpr of string * KlExpr                 // (Symbol, a) -> (Value -> a)
            | DefunExpr  of string * string list * KlExpr   // (Symbol, [Symbol], Expr) -> ([Value] -> a)
            | FreezeExpr of KlExpr                          // Expr -> (() -> Value)
            | TrapExpr   of Position * KlExpr * KlExpr
            | AppExpr    of Position * KlExpr * KlExpr list

module KlParser =
    let tSymbol = function
        | (SymbolToken s) -> s
        | _ -> failwith "Symbol expected"
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
                                          | _ -> failwith "Invalid cond clause")
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
and Thunk(cont : unit -> Result) =
    member this.Run() =
        match cont () with
        | ThunkResult thunk -> thunk.Run()
        | result -> result
and Result = ValueResult of KlValue
           | ErrorResult of string
           | ThunkResult of Thunk
// TODO break the distinction between Pending and Complete(Value|Error) into another level of DU?
// TODO use `inherits` to re-use DU cases and prevent nested boxing?

module KlEvaluator =
    let (|Greater|Equal|Lesser|) (x, y) = if x > y then Greater elif x < y then Lesser else Equal
    let vBool = function
        | BoolValue b -> b
        | _ -> failwith "Boolean value expected"
    let boolR = BoolValue >> ValueResult
    let trueR = boolR true
    let falseR = boolR false
    let branch f g x y v = if vBool v then f x else g y
    let branch1 f = branch f f
    let thunkR f = new Thunk(f) |> ThunkResult
    let funcR arity f = new Function(arity, f) |> FunctionValue |> ValueResult
    let append (globals, locals) defs = globals, List.Cons(Map.ofList defs, locals)
    let append1 env k v = append env [(k, v)]
    let closure eval env (paramz : string list) body =
        new Function(paramz.Length, fun args -> eval (append env (List.zip paramz args)) body) |> FunctionValue
    let vFunc ((globals, _) as env : Env) value =
        match value with
        | FunctionValue f -> f
        | SymbolValue s -> match globals.TryGetValue(s) with
                           | (true, FunctionValue f) -> f
                           | (false, _) -> failwithf "Symbol \"%s\" is undefined" s
                           | _ -> failwithf "Symbol \"%s\" does not represent function" s
        | _ -> failwith "Function value expected"
    let go = function
        | ThunkResult thunk -> thunk.Run()
        | result -> result
    let rec apply (pos : Position) (f : Function) (args : KlValue list) : Result =
        match args.Length, f.Arity with
        | Greater -> failwith "Too many arguments"
        | Lesser -> funcR (f.Arity - args.Length) (List.append args >> apply pos f)
        | Equal -> match pos with
                   | Head -> f.Apply args |> go
                   | Tail -> thunkR (fun () -> f.Apply args)
    let rec (>>=) result f =
        match go result with
        | ValueResult value -> f value
        | ErrorResult _ as error -> error
        | _ -> failwith "Unexpected thunk"
    let resolve (_, locals : Locals) symbolName =
        Seq.map (Map.tryFind symbolName) locals
        |> Seq.tryFind Option.isSome
        |> FSharpx.Option.concat
        |> FSharpx.Option.getOrElse (SymbolValue symbolName)
    let rec evalArgs evalE vals args =
        match args with
        | [] -> Choice1Of2 vals
        | arg :: args -> match evalE arg |> go with
                         | ValueResult v -> evalArgs evalE (List.append vals [v]) args
                         | e -> Choice2Of2 e
    let rec eval env = function // have `eval` call `eval0` and run thunks, `eval0` runs `eval` on other exprs
        | EmptyExpr    -> EmptyValue |> ValueResult
        | BoolExpr b   -> boolR b
        | NumberExpr n -> NumberValue n |> ValueResult
        | StringExpr s -> StringValue s |> ValueResult
        | SymbolExpr s -> resolve env s |> ValueResult
        | AndExpr (left, right) -> eval env left >>= branch (eval env) id right falseR
        | OrExpr  (left, right) -> eval env left >>= branch id (eval env) trueR right
        | IfExpr (condition, ifTrue, ifFalse) -> eval env condition >>= branch1 (eval env) ifTrue ifFalse
        | CondExpr clauses ->
            let rec evalClauses = function
                | (condition, ifTrue) :: rest -> eval env condition >>= branch (eval env) evalClauses ifTrue rest
                | [] -> failwith "No condition was true"
            evalClauses clauses
        | LetExpr (symbol, binding, body) ->
            eval env binding >>= (fun v -> eval (append1 env symbol v) body)
        | LambdaExpr (param, body) -> closure eval env [param] body |> ValueResult
        | DefunExpr (name, paramz, body) -> let f = closure eval env paramz body
                                            let (globals : Globals, _) = env
                                            globals.Add(name, f)
                                            ValueResult f
        | FreezeExpr expr -> closure eval env [] expr |> ValueResult
        | TrapExpr (pos, body, handler) ->
            match eval env body |> go with
            | ErrorResult e ->
                eprintfn "Error trapped: %s" e
                eval env handler >>= (fun v -> apply pos (vFunc env v) [ErrorValue e])
            | r -> go r
        | AppExpr (pos, f, args) ->
            let show = function
                        | SymbolExpr s -> s
                        | _ -> "unknown"
            eprintfn "Enter app %s" (show f) |> ignore
            let r = eval env f >>= (fun v -> FSharpx.Choice.choice (apply pos (vFunc env v)) id (evalArgs (eval env) [] args))
            eprintfn "Exit app %s" (show f) |> ignore
            r

module KlBuiltins =
    let inline invalidArgs () = failwith "Wrong number or type of arguments"
    let trueV = BoolValue true
    let falseV = BoolValue false
    let klIntern = function
        | [StringValue s] -> SymbolValue s
        | _ -> invalidArgs ()
    let klStringPos = function
        | [StringValue s; NumberValue index] ->
            let i = int index
            if i >= 0 && i < s.Length
                then s.[i] |> string |> StringValue |> ValueResult
                else "string index out of bounds" |> ErrorResult
        | _ -> invalidArgs ()
    let klStringTail = function
        | [StringValue s] -> s.Substring(1) |> StringValue
        | _ -> invalidArgs ()
    let klStringConcat = function
        | [StringValue x; StringValue y] -> x + y |> StringValue
        | _ -> invalidArgs ()
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
        | _ -> invalidArgs ()
    let klIsString = function
        | [StringValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let klIntToString = function
        | [NumberValue n] -> int n |> char |> string |> StringValue
        | _ -> invalidArgs ()
    let klStringToInt = function
        | [StringValue s] -> s.[0] |> int |> decimal |> NumberValue
        | _ -> invalidArgs ()
    let klSet ((globals, _) : Env) = function
        | [SymbolValue s; x] -> globals.[s] <- x
                                x
        | _ -> invalidArgs ()
    let klValue ((globals, _) : Env) = function
        | [SymbolValue s] -> match globals.TryGetValue(s) with
                             | (true, v) -> v
                             | (false, _) -> failwithf "Symbol \"%s\" is undefined" s
        | _ -> invalidArgs ()
    let klSimpleError = function
        | [StringValue s] -> ErrorResult s
        | _ -> invalidArgs ()
    let klErrorToString = function
        | [ErrorValue s] -> StringValue s
        | _ -> invalidArgs ()
    let klNewCons = function
        | [x; y] -> ConsValue (x, y)
        | _ -> invalidArgs ()
    let klHead = function
        | [ConsValue (x, _)] -> x
        | _ -> invalidArgs ()
    let klTail = function
        | [ConsValue (_, y)] -> y
        | _ -> invalidArgs ()
    let klIsCons = function
        | [ConsValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let rec klEq = function
        | EmptyValue, EmptyValue                 -> true
        | BoolValue x, BoolValue y               -> x = y
        | NumberValue x, NumberValue y           -> x = y
        | StringValue x, StringValue y           -> x = y
        | SymbolValue x, SymbolValue y           -> x = y
        | StreamValue x, StreamValue y           -> x = y
        | FunctionValue x, FunctionValue y       -> x = y
        | ErrorValue x, ErrorValue y             -> x = y
        | ConsValue (x1, x2), ConsValue (y1, y2) -> klEq (x1, y1) && klEq (x2, y2)
        | VectorValue xs, VectorValue ys         -> xs.Length = ys.Length && Array.forall2 (=) xs ys
        | (_, _) -> false
    let klEquals = function
        | [x; y] -> klEq (x, y) |> BoolValue
        | _ -> invalidArgs ()
    let rec klValueToToken = function
        | EmptyValue -> ComboToken []
        | BoolValue b -> BoolToken b
        | NumberValue n -> NumberToken n
        | StringValue s -> StringToken s
        | SymbolValue s -> SymbolToken s
        | ConsValue _ as cons ->
            let generator = function | ConsValue (head, tail) -> Some(klValueToToken head, tail)
                                     | EmptyValue -> None
                                     | _ -> invalidArgs ()
            cons |> Seq.unfold generator |> Seq.toList |> ComboToken
        | x -> invalidArg "_" <| x.ToString()
    let klEval env = function
        | [v] -> klValueToToken v |> KlParser.parse Head |> KlEvaluator.eval env
        | _ -> invalidArgs ()
    let klType = function
        | [x; _] -> x // TODO label the type of an expression (what does that mean?)
        | _ -> invalidArgs ()
    let klNewVector = function
        | [NumberValue length] -> Array.create (int length) EmptyValue |> VectorValue
        | _ -> invalidArgs ()
    let klReadVector = function
        | [VectorValue vector; NumberValue index] ->
            let i = int index
            if i >= 0 && i < vector.Length
                then vector.[i] |> ValueResult
                else ErrorResult "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klWriteVector = function
        | [VectorValue vector as vv; NumberValue index; value] ->
            let i = int index
            if i >= 0 && i < vector.Length
                then vector.[i] <- value
                     ValueResult vv
                else ErrorResult "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klIsVector = function
        | [VectorValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let klWriteByte = function
        | [NumberValue number; StreamValue stream] ->
            let i = int number
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.WriteByte(b)
                     b |> decimal |> NumberValue
                else invalidArgs ()
        | _ -> invalidArgs ()
    let klReadByte = function
        | [StreamValue stream] -> stream.ReadByte() |> decimal |> NumberValue
        | _ -> invalidArgs ()
    let klOpen = function
        | [StringValue path; SymbolValue "in"] -> System.IO.File.OpenRead(path) :> System.IO.Stream |> StreamValue
        | [StringValue path; SymbolValue "out"] -> System.IO.File.OpenWrite(path) :> System.IO.Stream |> StreamValue
        | _ -> invalidArgs ()
    let klClose = function
        | [StreamValue stream] -> stream.Close()
                                  EmptyValue
        | _ -> invalidArgs ()
    let epoch = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let startTime = System.DateTime.UtcNow
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let klGetTime = function
        // Both run and unix time are in milliseconds
        // ElapsedTicks is in 100 picoseconds/0.1 microseconds
        | [SymbolValue "run"] -> stopwatch.ElapsedTicks * 10000L |> decimal |> NumberValue
        | [SymbolValue "unix"] -> (System.DateTime.UtcNow - epoch).TotalSeconds |> decimal |> NumberValue
        // TODO support "real" time?
        | _ -> invalidArgs ()
    let op f wrapper = function
        | [NumberValue x; NumberValue y] -> f x y |> wrapper
        | _ -> invalidArgs ()
    let klAdd              = op (+)  NumberValue
    let klSubtract         = op (-)  NumberValue
    let klMultiply         = op (*)  NumberValue
    let klDivide           = op (/)  NumberValue
    let klGreaterThan      = op (>)  BoolValue
    let klLessThan         = op (<)  BoolValue
    let klGreaterThanEqual = op (>=) BoolValue
    let klLessThanEqual    = op (<=) BoolValue
    let klIsNumber = function
        | [NumberValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let funR arity f = FunctionValue (new Function (arity, f))
    let funV arity f = funR arity (f >> ValueResult)
    let emptyEnv () = new Globals(), []
    let baseEnv () =
        let (globals, _) as env = emptyEnv ()
        let rec install = function
            | [] -> ()
            | (name, value) :: defs ->
                globals.Add(name, value)
                install defs
        install [
            "intern",          funV 1 klIntern
            "pos",             funR 2 klStringPos
            "tlstr",           funV 1 klStringTail
            "cn",              funV 2 klStringConcat
            "str",             funV 1 klToString
            "string?",         funV 1 klIsString
            "n->string",       funV 1 klIntToString
            "string->n",       funV 1 klStringToInt
            "set",             funV 2 (klSet env)
            "value",           funV 1 (klValue env)
            "simple-error",    funR 1 klSimpleError
            "error-to-string", funV 1 klErrorToString
            "cons",            funV 2 klNewCons
            "hd",              funV 1 klHead
            "tl",              funV 1 klTail
            "cons?",           funV 1 klIsCons
            "=",               funV 2 klEquals
            "type",            funV 1 klType
            "eval-kl",         funR 1 (klEval env)
            "absvector",       funV 1 klNewVector
            "<-address",       funR 2 klReadVector
            "address->",       funR 3 klWriteVector
            "absvector?",      funV 1 klIsVector
            "write-byte",      funV 2 klWriteByte
            "read-byte",       funV 1 klReadByte
            "open",            funV 2 klOpen
            "close",           funV 1 klClose
            "get-time",        funV 1 klGetTime
            "+",               funV 2 klAdd
            "-",               funV 2 klSubtract
            "*",               funV 2 klMultiply
            "/",               funV 2 klDivide
            ">",               funV 2 klGreaterThan
            "<",               funV 2 klLessThan
            ">=",              funV 2 klGreaterThanEqual
            "<=",              funV 2 klLessThanEqual
            "number?",         funV 1 klIsNumber
            "*language*",      "F# 3.1" |> StringValue
            "*implementation*","CLR " + System.Environment.Version.ToString() |> StringValue
            "*release*",       "0" |> StringValue
            "*port*",          "0" |> StringValue
            "*porters*",       "Robert Koeninger" |> StringValue
            "*version*",       "19.1" |> StringValue
            "*stinput*",       System.Console.OpenStandardInput() |> StreamValue
            "*stoutput*",      System.Console.OpenStandardOutput() |> StreamValue
            "*home-directory*",System.Environment.CurrentDirectory |> StringValue
            ]
        env

module KlCompiler =
    let rec compiler = fun (x : KlExpr) -> "fsharp code"
