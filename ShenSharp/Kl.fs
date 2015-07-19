namespace Kl

open FParsec

type KlToken = BoolToken   of bool
             | NumberToken of decimal
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
            | BoolExpr    of bool
            | IntExpr     of int
            | DecimalExpr of decimal
            | StringExpr  of string
            | SymbolExpr  of string
            | AndExpr     of KlExpr * KlExpr                 // (Bool, Bool) -> Bool
            | OrExpr      of KlExpr * KlExpr                 // (Bool, Bool) -> Bool
            | IfExpr      of KlExpr * KlExpr * KlExpr        // (Bool, a, a) -> a
            | CondExpr    of (KlExpr * KlExpr) list          // [(Bool, a)] -> a
            | LetExpr     of string * KlExpr * KlExpr        // (Symbol, a, Expr) -> a
            | LambdaExpr  of string * KlExpr                 // (Symbol, a) -> (Value -> a)
            | DefunExpr   of string * string list * KlExpr   // (Symbol, [Symbol], Expr) -> ([Value] -> a)
            | FreezeExpr  of KlExpr                          // Expr -> (() -> Value)
            | TrapExpr    of Position * KlExpr * KlExpr
            | AppExpr     of Position * KlExpr * KlExpr list

module KlParser =
    let tSymbol = function
        | (SymbolToken s) -> s
        | _ -> failwith "Symbol expected"
    let rec parse pos = function
        | ComboToken [] -> EmptyExpr
        | BoolToken b -> BoolExpr b
        | NumberToken n -> if n % 1.0m = 0m then IntExpr (int n) else DecimalExpr n
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

type Globals = System.Collections.Generic.Dictionary<string, KlValue>
and Locals = Map<string, KlValue> list
and Env = { Globals : Globals; Locals : Locals }
and Function(arity : int, f : KlValue list -> Result) =
    member this.Arity = arity
    member this.Apply(args : KlValue list) = f args
and InStream(read : unit -> int, close : unit -> unit) =
    member this.Read() = read()
    member this.Close() = close()
and OutStream(write: byte -> unit, close: unit -> unit) =
    member this.Write(b: byte) = write b
    member this.Close() = close()
and KlValue = EmptyValue
            | BoolValue      of bool
            | IntValue       of int
            | DecimalValue   of decimal
            | StringValue    of string
            | SymbolValue    of string
            | FunctionValue  of Function
            | VectorValue    of KlValue array
            | ConsValue      of KlValue * KlValue
            | ErrorValue     of string
            | InStreamValue  of InStream
            | OutStreamValue of OutStream
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
    let append env defs = { env with Locals = List.Cons(Map.ofList defs, env.Locals) }
    let append1 env k v = append env [(k, v)]
    let closure eval env (paramz : string list) body =
        new Function(paramz.Length, fun args -> eval (append env (List.zip paramz args)) body) |> FunctionValue
    let vFunc (env : Env) = function
        | FunctionValue f -> f
        | SymbolValue s -> match env.Globals.TryGetValue(s) with
                           | (true, FunctionValue f) -> f
                           | (false, _) -> failwithf "Symbol \"%s\" is undefined" s // TODO this should be an ErrorResult
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
    let resolve (env : Env) symbolName =
        Seq.map (Map.tryFind symbolName) env.Locals
        |> Seq.tryFind Option.isSome
        |> FSharpx.Option.concat
        |> FSharpx.Option.getOrElse (SymbolValue symbolName)
    let rec evalArgs evalE vals args =
        match args with
        | [] -> Choice1Of2 vals
        | arg :: args -> match evalE arg |> go with
                         | ValueResult v -> evalArgs evalE (List.append vals [v]) args
                         | e -> Choice2Of2 e
    let rec eval env = function // TODO have `eval` call `eval0` and run thunks, `eval0` runs `eval` on other exprs
        | EmptyExpr    -> EmptyValue |> ValueResult
        | BoolExpr b   -> boolR b
        | IntExpr n    -> IntValue n |> ValueResult
        | DecimalExpr n-> DecimalValue n |> ValueResult
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
        | DefunExpr (name, paramz, body) ->
            let f = closure eval env paramz body
            env.Globals.[name] <- f
            ValueResult f
        | FreezeExpr expr -> closure eval env [] expr |> ValueResult
        | TrapExpr (pos, body, handler) ->
            match eval env body |> go with
            | ErrorResult e -> eval env handler >>= (fun v -> apply pos (vFunc env v) [ErrorValue e])
            | r -> r
        | AppExpr (pos, f, args) ->
            eval env f >>= (fun v -> FSharpx.Choice.choice (apply pos (vFunc env v)) id (evalArgs (eval env) [] args))

type ConsoleIn(stream: System.IO.Stream) =
    let reader = new System.IO.StreamReader(stream)
    let mutable currentLine = ""
    let mutable currentPos = 0
    member this.Read() = 
        if currentPos >= currentLine.Length then
            currentLine <- reader.ReadLine()
            if System.Object.ReferenceEquals(currentLine, null) then
                -1
            else
                currentLine <- currentLine + "\n"
                currentPos <- 0
                let ch = currentLine.[currentPos]
                currentPos <- currentPos + 1
                (int) ch
        else
            let ch = currentLine.[currentPos]
            currentPos <- currentPos + 1
            (int) ch
    member this.Close() = stream.Close()

module KlBuiltins =
    let inline invalidArgs () = failwith "Wrong number or type of arguments"
    let trueV = BoolValue true
    let falseV = BoolValue false
    let klIntern = function
        | [StringValue s] -> SymbolValue s
        | _ -> invalidArgs ()
    let klStringPos = function
        | [StringValue s; IntValue index] ->
            if index >= 0 && index < s.Length
                then s.[index] |> string |> StringValue |> ValueResult
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
        | IntValue n -> n.ToString()
        | DecimalValue n -> n.ToString()
        | StringValue s -> "\"" + s + "\""
        | SymbolValue s -> s
        | ConsValue (head, tail) -> sprintf "(cons %s %s)" (klStr head) (klStr tail)
        | VectorValue value -> sprintf "(@v%s)" (System.String.Join("", (Array.map (fun s -> " " + klStr s) value)))
        | ErrorValue message -> sprintf "(simple-error \"%s\")" message
        | FunctionValue f -> sprintf "<Function %s>" (f.ToString())
        | InStreamValue s -> sprintf "<InStream %s>" (s.ToString())
        | OutStreamValue s -> sprintf "<OutStream %s>" (s.ToString())
    let rec klToString = function
        | [x : KlValue] -> x |> klStr |> StringValue
        | _ -> invalidArgs ()
    let klIsString = function
        | [StringValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let klIntToString = function
        | [IntValue n] -> int n |> char |> string |> StringValue
        | _ -> invalidArgs ()
    let klStringToInt = function
        | [StringValue s] -> s.[0] |> int |> IntValue
        | _ -> invalidArgs ()
    let klSet env = function
        | [SymbolValue s; x] -> env.Globals.[s] <- x
                                x
        | _ -> invalidArgs ()
    let klValue env = function
        | [SymbolValue s] -> match env.Globals.TryGetValue(s) with
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
        | IntValue x, IntValue y                 -> x = y
        | DecimalValue x, DecimalValue y         -> x = y
        | IntValue x, DecimalValue y             -> decimal x = y
        | DecimalValue x, IntValue y             -> x = decimal y
        | StringValue x, StringValue y           -> x = y
        | SymbolValue x, SymbolValue y           -> x = y
        | InStreamValue x, InStreamValue y       -> x = y
        | OutStreamValue x, OutStreamValue y     -> x = y
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
        | IntValue n -> n |> decimal |> NumberToken
        | DecimalValue n -> n |> NumberToken
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
        | [IntValue length] -> Array.create length EmptyValue |> VectorValue
        | _ -> invalidArgs ()
    let klReadVector = function
        | [VectorValue vector; IntValue index] ->
            if index >= 0 && index < vector.Length
                then vector.[index] |> ValueResult
                else ErrorResult "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klWriteVector = function
        | [VectorValue vector as vv; IntValue index; value] ->
            if index >= 0 && index < vector.Length
                then vector.[index] <- value
                     ValueResult vv
                else ErrorResult "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klIsVector = function
        | [VectorValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let klWriteByte = function
        | [IntValue i; OutStreamValue stream] ->
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.Write(b)
                     b |> int |> IntValue
                else invalidArgs ()
        | _ -> invalidArgs ()
    let klReadByte = function
        | [InStreamValue stream] -> stream.Read() |> IntValue
        | _ -> invalidArgs ()
    let klOpen = function
        | [StringValue path; SymbolValue "in"] ->
            let stream = System.IO.File.OpenRead(path)
            new InStream(stream.ReadByte, stream.Close) |> InStreamValue
        | [StringValue path; SymbolValue "out"] ->
            let stream = System.IO.File.OpenWrite(path)
            new OutStream(stream.WriteByte, stream.Close) |> OutStreamValue
        | _ -> invalidArgs ()
    let klClose = function
        | [InStreamValue stream] -> stream.Close()
                                    EmptyValue
        | [OutStreamValue stream] -> stream.Close()
                                     EmptyValue
        | _ -> invalidArgs ()
    let epoch = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let startTime = System.DateTime.UtcNow
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let klGetTime = function
        // Both run and unix time are in milliseconds
        // ElapsedTicks is in 100 picoseconds/0.1 microseconds
        | [SymbolValue "run"] -> stopwatch.ElapsedTicks / 10000L |> int |> IntValue
        | [SymbolValue "unix"] -> (System.DateTime.UtcNow - epoch).TotalSeconds |> int |> IntValue
        // TODO support "real" time?
        | _ -> invalidArgs ()
    let klAdd = function
        | [IntValue x;     IntValue y]     -> x + y |> IntValue
        | [IntValue x;     DecimalValue y] -> decimal x + y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x + decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x + y |> DecimalValue
        | _ -> invalidArgs ()
    let klSubtract = function
        | [IntValue x;     IntValue y]     -> x - y |> IntValue
        | [IntValue x;     DecimalValue y] -> decimal x - y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x - decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x - y |> DecimalValue
        | _ -> invalidArgs ()
    let klMultiply = function
        | [IntValue x;     IntValue y]     -> x * y |> IntValue
        | [IntValue x;     DecimalValue y] -> decimal x * y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x * decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x * y |> DecimalValue
        | _ -> invalidArgs ()
    let klDivide = function
        | [IntValue x;     IntValue y]     -> decimal x / decimal y |> DecimalValue
        | [IntValue x;     DecimalValue y] -> decimal x / y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x / decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x / y |> DecimalValue
        | _ -> invalidArgs ()
    let klGreaterThan = function
        | [IntValue x;     IntValue y]     -> x > y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x > y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x > decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x > y |> BoolValue
        | _ -> invalidArgs ()
    let klLessThan = function
        | [IntValue x;     IntValue y]     -> x < y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x < y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x < decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x < y |> BoolValue
        | _ -> invalidArgs ()
    let klGreaterThanEqual = function
        | [IntValue x;     IntValue y]     -> x >= y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x >= y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x >= decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x >= y |> BoolValue
        | _ -> invalidArgs ()
    let klLessThanEqual = function
        | [IntValue x;     IntValue y]     -> x <= y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x <= y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x <= decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x <= y |> BoolValue
        | _ -> invalidArgs ()
    let klIsNumber = function
        | [IntValue _] -> trueV
        | [DecimalValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let funR arity f = FunctionValue (new Function (arity, f))
    let funV arity f = funR arity (f >> ValueResult)
    let emptyEnv () = { Globals = new Globals(); Locals = [] }
    let stinput =
        let consoleIn = new ConsoleIn(System.Console.OpenStandardInput())
        new InStream(consoleIn.Read, consoleIn.Close) |> InStreamValue
    let stoutput =
        let consoleOutStream = System.Console.OpenStandardOutput()
        new OutStream(consoleOutStream.WriteByte, consoleOutStream.Close) |> OutStreamValue
    let baseEnv () =
        let env = emptyEnv ()
        let rec install = function
            | [] -> ()
            | (name, value) :: defs ->
                env.Globals.[name] <- value
                install defs
        install [
            "intern",           funV 1 klIntern
            "pos",              funR 2 klStringPos
            "tlstr",            funV 1 klStringTail
            "cn",               funV 2 klStringConcat
            "str",              funV 1 klToString
            "string?",          funV 1 klIsString
            "n->string",        funV 1 klIntToString
            "string->n",        funV 1 klStringToInt
            "set",              funV 2 (klSet env)
            "value",            funV 1 (klValue env)
            "simple-error",     funR 1 klSimpleError
            "error-to-string",  funV 1 klErrorToString
            "cons",             funV 2 klNewCons
            "hd",               funV 1 klHead
            "tl",               funV 1 klTail
            "cons?",            funV 1 klIsCons
            "=",                funV 2 klEquals
            "type",             funV 1 klType
            "eval-kl",          funR 1 (klEval env)
            "absvector",        funV 1 klNewVector
            "<-address",        funR 2 klReadVector
            "address->",        funR 3 klWriteVector
            "absvector?",       funV 1 klIsVector
            "write-byte",       funV 2 klWriteByte
            "read-byte",        funV 1 klReadByte
            "open",             funV 2 klOpen
            "close",            funV 1 klClose
            "get-time",         funV 1 klGetTime
            "+",                funV 2 klAdd
            "-",                funV 2 klSubtract
            "*",                funV 2 klMultiply
            "/",                funV 2 klDivide
            ">",                funV 2 klGreaterThan
            "<",                funV 2 klLessThan
            ">=",               funV 2 klGreaterThanEqual
            "<=",               funV 2 klLessThanEqual
            "number?",          funV 1 klIsNumber
            "*language*",       "F# 3.1" |> StringValue
            "*implementation*", "CLR " + System.Environment.Version.ToString() |> StringValue
            "*port*",           "0.1" |> StringValue
            "*porters*",        "Robert Koeninger" |> StringValue
            "*version*",        "19.2" |> StringValue
            "*stinput*",        stinput
            "*stoutput*",       stoutput
            "*home-directory*", System.Environment.CurrentDirectory |> StringValue
            ]
        env

module KlCompiler =
    let rec compiler = fun (x : KlExpr) -> "fsharp code"
