namespace Kl

type KlToken = BoolToken   of bool
             | NumberToken of decimal
             | StringToken of string
             | SymbolToken of string
             | ComboToken  of KlToken list

type Position = Head | Tail

type KlExpr = EmptyExpr
            | BoolExpr    of bool
            | IntExpr     of int
            | DecimalExpr of decimal
            | StringExpr  of string
            | SymbolExpr  of string
            | AndExpr     of KlExpr * KlExpr
            | OrExpr      of KlExpr * KlExpr
            | IfExpr      of KlExpr * KlExpr * KlExpr
            | CondExpr    of (KlExpr * KlExpr) list
            | LetExpr     of string * KlExpr * KlExpr
            | LambdaExpr  of string * KlExpr
            | DefunExpr   of string * string list * KlExpr
            | FreezeExpr  of KlExpr
            | TrapExpr    of Position * KlExpr * KlExpr
            | AppExpr     of Position * KlExpr * KlExpr list

type [<ReferenceEquality>] InStream = {Read: unit -> int; Close: unit -> unit}
type [<ReferenceEquality>] OutStream = {Write: byte -> unit; Close: unit -> unit}

type Globals = System.Collections.Generic.Dictionary<string, KlValue>
and Locals = Map<string, KlValue> list
and Function(name: string, arity: int, f: KlValue list -> Work) =
    member this.Name = name
    member this.Arity = arity
    member this.Apply(args: KlValue list) = f args
    override this.ToString() = this.Name
and Thunk(cont: unit -> Work) =
    member this.Run = cont
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
and Result = ValueResult of KlValue
           | ErrorResult of string
and Work = Completed of Result
         | Pending   of Thunk

type Env = {SymbolDefinitions: Globals; FunctionDefinitions: Globals; Locals: Locals}

// TODO find some other way to do this
type FunctionResolveResult = FunctionResult of Function
                           | FunctionResolveError of string

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

module Extensions =
    let (|Integral|Fractional|) x = if x % 1.0m = 0m then Integral else Fractional
    let (|Greater|Equal|Lesser|) (x, y) = if x > y then Greater elif x < y then Lesser else Equal
    type System.Collections.Generic.Dictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue(key) with
            | true, x -> Some x
            | false, _ -> None
open Extensions

open FParsec

// Tokenizer is strict about spacing. It will not handle extra spaces inside of parens.
// TODO should we worry about this?
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

module KlParser =
    let tSymbol = function
        | (SymbolToken s) -> s
        | _ -> failwith "Symbol expected"
    let rec parse pos = function
        | ComboToken [] -> EmptyExpr
        | BoolToken b -> BoolExpr b
        | NumberToken n ->
            match n with
            | Integral -> IntExpr (int n)
            | Fractional -> DecimalExpr n
        | StringToken s -> StringExpr s
        | SymbolToken "true" -> BoolExpr true
        | SymbolToken "false" -> BoolExpr false
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

open FSharpx.Option
open FSharpx.Choice

module KlEvaluator =
    let vBool x =
        match x with
        | BoolValue b -> b
        | _ -> failwith "Boolean value expected"
    let trueR = BoolValue true |> ValueResult
    let falseR = BoolValue false |> ValueResult
    let trueW = Completed trueR
    let falseW = Completed falseR
    let branch f g x y v = if vBool v then f x else g y
    let branch1 f = branch f f
    let thunkW f = new Thunk(f) |> Pending
    let funcW name arity f = new Function(name, arity, f) |> FunctionValue |> ValueResult |> Completed
    let append env defs = {env with Locals = List.Cons(Map.ofList defs, env.Locals)}
    let append1 env k v = append env [(k, v)]
    let closure eval env (paramz: string list) body =
        new Function("Anonymous", paramz.Length, fun args -> eval (append env (List.zip paramz args)) body) |> FunctionValue
    let vFunc (env: Env) = function
        | FunctionValue f -> FunctionResult f
        | SymbolValue s ->
            match env.FunctionDefinitions.GetMaybe(s) with
            | Some (FunctionValue f) -> FunctionResult f
            | Some _ -> sprintf "Symbol \"%s\" does not represent function" s |> FunctionResolveError
            | None -> sprintf "Symbol \"%s\" is undefined" s |> FunctionResolveError
        | _ -> sprintf "Function value or symbol expected" |> FunctionResolveError
    let rec go = function
        | Pending thunk -> thunk.Run() |> go
        | Completed result -> result
    let rec apply pos fr (args: KlValue list) =
        match fr with
        | FunctionResolveError e -> ErrorResult e |> Completed
        | FunctionResult f ->
            match args.Length, f.Arity with
            | Greater -> "Too many arguments" |> ErrorResult |> Completed
            | Lesser -> funcW ("Partial " + f.Name)
                              (f.Arity - args.Length)
                              (List.append args >> apply pos fr)
            | Equal -> match pos with
                       | Head -> f.Apply args
                       | Tail -> thunkW (fun () -> f.Apply args)
    let (>>=) result f =
        match result with
        | ValueResult value -> f value
        | ErrorResult _ as error -> Completed error
    let (>>>=) result f =
        match result with
        | ValueResult value -> f value |> Completed
        | ErrorResult _ as error -> error |> Completed
    let resolve locals symbolName =
        Seq.map (Map.tryFind symbolName) locals
        |> Seq.tryFind Option.isSome
        |> concat
        |> getOrElse (SymbolValue symbolName)
    let rec evalArgs evalE vals args =
        match args with
        | [] -> Choice1Of2 vals
        | arg :: args ->
            match evalE arg |> go with
            | ValueResult v -> evalArgs evalE (List.append vals [v]) args
            | ErrorResult e -> Choice2Of2 e
    let rec eval env expr = evalw env expr |> go
    and evalw env = function
        | EmptyExpr     -> EmptyValue |> ValueResult |> Completed
        | BoolExpr b    -> BoolValue b |> ValueResult |> Completed
        | IntExpr n     -> IntValue n |> ValueResult |> Completed
        | DecimalExpr n -> DecimalValue n |> ValueResult |> Completed
        | StringExpr s  -> StringValue s |> ValueResult |> Completed
        | SymbolExpr "true"  -> trueW
        | SymbolExpr "false" -> falseW
        | SymbolExpr s       -> resolve env.Locals s |> ValueResult |> Completed
        | AndExpr (left, right) -> eval env left >>= branch (evalw env) id right falseW
        | OrExpr  (left, right) -> eval env left >>= branch id (evalw env) trueW right
        | IfExpr (condition, ifTrue, ifFalse) -> eval env condition >>= branch1 (evalw env) ifTrue ifFalse
        | CondExpr clauses ->
            let rec evalClauses = function
                | (condition, ifTrue) :: rest -> eval env condition >>= branch (evalw env) evalClauses ifTrue rest
                | [] -> failwith "No condition was true"
            evalClauses clauses
        | LetExpr (symbol, binding, body) -> eval env binding >>>= (fun v -> eval (append1 env symbol v) body)
        | LambdaExpr (param, body) -> closure evalw env [param] body |> ValueResult |> Completed
        | DefunExpr (name, paramz, body) ->
            let f = closure evalw env paramz body
            env.FunctionDefinitions.[name] <- f
            f |> ValueResult |> Completed
        | FreezeExpr expr -> closure evalw env [] expr |> ValueResult |> Completed
        | TrapExpr (pos, body, handler) ->
            match eval env body with
            | ErrorResult e -> eval env handler >>= (fun v -> apply pos (vFunc env v) [ErrorValue e])
            | r -> Completed r
        | AppExpr (pos, f, args) ->
            eval env f >>= (fun v -> choice (apply pos (vFunc env v))
                                            (ErrorResult >> Completed)
                                            (evalArgs (evalw env) [] args))

open System
open System.IO

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
        | VectorValue value -> sprintf "(@v%s)" (String.Join("", (Array.map (fun s -> " " + klStr s) value)))
        | ErrorValue message -> sprintf "(simple-error \"%s\")" message
        | FunctionValue f -> sprintf "<Function %s>" (f.ToString())
        | InStreamValue s -> sprintf "<InStream %s>" (s.ToString())
        | OutStreamValue s -> sprintf "<OutStream %s>" (s.ToString())
    let rec klToString = function
        | [x] -> x |> klStr |> StringValue
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
        | [SymbolValue s; x] -> env.SymbolDefinitions.[s] <- x
                                x
        | _ -> invalidArgs ()
    let klValue env = function
        | [SymbolValue s] ->
            match env.SymbolDefinitions.GetMaybe(s) with
            | Some v -> ValueResult v
            | None -> sprintf "Symbol \"%s\" is undefined" s |> ErrorResult
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
    let rec klEq a b =
        match a, b with
        | EmptyValue,         EmptyValue         -> true
        | BoolValue x,        BoolValue y        -> x = y
        | IntValue x,         IntValue y         -> x = y
        | DecimalValue x,     DecimalValue y     -> x = y
        | IntValue x,         DecimalValue y     -> decimal x = y
        | DecimalValue x,     IntValue y         -> x = decimal y
        | StringValue x,      StringValue y      -> x = y
        | SymbolValue x,      SymbolValue y      -> x = y
        | InStreamValue x,    InStreamValue y    -> x = y
        | OutStreamValue x,   OutStreamValue y   -> x = y
        | FunctionValue x,    FunctionValue y    -> x = y
        | ErrorValue x,       ErrorValue y       -> x = y
        | ConsValue (x1, x2), ConsValue (y1, y2) -> klEq x1 y1 && klEq x2 y2
        | VectorValue xs,     VectorValue ys     -> xs.Length = ys.Length && Array.forall2 klEq xs ys
        | (_, _) -> false
    let klEquals = function
        | [x; y] -> klEq x y |> BoolValue
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
        | [IntValue length] -> Array.create length (SymbolValue "fail!") |> VectorValue
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
            try let stream = File.OpenRead(path)
                InStreamValue {Read = stream.ReadByte; Close = stream.Close} |> ValueResult
            with | :? IOException as e -> ErrorResult e.Message
                 | e -> raise e
        | [StringValue path; SymbolValue "out"] ->
            try let stream = File.OpenWrite(path)
                OutStreamValue {Write = stream.WriteByte; Close = stream.Close} |> ValueResult
            with | :? IOException as e -> ErrorResult e.Message
                 | e -> raise e
        | _ -> invalidArgs ()
    let klClose = function
        | [InStreamValue stream]  -> stream.Close(); EmptyValue
        | [OutStreamValue stream] -> stream.Close(); EmptyValue
        | _ -> invalidArgs ()
    let epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let startTime = DateTime.UtcNow
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let klGetTime = function // All returned values are in milliseconds
        | [SymbolValue "run"] -> stopwatch.ElapsedTicks / 10000L |> int |> IntValue
        | [SymbolValue "unix"] -> (DateTime.UtcNow - epoch).TotalSeconds |> int |> IntValue
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
    let funW name arity f = name, FunctionValue(new Function(name, arity, f))
    let funR name arity f = funW name arity (f >> Completed)
    let funV name arity f = funR name arity (f >> ValueResult)
    let stinput =
        let consoleIn = new ConsoleIn(Console.OpenStandardInput())
        InStreamValue {Read = consoleIn.Read; Close = consoleIn.Close}
    let stoutput =
        let consoleOutStream = Console.OpenStandardOutput()
        OutStreamValue {Write = consoleOutStream.WriteByte; Close = consoleOutStream.Close}
    let rec install (globals: System.Collections.Generic.Dictionary<string, KlValue>) = function
        | [] -> ()
        | (name, value) :: defs ->
            globals.[name] <- value
            install globals defs
    let emptyEnv () = {SymbolDefinitions = new Globals(); FunctionDefinitions = new Globals(); Locals = []}
    let baseEnv () =
        let env = emptyEnv ()
        install env.FunctionDefinitions [
            funV "intern"          1 klIntern
            funR "pos"             2 klStringPos
            funV "tlstr"           1 klStringTail
            funV "cn"              2 klStringConcat
            funV "str"             1 klToString
            funV "string?"         1 klIsString
            funV "n->string"       1 klIntToString
            funV "string->n"       1 klStringToInt
            funV "set"             2 (klSet env)
            funR "value"           1 (klValue env)
            funR "simple-error"    1 klSimpleError
            funV "error-to-string" 1 klErrorToString
            funV "cons"            2 klNewCons
            funV "hd"              1 klHead
            funV "tl"              1 klTail
            funV "cons?"           1 klIsCons
            funV "="               2 klEquals
            funV "type"            1 klType
            funR "eval-kl"         1 (klEval env)
            funV "absvector"       1 klNewVector
            funR "<-address"       2 klReadVector
            funR "address->"       3 klWriteVector
            funV "absvector?"      1 klIsVector
            funV "write-byte"      2 klWriteByte
            funV "read-byte"       1 klReadByte
            funR "open"            2 klOpen
            funV "close"           1 klClose
            funV "get-time"        1 klGetTime
            funV "+"               2 klAdd
            funV "-"               2 klSubtract
            funV "*"               2 klMultiply
            funV "/"               2 klDivide
            funV ">"               2 klGreaterThan
            funV "<"               2 klLessThan
            funV ">="              2 klGreaterThanEqual
            funV "<="              2 klLessThanEqual
            funV "number?"         1 klIsNumber
        ]
        install env.SymbolDefinitions [
            "*language*",       "F# 3.1" |> StringValue
            "*implementation*", "CLR " + Environment.Version.ToString() |> StringValue
            "*port*",           "0.1" |> StringValue
            "*porters*",        "Robert Koeninger" |> StringValue
            "*version*",        "19.2" |> StringValue
            "*stinput*",        stinput
            "*stoutput*",       stoutput
        ]
        env
    let klPrint = function
        | [x] -> Console.Write(klStr x)
                 EmptyValue
        | _ -> invalidArgs()
    let klFillVector = function
        | [VectorValue array as vector; IntValue stop; IntValue start; fillValue] ->
            for i = start to stop do
                array.[i] <- fillValue
            vector
        | _ -> invalidArgs()
    let rec klElement = function
        | [_; EmptyValue] -> falseV
        | [element; ConsValue(head, tail)] ->
            if klEq element head then
                trueV
            else
                klElement [element; tail]
        | _ -> invalidArgs()
