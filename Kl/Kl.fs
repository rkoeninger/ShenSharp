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

type Defines = System.Collections.Generic.Dictionary<string, KlValue>
and Globals = {Symbols: Defines; Functions: Defines}
and Locals = Map<string, KlValue> list
and Function(name: string, arity: int, locals: Locals, f: Globals -> KlValue list -> Work) =
    member this.Name = name
    member this.Arity = arity
    member this.Locals = locals
    member this.Apply(globals: Globals, args: KlValue list) = f globals args
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

type Env = {Globals: Globals; Locals: Locals}

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
    let funcW name arity locals f = new Function(name, arity, locals, f) |> FunctionValue |> ValueResult |> Completed
    let append env defs = {env with Locals = List.Cons(Map.ofList defs, env.Locals)}
    let append1 env k v = append env [(k, v)]
    let closure eval env (paramz: string list) body =
        new Function("Anonymous", paramz.Length, env.Locals, fun _ args -> eval (append env (List.zip paramz args)) body) |> FunctionValue
    let vFunc (env: Env) = function
        | FunctionValue f -> FunctionResult f
        | SymbolValue s ->
            match env.Globals.Functions.GetMaybe(s) with
            | Some (FunctionValue f) -> FunctionResult f
            | Some _ -> sprintf "Symbol \"%s\" does not represent function" s |> FunctionResolveError
            | None -> sprintf "Symbol \"%s\" is undefined" s |> FunctionResolveError
        | _ -> sprintf "Function value or symbol expected" |> FunctionResolveError
    let rec go = function
        | Pending thunk -> thunk.Run() |> go
        | Completed result -> result
    let rec apply pos globals fr (args: KlValue list) =
        match fr with
        | FunctionResolveError e -> ErrorResult e |> Completed
        | FunctionResult f ->
            match args.Length, f.Arity with
            | Greater -> "Too many arguments" |> ErrorResult |> Completed
            | Lesser -> funcW ("Partial " + f.Name)
                              (f.Arity - args.Length)
                              f.Locals
                              (fun globals moreArgs -> apply pos globals fr (List.append args moreArgs))
            | Equal -> match pos with
                       | Head -> f.Apply(globals, args)
                       | Tail -> thunkW (fun () -> f.Apply(globals, args))
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
            env.Globals.Functions.[name] <- f
            f |> ValueResult |> Completed
        | FreezeExpr expr -> closure evalw env [] expr |> ValueResult |> Completed
        | TrapExpr (pos, body, handler) ->
            match eval env body with
            | ErrorResult e -> eval env handler >>= (fun v -> apply pos env.Globals (vFunc env v) [ErrorValue e])
            | r -> Completed r
        | AppExpr (pos, f, args) ->
            eval env f >>= (fun v -> choice (apply pos env.Globals (vFunc env v))
                                            (ErrorResult >> Completed)
                                            (evalArgs (evalw env) [] args))

open System
open System.IO

module KlBuiltins =
    let inline invalidArgs () = failwith "Wrong number or type of arguments"
    let vBool x =
        match x with
        | BoolValue b -> b
        | _ -> failwith "Boolean value expected"
    let trueV = BoolValue true
    let falseV = BoolValue false
    let klIntern _ args =
        match args with
        | [StringValue s] -> SymbolValue s
        | _ -> invalidArgs ()
    let klStringPos _ args =
        match args with
        | [StringValue s; IntValue index] ->
            if index >= 0 && index < s.Length
                then s.[index] |> string |> StringValue |> ValueResult
                else "string index out of bounds" |> ErrorResult
        | _ -> invalidArgs ()
    let klStringTail _ args =
        match args with
        | [StringValue s] -> s.Substring(1) |> StringValue
        | _ -> invalidArgs ()
    let klStringConcat _ args =
        match args with
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
    let klToString _ args =
        match args with
        | [x] -> x |> klStr |> StringValue
        | _ -> invalidArgs ()
    let klIsString _ args =
        match args with
        | [StringValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let klIntToString _ args =
        match args with
        | [IntValue n] -> int n |> char |> string |> StringValue
        | _ -> invalidArgs ()
    let klStringToInt _ args =
        match args with
        | [StringValue s] -> s.[0] |> int |> IntValue
        | _ -> invalidArgs ()
    let klSet globals args =
        match args with
        | [SymbolValue s; x] -> globals.Symbols.[s] <- x
                                x
        | _ -> invalidArgs ()
    let klValue globals args =
        match args with
        | [SymbolValue s] ->
            match globals.Symbols.GetMaybe(s) with
            | Some v -> ValueResult v
            | None -> sprintf "Symbol \"%s\" is undefined" s |> ErrorResult
        | _ -> invalidArgs ()
    let klSimpleError _ args =
        match args with
        | [StringValue s] -> ErrorResult s
        | _ -> invalidArgs ()
    let klErrorToString _ args =
        match args with
        | [ErrorValue s] -> StringValue s
        | _ -> invalidArgs ()
    let klNewCons _ args =
        match args with
        | [x; y] -> ConsValue (x, y)
        | _ -> invalidArgs ()
    let klHead _ args =
        match args with
        | [ConsValue (x, _)] -> x
        | _ -> invalidArgs ()
    let klTail _ args =
        match args with
        | [ConsValue (_, y)] -> y
        | _ -> invalidArgs ()
    let klIsCons _ args =
        match args with
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
    let klEquals _ args =
        match args with
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
    let klEval globals args =
        match args with
        | [v] -> klValueToToken v |> KlParser.parse Head |> KlEvaluator.eval {Locals = []; Globals = globals}
        | _ -> invalidArgs ()
    let klType globals args =
        match args with
        | [x; _] -> x // TODO label the type of an expression (what does that mean?)
        | _ -> invalidArgs ()
    let klNewVector _ args =
        match args with
        | [IntValue length] -> Array.create length (SymbolValue "fail!") |> VectorValue
        | _ -> invalidArgs ()
    let klReadVector _ args =
        match args with
        | [VectorValue vector; IntValue index] ->
            if index >= 0 && index < vector.Length
                then vector.[index] |> ValueResult
                else ErrorResult "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klWriteVector _ args =
        match args with
        | [VectorValue vector as vv; IntValue index; value] ->
            if index >= 0 && index < vector.Length
                then vector.[index] <- value
                     ValueResult vv
                else ErrorResult "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klIsVector _ args =
        match args with
        | [VectorValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let klWriteByte _ args =
        match args with
        | [IntValue i; OutStreamValue stream] ->
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.Write(b)
                     b |> int |> IntValue
                else invalidArgs ()
        | _ -> invalidArgs ()
    let klReadByte _ args =
        match args with
        | [InStreamValue stream] -> stream.Read() |> IntValue
        | _ -> invalidArgs ()
    let klOpen _ args =
        match args with
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
    let klClose _ args =
        match args with
        | [InStreamValue stream]  -> stream.Close(); EmptyValue
        | [OutStreamValue stream] -> stream.Close(); EmptyValue
        | _ -> invalidArgs ()
    let epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let startTime = DateTime.UtcNow
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let klGetTime _ args = // All returned values are in milliseconds
        match args with
        | [SymbolValue "run"] -> stopwatch.ElapsedTicks / 10000L |> int |> IntValue
        | [SymbolValue "unix"] -> (DateTime.UtcNow - epoch).TotalSeconds |> int |> IntValue
        | _ -> invalidArgs ()
    let klAdd _ args =
        match args with
        | [IntValue x;     IntValue y]     -> x + y |> IntValue
        | [IntValue x;     DecimalValue y] -> decimal x + y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x + decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x + y |> DecimalValue
        | _ -> invalidArgs ()
    let klSubtract _ args =
        match args with
        | [IntValue x;     IntValue y]     -> x - y |> IntValue
        | [IntValue x;     DecimalValue y] -> decimal x - y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x - decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x - y |> DecimalValue
        | _ -> invalidArgs ()
    let klMultiply _ args =
        match args with
        | [IntValue x;     IntValue y]     -> x * y |> IntValue
        | [IntValue x;     DecimalValue y] -> decimal x * y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x * decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x * y |> DecimalValue
        | _ -> invalidArgs ()
    let klDivide _ args =
        match args with
        | [IntValue x;     IntValue y]     -> decimal x / decimal y |> DecimalValue
        | [IntValue x;     DecimalValue y] -> decimal x / y |> DecimalValue
        | [DecimalValue x; IntValue y]     -> x / decimal y |> DecimalValue
        | [DecimalValue x; DecimalValue y] -> x / y |> DecimalValue
        | _ -> invalidArgs ()
    let klGreaterThan _ args =
        match args with
        | [IntValue x;     IntValue y]     -> x > y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x > y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x > decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x > y |> BoolValue
        | _ -> invalidArgs ()
    let klLessThan _ args =
        match args with
        | [IntValue x;     IntValue y]     -> x < y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x < y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x < decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x < y |> BoolValue
        | _ -> invalidArgs ()
    let klGreaterThanEqual _ args =
        match args with
        | [IntValue x;     IntValue y]     -> x >= y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x >= y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x >= decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x >= y |> BoolValue
        | _ -> invalidArgs ()
    let klLessThanEqual _ args =
        match args with
        | [IntValue x;     IntValue y]     -> x <= y |> BoolValue
        | [IntValue x;     DecimalValue y] -> decimal x <= y |> BoolValue
        | [DecimalValue x; IntValue y]     -> x <= decimal y |> BoolValue
        | [DecimalValue x; DecimalValue y] -> x <= y |> BoolValue
        | _ -> invalidArgs ()
    let klIsNumber _ args =
        match args with
        | [IntValue _] -> trueV
        | [DecimalValue _] -> trueV
        | [_] -> falseV
        | _ -> invalidArgs ()
    let trapError r c =
        match r with
        | ErrorResult e -> c (ErrorValue e)
        | r -> r
    let funW name arity f = name, FunctionValue(new Function(name, arity, [], f))
    let funR name arity f = funW name arity (fun globals args -> Completed (f globals args))
    let funV name arity f = funR name arity (fun globals args -> ValueResult (f globals args))
    let stinput =
        let consoleIn = new ConsoleIn(Console.OpenStandardInput())
        InStreamValue {Read = consoleIn.Read; Close = consoleIn.Close}
    let stoutput =
        let consoleOutStream = Console.OpenStandardOutput()
        OutStreamValue {Write = consoleOutStream.WriteByte; Close = consoleOutStream.Close}
    let rec install (functions: Defines) = function
        | [] -> ()
        | (name, value) :: defs ->
            functions.[name] <- value
            install functions defs
    let newGlobals() = {Symbols = new Defines(); Functions = new Defines()}
    let emptyEnv () = {Globals = newGlobals(); Locals = []}
    let baseEnv () =
        let env = emptyEnv ()
        install env.Globals.Functions [
            funV "intern"          1 klIntern
            funR "pos"             2 klStringPos
            funV "tlstr"           1 klStringTail
            funV "cn"              2 klStringConcat
            funV "str"             1 klToString
            funV "string?"         1 klIsString
            funV "n->string"       1 klIntToString
            funV "string->n"       1 klStringToInt
            funV "set"             2 klSet
            funR "value"           1 klValue
            funR "simple-error"    1 klSimpleError
            funV "error-to-string" 1 klErrorToString
            funV "cons"            2 klNewCons
            funV "hd"              1 klHead
            funV "tl"              1 klTail
            funV "cons?"           1 klIsCons
            funV "="               2 klEquals
            funV "type"            1 klType
            funR "eval-kl"         1 klEval
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
        install env.Globals.Symbols [
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
            Array.fill array start (stop - start) fillValue
            vector
        | _ -> invalidArgs()
    let rec klElement = function
        | [_; EmptyValue] -> falseV
        | [key; ConsValue(head, _)] when klEq key head -> trueV
        | [key; ConsValue(_, tail)] -> klElement [key; tail]
        | _ -> invalidArgs()
