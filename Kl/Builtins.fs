namespace Kl

open Extensions
open FSharpx.Option
open FSharpx.Choice
open System
open System.IO

module Builtins =
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
    let rec private klEq a b =
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
    let rec private klValueToToken = function
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
        | [v] -> klValueToToken v |> Parser.parse Head |> Evaluator.eval {Locals = []; Globals = globals}
        | _ -> invalidArgs ()
    let klType globals args =
        match args with
        | [x; _] -> x
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
    let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private startTime = DateTime.UtcNow
    let private stopwatch = Diagnostics.Stopwatch.StartNew()
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
