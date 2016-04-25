namespace Kl

open Extensions
open System
open System.IO

module Builtins =

    let inline invalidArgs () = failwith "Wrong number or type of arguments"
    let klIntern _ args =
        match args with
        | [StringValue s] -> SymbolValue s
        | _ -> invalidArgs ()
    let klStringPos _ args =
        match args with
        | [StringValue s; IntValue index] ->
            if index >= 0 && index < s.Length
                then Ok(StringValue(string s.[index]))
                else Err "string index out of bounds"
        | _ -> invalidArgs ()
    let klStringTail _ args =
        match args with
        | [StringValue s] -> StringValue(s.Substring 1)
        | _ -> invalidArgs ()
    let klStringConcat _ args =
        match args with
        | [StringValue x; StringValue y] -> StringValue(x + y)
        | _ -> invalidArgs ()
    let klToString _ args =
        match args with
        | [x] -> StringValue(Values.toStr x)
        | _ -> invalidArgs ()
    let klIsString _ args =
        match args with
        | [StringValue _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> invalidArgs ()
    let klIntToString _ args =
        match args with
        | [IntValue n] -> StringValue(string(char(int n)))
        | _ -> invalidArgs ()
    let klStringToInt _ args =
        match args with
        | [StringValue s] -> IntValue(int s.[0])
        | _ -> invalidArgs ()
    let klSet globals args =
        match args with
        | [SymbolValue s; x] ->
            globals.Symbols.[s] <- x
            x
        | _ -> invalidArgs ()
    let klValue globals args =
        match args with
        | [SymbolValue s] ->
            match globals.Symbols.GetMaybe(s) with
            | Some v -> Ok v
            | None -> Err(sprintf "Symbol \"%s\" is undefined" s)
        | _ -> invalidArgs ()
    let klSimpleError _ args =
        match args with
        | [StringValue s] -> Err s
        | _ -> invalidArgs ()
    let klErrorToString _ args =
        match args with
        | [ErrorValue s] -> StringValue s
        | _ -> invalidArgs ()
    let klNewCons _ args =
        match args with
        | [x; y] -> ConsValue(x, y)
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
        | [ConsValue _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> invalidArgs ()
    let klEquals _ args =
        match args with
        | [x; y] -> BoolValue(Values.eq x y)
        | _ -> invalidArgs ()
    let klEval globals args =
        match args with
        | [v] -> Values.toToken v |> Parser.rootParse |> Evaluator.rootEval globals
        | _ -> invalidArgs ()
    let klType globals args =
        match args with
        | [x; _] -> x
        | _ -> invalidArgs ()
    let klNewVector _ args =
        match args with
        | [IntValue length] ->
            let failSymbol = SymbolValue "fail!"
            VectorValue(Array.create length failSymbol)
        | _ -> invalidArgs ()
    let klReadVector _ args =
        match args with
        | [VectorValue vector; IntValue index] ->
            if index >= 0 && index < vector.Length
                then Ok vector.[index]
                else Err "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klWriteVector _ args =
        match args with
        | [VectorValue vector as vv; IntValue index; value] ->
            if index >= 0 && index < vector.Length
                then vector.[index] <- value
                     Ok vv
                else Err "Vector index out of bounds"
        | _ -> invalidArgs ()
    let klIsVector _ args =
        match args with
        | [VectorValue _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> invalidArgs ()
    let klWriteByte _ args =
        match args with
        | [IntValue i; OutStreamValue stream] ->
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.Write(b)
                     IntValue(int b)
                else invalidArgs ()
        | _ -> invalidArgs ()
    let klReadByte _ args =
        match args with
        | [InStreamValue stream] -> IntValue(stream.Read())
        | _ -> invalidArgs ()
    let klOpen _ args =
        match args with
        | [StringValue path; SymbolValue "in"] ->
            try let stream = File.OpenRead(path)
                Ok(InStreamValue{Read = stream.ReadByte; Close = stream.Close})
            with | :? IOException as e -> Err e.Message
                 | e -> raise e
        | [StringValue path; SymbolValue "out"] ->
            try let stream = File.OpenWrite(path)
                Ok(OutStreamValue{Write = stream.WriteByte; Close = stream.Close})
            with | :? IOException as e -> Err e.Message
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
        | [SymbolValue "run"] -> Ok(IntValue(int (stopwatch.ElapsedTicks / 10000L)))
        | [SymbolValue "unix"] -> Ok(IntValue(int (DateTime.UtcNow - epoch).TotalSeconds))
        | _ -> Err "get-time only takes 'run or 'unix as an argument"
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
        | [IntValue _] -> Values.truev
        | [DecimalValue _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> invalidArgs ()
    let trapError r c =
        match r with
        | Err e -> c (ErrorValue e)
        | r -> r
    let stinput =
        let consoleIn = new ConsoleIn(Console.OpenStandardInput())
        InStreamValue {Read = consoleIn.Read; Close = consoleIn.Close}
    let stoutput =
        let consoleOutStream = Console.OpenStandardOutput()
        OutStreamValue {Write = consoleOutStream.WriteByte; Close = consoleOutStream.Close}
    let klPrint = function
        | [x] -> Console.Write(Values.toStr x)
                 EmptyValue
        | _ -> invalidArgs()
    let klFillVector = function
        | [VectorValue array as vector; IntValue stop; IntValue start; fillValue] ->
            Array.fill array start (stop - start) fillValue
            vector
        | _ -> invalidArgs()
    let rec klElement = function
        | [_; EmptyValue] -> Values.falsev
        | [key; ConsValue(head, _)] when Values.eq key head -> Values.truev
        | [key; ConsValue(_, tail)] -> klElement [key; tail]
        | _ -> invalidArgs()
