namespace Kl

open Extensions
open System
open System.Diagnostics
open System.IO

module Builtins =

    let inline invalidArgs () = failwith "Wrong number or type of arguments"

    let private arityErr name expected (args: 'a list) =
        Err(sprintf "%s expects %i argument(s), but given %i" name expected args.Length)

    let private typeErr1 name type1 =
        Err(sprintf "%s expects a(n) %s" name type1)

    let private typeErr2 name type1 type2 =
        Err(sprintf "%s expects a(n) %s and a(n) %s" name type1 type2)

    let private typeErr3 name type1 type2 type3 =
        Err(sprintf "%s expects a(n) %s, a(n) %s and a(n) %s" name type1 type2 type3)

    let private typeErr4 name type1 type2 type3 type4 =
        Err(sprintf "%s expects a(n) %s, a(n) %s, a(n) %s and a(n) %s" name type1 type2 type3 type4)

    let klIntern _ args =
        match args with
        | [StringValue s] -> Ok(SymbolValue s)
        | [_] -> typeErr1 "intern" "string"
        | _ -> arityErr "intern" 1 args

    let klStringPos _ args =
        match args with
        | [StringValue s; IntValue index] ->
            if index >= 0 && index < s.Length
                then Ok(StringValue(string s.[index]))
                else Err(sprintf "Index %i out of bounds for string of length %i" index s.Length)
        | [_; _] -> typeErr2 "pos" "string" "int"
        | _ -> arityErr "pos" 2 args

    let klStringTail _ args =
        match args with
        | [StringValue s] ->
            if (s.Length > 0)
                then Ok(StringValue(s.Substring 1))
                else Err "strtl expects a non-empty string"
        | [_] -> typeErr1 "strtl" "string"
        | _ -> arityErr "strtl" 1 args

    let klStringConcat _ args =
        match args with
        | [StringValue x; StringValue y] -> Ok(StringValue(x + y))
        | [_; _] -> typeErr2 "cn" "string" "string"
        | _ -> arityErr "cn" 2 args

    let klToString _ args =
        match args with
        | [x] -> Ok(StringValue(Values.toStr x))
        | _ -> arityErr "str" 1 args

    let klIsString _ args =
        match args with
        | [StringValue _] -> Values.truer
        | [_] -> Values.falser
        | _ -> arityErr "string?" 1 args

    let klIntToString _ args =
        match args with
        | [IntValue n] -> Ok(StringValue(string(char(int n))))
        | [_] -> typeErr1 "n->string" "int"
        | _ -> arityErr "n->string" 1 args

    let klStringToInt _ args =
        match args with
        | [StringValue s] -> Ok(IntValue(int s.[0]))
        | [_] -> typeErr1 "string->n" "string"
        | _ -> arityErr "string->n" 1 args

    let klSet globals args =
        match args with
        | [SymbolValue s; x] ->
            globals.Symbols.[s] <- x
            Ok x
        | [_; _] -> typeErr2 "set" "symbol" "value"
        | _ -> arityErr "set" 2 args

    let klValue globals args =
        match args with
        | [SymbolValue s] ->
            match globals.Symbols.GetMaybe(s) with
            | Some v -> Ok v
            | None -> Err(sprintf "Symbol \"%s\" is undefined" s)
        | [_] -> typeErr1 "value" "symbol"
        | _ -> arityErr "value" 1 args

    let klSimpleError _ args =
        match args with
        | [StringValue s] -> Err s
        | [_] -> typeErr1 "simple-error" "string"
        | _ -> arityErr "simple-error" 1 args

    let klErrorToString _ args =
        match args with
        | [ErrorValue s] -> Ok(StringValue s)
        | [_] -> typeErr1 "error-to-string" "error"
        | _ -> arityErr "error-to-string" 1 args

    let klNewCons _ args =
        match args with
        | [x; y] -> Ok(ConsValue(x, y))
        | _ -> arityErr "cons" 2 args

    let klHead _ args =
        match args with
        | [ConsValue (x, _)] -> Ok x
        | [_] -> typeErr1 "hd" "cons"
        | _ -> arityErr "hd" 1 args

    let klTail _ args =
        match args with
        | [ConsValue (_, y)] -> Ok y
        | [_] -> typeErr1 "tl" "cons"
        | _ -> arityErr "tl" 1 args

    let klIsCons _ args =
        match args with
        | [ConsValue _] -> Values.truer
        | [_] -> Values.falser
        | _ -> arityErr "cons?" 1 args

    let klEquals _ args =
        match args with
        | [x; y] -> Ok(BoolValue(Values.eq x y))
        | _ -> arityErr "=" 2 args

    let klEval globals args =
        match args with
        | [v] ->
            Values.toToken v
            |> Parser.rootParse
            |> Evaluator.rootEval globals
        | _ -> arityErr "eval-kl" 1 args

    let klType globals args =
        match args with
        | [x; _] -> Ok x
        | _ -> arityErr "type" 2 args

    let klNewVector _ args =
        match args with
        | [IntValue length] ->
            let failSymbol = SymbolValue "fail!"
            Ok(VectorValue(Array.create length failSymbol))
        | [_] -> typeErr1 "absvector" "int"
        | _ -> arityErr "absvector" 1 args

    let klReadVector _ args =
        match args with
        | [VectorValue vector; IntValue index] ->
            if index >= 0 && index < vector.Length
                then Ok vector.[index]
                else Err(sprintf "Index %i out of bounds for vector of length %i" index vector.Length)
        | [_; _] -> typeErr2 "<-address" "vector" "int"
        | _ -> arityErr "<-address" 2 args

    let klWriteVector _ args =
        match args with
        | [VectorValue vector as vv; IntValue index; value] ->
            if index >= 0 && index < vector.Length
                then vector.[index] <- value
                     Ok vv
                else Err(sprintf "Index %i out of bounds for vector of length %i" index vector.Length)
        | [_; _; _] -> typeErr3 "address->" "vector" "int" "value"
        | _ -> arityErr "address->" 3 args

    let klIsVector _ args =
        match args with
        | [VectorValue _] -> Values.truer
        | [_] -> Values.falser
        | _ -> arityErr "absvector?" 1 args

    let klWriteByte _ args =
        match args with
        | [IntValue i; OutStreamValue stream] ->
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.Write(b)
                     Ok(IntValue(int b))
                else Err(sprintf "int value %i is exceeds the range of a byte" i)
        | [_; _] -> typeErr2 "write-byte" "int" "out-stream"
        | _ -> arityErr "write-byte" 2 args

    let klReadByte _ args =
        match args with
        | [InStreamValue stream] -> Ok(IntValue(stream.Read()))
        | [_] -> typeErr1 "read-byte" "in-stream"
        | _ -> arityErr "read-byte" 1 args

    let klOpen _ args =
        match args with
        | [StringValue path; SymbolValue "in"] ->
            try let stream = File.OpenRead(path)
                Ok(InStreamValue{Read = stream.ReadByte; Close = stream.Close})
            with e -> Err e.Message
        | [StringValue path; SymbolValue "out"] ->
            try let stream = File.OpenWrite(path)
                Ok(OutStreamValue{Write = stream.WriteByte; Close = stream.Close})
            with e -> Err e.Message
        | [StringValue _; SymbolValue s] ->
            Err(sprintf "open expects symbol 'in or 'out as 2nd argument, not '%s" s)
        | [_; _] -> typeErr2 "open" "string" "symbol"
        | _ -> arityErr "open" 2 args

    let klClose _ args =
        match args with
        | [InStreamValue stream] ->
            stream.Close()
            Ok EmptyValue
        | [OutStreamValue stream] ->
            stream.Close()
            Ok EmptyValue
        | [_] -> typeErr1 "close" "stream"
        | _ -> arityErr "close" 1 args

    let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private startTime = DateTime.UtcNow
    let private stopwatch = Stopwatch.StartNew()

    /// <remarks>
    /// All returned values are in milliseconds
    /// </remarks>
    let klGetTime _ args =
        match args with
        | [SymbolValue "run"] -> Ok(IntValue(int (stopwatch.ElapsedTicks / 10000L)))
        | [SymbolValue "unix"] -> Ok(IntValue(int (DateTime.UtcNow - epoch).TotalSeconds))
        | [SymbolValue s] -> Err(sprintf "get-time expects symbols 'run or 'unix' as argument, not %s" s)
        | [_] -> typeErr1 "get-time" "symbol"
        | _ -> arityErr "get-time" 1 args

    let klAdd _ args =
        match args with
        | [IntValue x;     IntValue y]     -> Ok(IntValue(x + y))
        | [IntValue x;     DecimalValue y] -> Ok(DecimalValue(decimal x + y))
        | [DecimalValue x; IntValue y]     -> Ok(DecimalValue(x + decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(DecimalValue(x + y))
        | [_; _] -> typeErr2 "+" "int/decimal" "int/decimal"
        | _ -> arityErr "+" 2 args

    let klSubtract _ args =
        match args with
        | [IntValue x;     IntValue y]     -> Ok(IntValue(x - y))
        | [IntValue x;     DecimalValue y] -> Ok(DecimalValue(decimal x - y))
        | [DecimalValue x; IntValue y]     -> Ok(DecimalValue(x - decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(DecimalValue(x - y))
        | [_; _] -> typeErr2 "-" "int/decimal" "int/decimal"
        | _ -> arityErr "-" 2 args

    let klMultiply _ args =
        match args with
        | [IntValue x;     IntValue y]     -> Ok(IntValue(x * y))
        | [IntValue x;     DecimalValue y] -> Ok(DecimalValue(decimal x * y))
        | [DecimalValue x; IntValue y]     -> Ok(DecimalValue(x * decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(DecimalValue(x * y))
        | [_; _] -> typeErr2 "*" "int/decimal" "int/decimal"
        | _ -> arityErr "*" 2 args

    let klDivide _ args =
        match args with
        | [_; IntValue 0] -> Err "Division by zero"
        | [_; DecimalValue 0m] -> Err "Division by zero"
        | [IntValue x;     IntValue y]     -> Ok(DecimalValue(decimal x / decimal y))
        | [IntValue x;     DecimalValue y] -> Ok(DecimalValue(decimal x / y))
        | [DecimalValue x; IntValue y]     -> Ok(DecimalValue(x / decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(DecimalValue(x / y))
        | [_; _] -> typeErr2 "/" "int/decimal" "int/decimal"
        | _ -> arityErr "/" 2 args

    let klGreaterThan _ args =
        match args with
        | [IntValue x;     IntValue y]     -> Ok(BoolValue(x > y))
        | [IntValue x;     DecimalValue y] -> Ok(BoolValue(decimal x > y))
        | [DecimalValue x; IntValue y]     -> Ok(BoolValue(x > decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(BoolValue(x > y))
        | [_; _] -> typeErr2 ">" "int/decimal" "int/decimal"
        | _ -> arityErr ">" 2 args

    let klLessThan _ args =
        match args with
        | [IntValue x;     IntValue y]     -> Ok(BoolValue(x < y))
        | [IntValue x;     DecimalValue y] -> Ok(BoolValue(decimal x < y))
        | [DecimalValue x; IntValue y]     -> Ok(BoolValue(x < decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(BoolValue(x < y))
        | [_; _] -> typeErr2 "<" "int/decimal" "int/decimal"
        | _ -> arityErr "<" 2 args

    let klGreaterThanEqual _ args =
        match args with
        | [IntValue x;     IntValue y]     -> Ok(BoolValue(x >= y))
        | [IntValue x;     DecimalValue y] -> Ok(BoolValue(decimal x >= y))
        | [DecimalValue x; IntValue y]     -> Ok(BoolValue(x >= decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(BoolValue(x >= y))
        | [_; _] -> typeErr2 ">=" "int/decimal" "int/decimal"
        | _ -> arityErr ">=" 2 args

    let klLessThanEqual _ args =
        match args with
        | [IntValue x;     IntValue y]     -> Ok(BoolValue(x <= y))
        | [IntValue x;     DecimalValue y] -> Ok(BoolValue(decimal x <= y))
        | [DecimalValue x; IntValue y]     -> Ok(BoolValue(x <= decimal y))
        | [DecimalValue x; DecimalValue y] -> Ok(BoolValue(x <= y))
        | [_; _] -> typeErr2 "<=" "int/decimal" "int/decimal"
        | _ -> arityErr "<=" 2 args

    let klIsNumber _ args =
        match args with
        | [IntValue _] | [DecimalValue _] -> Values.truer
        | [_] -> Values.falser
        | _ -> arityErr "number?" 1 args

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

    let klPrint _ args =
        match args with
        | [x] -> Console.Write(Values.toStr x)
                 Ok EmptyValue
        | _ -> arityErr "print" 1 args

    let klFillVector _ args =
        match args with
        | [VectorValue array as vector; IntValue stop; IntValue start; fillValue] ->
            Array.fill array start (stop - start) fillValue
            Ok vector
        | [_; _; _; _] -> typeErr4 "shen.fillvector" "vector" "int" "int" "value"
        | _ -> arityErr "shen.fillvector" 4 args

    let rec klElement globals args =
        match args with
        | [_; EmptyValue] -> Values.falser
        | [key; ConsValue(head, _)] when Values.eq key head -> Values.truer
        | [key; ConsValue(_, tail)] -> klElement globals [key; tail]
        | [_; _] -> typeErr2 "element?" "value" "cons"
        | _ -> arityErr "element?" 2 args
