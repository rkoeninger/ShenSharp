namespace Kl

open Extensions
open System
open System.Diagnostics
open System.IO

module Builtins =

    let inline invalidArgs () = failwith "Wrong number or type of arguments"

    let private arityErr = Values.arityErr

    let private typeErr1 name type1 =
        Values.err(sprintf "%s expects a(n) %s" name type1)

    let private typeErr2 name type1 type2 =
        Values.err(sprintf "%s expects a(n) %s and a(n) %s" name type1 type2)

    let private typeErr3 name type1 type2 type3 =
        Values.err(sprintf "%s expects a(n) %s, a(n) %s and a(n) %s" name type1 type2 type3)

    let private typeErr4 name type1 type2 type3 type4 =
        Values.err(sprintf "%s expects a(n) %s, a(n) %s, a(n) %s and a(n) %s" name type1 type2 type3 type4)

    let klIntern _ args =
        match args with
        | [Str s] -> Sym s
        | [_] -> typeErr1 "intern" "string"
        | _ -> arityErr "intern" 1 args

    let klStringPos _ args =
        match args with
        | [Str s; Int index] ->
            if index >= 0 && index < s.Length
                then Str(string s.[index])
                else Values.err(sprintf "Index %i out of bounds for string of length %i" index s.Length)
        | [_; _] -> typeErr2 "pos" "string" "int"
        | _ -> arityErr "pos" 2 args

    let klStringTail _ args =
        match args with
        | [Str s] ->
            if (s.Length > 0)
                then Str(s.Substring 1)
                else Values.err "strtl expects a non-empty string"
        | [_] -> typeErr1 "strtl" "string"
        | _ -> arityErr "strtl" 1 args

    let klStringConcat _ args =
        match args with
        | [Str x; Str y] -> Str(x + y)
        | [_; _] -> typeErr2 "cn" "string" "string"
        | _ -> arityErr "cn" 2 args

    let klToString _ args =
        match args with
        | [x] -> Str(Values.toStr x)
        | _ -> arityErr "str" 1 args

    let klIsString _ args =
        match args with
        | [Str _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> arityErr "string?" 1 args

    let klIntToString _ args =
        match args with
        | [Int n] -> Str(string(char(int n)))
        | [_] -> typeErr1 "n->string" "int"
        | _ -> arityErr "n->string" 1 args

    let klStringToInt _ args =
        match args with
        | [Str s] -> Int(int s.[0])
        | [_] -> typeErr1 "string->n" "string"
        | _ -> arityErr "string->n" 1 args

    let klSet globals args =
        match args with
        | [Sym s; x] ->
            globals.Symbols.[s] <- x
            x
        | [_; _] -> typeErr2 "set" "symbol" "value"
        | _ -> arityErr "set" 2 args

    let klValue globals args =
        match args with
        | [Sym s] ->
            match globals.Symbols.GetMaybe(s) with
            | Some v -> v
            | None -> Values.err(sprintf "Symbol \"%s\" is undefined" s)
        | [_] -> typeErr1 "value" "symbol"
        | _ -> arityErr "value" 1 args

    let klSimpleError _ args =
        match args with
        | [Str s] -> Values.err s
        | [_] -> typeErr1 "simple-error" "string"
        | _ -> arityErr "simple-error" 1 args

    let klErrorToString _ args =
        match args with
        | [Err s] -> Str s
        | [_] -> typeErr1 "error-to-string" "error"
        | _ -> arityErr "error-to-string" 1 args

    let klNewCons _ args =
        match args with
        | [x; y] -> Cons(x, y)
        | _ -> arityErr "cons" 2 args

    let klHead _ args =
        match args with
        | [Cons (x, _)] -> x
        | [_] -> typeErr1 "hd" "cons"
        | _ -> arityErr "hd" 1 args

    let klTail _ args =
        match args with
        | [Cons (_, y)] -> y
        | [_] -> typeErr1 "tl" "cons"
        | _ -> arityErr "tl" 1 args

    let klIsCons _ args =
        match args with
        | [Cons _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> arityErr "cons?" 1 args

    let klEquals _ args =
        match args with
        | [x; y] -> Bool(Values.eq x y)
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
        | [x; _] -> x
        | _ -> arityErr "type" 2 args

    let klNewVector _ args =
        match args with
        | [Int length] ->
            let failSymbol = Sym "fail!"
            Vec(Array.create length failSymbol)
        | [_] -> typeErr1 "absvector" "int"
        | _ -> arityErr "absvector" 1 args

    let klReadVector _ args =
        match args with
        | [Vec vector; Int index] ->
            if index >= 0 && index < vector.Length
                then vector.[index]
                else Values.err(sprintf "Index %i out of bounds for vector of length %i" index vector.Length)
        | [_; _] -> typeErr2 "<-address" "vector" "int"
        | _ -> arityErr "<-address" 2 args

    let klWriteVector _ args =
        match args with
        | [Vec vector as vv; Int index; value] ->
            if index >= 0 && index < vector.Length
                then vector.[index] <- value
                     vv
                else Values.err(sprintf "Index %i out of bounds for vector of length %i" index vector.Length)
        | [_; _; _] -> typeErr3 "address->" "vector" "int" "value"
        | _ -> arityErr "address->" 3 args

    let klIsVector _ args =
        match args with
        | [Vec _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> arityErr "absvector?" 1 args

    let klWriteByte _ args =
        match args with
        | [Int i; OutStream stream] ->
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.Write(b)
                     Int(int b)
                else Values.err(sprintf "int value %i is exceeds the range of a byte" i)
        | [_; _] -> typeErr2 "write-byte" "int" "out-stream"
        | _ -> arityErr "write-byte" 2 args

    let klReadByte _ args =
        match args with
        | [InStream stream] -> Int(stream.Read())
        | [_] -> typeErr1 "read-byte" "in-stream"
        | _ -> arityErr "read-byte" 1 args

    let klOpen _ args =
        match args with
        | [Str path; Sym "in"] ->
            try let stream = File.OpenRead(path)
                InStream{Read = stream.ReadByte; Close = stream.Close}
            with e -> Values.err e.Message
        | [Str path; Sym "out"] ->
            try let stream = File.OpenWrite(path)
                OutStream{Write = stream.WriteByte; Close = stream.Close}
            with e -> Values.err e.Message
        | [Str _; Sym s] ->
            Values.err(sprintf "open expects symbol 'in or 'out as 2nd argument, not '%s" s)
        | [_; _] -> typeErr2 "open" "string" "symbol"
        | _ -> arityErr "open" 2 args

    let klClose _ args =
        match args with
        | [InStream stream] ->
            stream.Close()
            Empty
        | [OutStream stream] ->
            stream.Close()
            Empty
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
        | [Sym "run"] -> Int(int (stopwatch.ElapsedTicks / 10000L))
        | [Sym "unix"] -> Int(int (DateTime.UtcNow - epoch).TotalSeconds)
        | [Sym s] -> Values.err(sprintf "get-time expects symbols 'run or 'unix' as argument, not %s" s)
        | [_] -> typeErr1 "get-time" "symbol"
        | _ -> arityErr "get-time" 1 args

    let klAdd _ args =
        match args with
        | [Int x; Int y] -> Int(x + y)
        | [Int x; Dec y] -> Dec(decimal x + y)
        | [Dec x; Int y] -> Dec(x + decimal y)
        | [Dec x; Dec y] -> Dec(x + y)
        | [_; _] -> typeErr2 "+" "int/decimal" "int/decimal"
        | _ -> arityErr "+" 2 args

    let klSubtract _ args =
        match args with
        | [Int x; Int y] -> Int(x - y)
        | [Int x; Dec y] -> Dec(decimal x - y)
        | [Dec x; Int y] -> Dec(x - decimal y)
        | [Dec x; Dec y] -> Dec(x - y)
        | [_; _] -> typeErr2 "-" "int/decimal" "int/decimal"
        | _ -> arityErr "-" 2 args

    let klMultiply _ args =
        match args with
        | [Int x; Int y] -> Int(x * y)
        | [Int x; Dec y] -> Dec(decimal x * y)
        | [Dec x; Int y] -> Dec(x * decimal y)
        | [Dec x; Dec y] -> Dec(x * y)
        | [_; _] -> typeErr2 "*" "int/decimal" "int/decimal"
        | _ -> arityErr "*" 2 args

    let klDivide _ args =
        match args with
        | [_; Int 0] -> Values.err "Division by zero"
        | [_; Dec 0m] -> Values.err "Division by zero"
        | [Int x; Int y] -> Dec(decimal x / decimal y)
        | [Int x; Dec y] -> Dec(decimal x / y)
        | [Dec x; Int y] -> Dec(x / decimal y)
        | [Dec x; Dec y] -> Dec(x / y)
        | [_; _] -> typeErr2 "/" "int/decimal" "int/decimal"
        | _ -> arityErr "/" 2 args

    let klGreaterThan _ args =
        match args with
        | [Int x; Int y] -> Bool(x > y)
        | [Int x; Dec y] -> Bool(decimal x > y)
        | [Dec x; Int y] -> Bool(x > decimal y)
        | [Dec x; Dec y] -> Bool(x > y)
        | [_; _] -> typeErr2 ">" "int/decimal" "int/decimal"
        | _ -> arityErr ">" 2 args

    let klLessThan _ args =
        match args with
        | [Int x; Int y] -> Bool(x < y)
        | [Int x; Dec y] -> Bool(decimal x < y)
        | [Dec x; Int y] -> Bool(x < decimal y)
        | [Dec x; Dec y] -> Bool(x < y)
        | [_; _] -> typeErr2 "<" "int/decimal" "int/decimal"
        | _ -> arityErr "<" 2 args

    let klGreaterThanEqual _ args =
        match args with
        | [Int x; Int y] -> Bool(x >= y)
        | [Int x; Dec y] -> Bool(decimal x >= y)
        | [Dec x; Int y] -> Bool(x >= decimal y)
        | [Dec x; Dec y] -> Bool(x >= y)
        | [_; _] -> typeErr2 ">=" "int/decimal" "int/decimal"
        | _ -> arityErr ">=" 2 args

    let klLessThanEqual _ args =
        match args with
        | [Int x; Int y] -> Bool(x <= y)
        | [Int x; Dec y] -> Bool(decimal x <= y)
        | [Dec x; Int y] -> Bool(x <= decimal y)
        | [Dec x; Dec y] -> Bool(x <= y)
        | [_; _] -> typeErr2 "<=" "int/decimal" "int/decimal"
        | _ -> arityErr "<=" 2 args

    let klIsNumber _ args =
        match args with
        | [Int _] | [Dec _] -> Values.truev
        | [_] -> Values.falsev
        | _ -> arityErr "number?" 1 args

    let stinput =
        let consoleIn = new ConsoleIn(Console.OpenStandardInput())
        InStream {Read = consoleIn.Read; Close = consoleIn.Close}

    let stoutput =
        let consoleOutStream = Console.OpenStandardOutput()
        OutStream {Write = consoleOutStream.WriteByte; Close = consoleOutStream.Close}

    let klPrint _ args =
        match args with
        | [x] -> Console.Write(Values.toStr x)
                 Empty
        | _ -> arityErr "print" 1 args

    let klFillVector _ args =
        match args with
        | [Vec array as vector; Int stop; Int start; fillValue] ->
            Array.fill array start (stop - start) fillValue
            vector
        | [_; _; _; _] -> typeErr4 "shen.fillvector" "vector" "int" "int" "value"
        | _ -> arityErr "shen.fillvector" 4 args

    let rec klElement globals args =
        match args with
        | [_; Empty] -> Values.falsev
        | [key; Cons(head, _)] when Values.eq key head -> Values.truev
        | [key; Cons(_, tail)] -> klElement globals [key; tail]
        | [_; _] -> typeErr2 "element?" "value" "cons"
        | _ -> arityErr "element?" 2 args
