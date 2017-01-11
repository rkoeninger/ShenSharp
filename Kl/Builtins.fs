namespace Kl

open Extensions
open System
open System.Diagnostics
open System.IO
open Kl.Values
open Kl.Evaluator

module Builtins =

    let private arityErr name expected (args: Value list) =
        err(sprintf "%s expected %i arguments, but given %i" name expected args.Length)

    let private typeErr name (types: string list) =
        if types.IsEmpty
            then err(sprintf "%s expected no arguments" name)
            else err(sprintf "%s expected arguments of type(s): %s" name (String.Join(" ", types)))

    let klIntern _ args =
        match args with
        | [Str s] -> Sym s
        | [_] -> typeErr "intern" ["string"]
        | _ -> arityErr "intern" 1 args

    let klStringPos _ args =
        match args with
        | [Str s; Int index] ->
            if index >= 0 && index < s.Length
                then Str(string s.[index])
                else err(sprintf "Index %i out of bounds for string of length %i" index s.Length)
        | [_; _] -> typeErr "pos" ["string"; "int"]
        | _ -> arityErr "pos" 2 args

    let klStringTail _ args =
        match args with
        | [Str s] ->
            if (s.Length > 0)
                then Str(s.Substring 1)
                else err "strtl expects a non-empty string"
        | [_] -> typeErr "strtl" ["string"]
        | _ -> arityErr "strtl" 1 args

    let klStringConcat _ args =
        match args with
        | [Str x; Str y] -> Str(x + y)
        | [_; _] -> typeErr "cn" ["string"; "string"]
        | _ -> arityErr "cn" 2 args

    let klToString _ args =
        match args with
        | [x] -> Str(string x)
        | _ -> arityErr "str" 1 args

    let klIsString _ args =
        match args with
        | [Str _] -> truev
        | [_] -> falsev
        | _ -> arityErr "string?" 1 args

    let klIntToString _ args =
        match args with
        | [Int n] -> Str(string(char n))
        | [_] -> typeErr "n->string" ["int"]
        | _ -> arityErr "n->string" 1 args

    let klStringToInt _ args =
        match args with
        | [Str s] -> Int(int s.[0])
        | [_] -> typeErr "string->n" ["string"]
        | _ -> arityErr "string->n" 1 args

    let klSet globals args =
        match args with
        | [Sym s; x] ->
            globals.Symbols.[s] <- x
            x
        | [_; _] -> typeErr "set" ["symbol"; "value"]
        | _ -> arityErr "set" 2 args

    let klValue globals args =
        match args with
        | [Sym s] ->
            match globals.Symbols.GetMaybe s with
            | Some v -> v
            | None -> err(sprintf "Symbol \"%s\" is undefined" s)
        | [_] -> typeErr "value" ["symbol"]
        | _ -> arityErr "value" 1 args

    let klSimpleError _ args =
        match args with
        | [Str s] -> err s
        | [_] -> typeErr "simple-error" ["string"]
        | _ -> arityErr "simple-error" 1 args

    let klErrorToString _ args =
        match args with
        | [Err s] -> Str s
        | [_] -> typeErr "error-to-string" ["error"]
        | _ -> arityErr "error-to-string" 1 args

    let klNewCons _ args =
        match args with
        | [x; y] -> Cons(x, y)
        | _ -> arityErr "cons" 2 args

    let klHead _ args =
        match args with
        | [Cons (x, _)] -> x
        | [_] -> typeErr "hd" ["cons"]
        | _ -> arityErr "hd" 1 args

    let klTail _ args =
        match args with
        | [Cons (_, y)] -> y
        | [_] -> typeErr "tl" ["cons"]
        | _ -> arityErr "tl" 1 args

    let klIsCons _ args =
        match args with
        | [Cons _] -> truev
        | [_] -> falsev
        | _ -> arityErr "cons?" 1 args

    let klEquals _ args =
        match args with
        | [x; y] -> Bool(x = y)
        | _ -> arityErr "=" 2 args

    let klEval globals args =
        match args with
        | [v] -> rootEval globals v
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
        | [_] -> typeErr "absvector" ["int"]
        | _ -> arityErr "absvector" 1 args

    let klReadVector _ args =
        match args with
        | [Vec vector; Int index] ->
            if index >= 0 && index < vector.Length
                then vector.[index]
                else err(sprintf "Index %i out of bounds for vector of length %i" index vector.Length)
        | [_; _] -> typeErr "<-address" ["vector"; "int"]
        | _ -> arityErr "<-address" 2 args

    let klWriteVector _ args =
        match args with
        | [Vec vector as vv; Int index; value] ->
            if index >= 0 && index < vector.Length
                then vector.[index] <- value
                     vv
                else err(sprintf "Index %i out of bounds for vector of length %i" index vector.Length)
        | [_; _; _] -> typeErr "address->" ["vector"; "int"; "value"]
        | _ -> arityErr "address->" 3 args

    let klIsVector _ args =
        match args with
        | [Vec _] -> truev
        | [_] -> falsev
        | _ -> arityErr "absvector?" 1 args

    let klWriteByte _ args =
        match args with
        | [Int i; OutStream stream] ->
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.Write(b)
                     Int(int b)
                else err(sprintf "int value %i is exceeds the range of a byte" i)
        | [_; _] -> typeErr "write-byte" ["int"; "out-stream"]
        | _ -> arityErr "write-byte" 2 args

    let klReadByte _ args =
        match args with
        | [InStream stream] -> Int(stream.Read())
        | [_] -> typeErr "read-byte" ["in-stream"]
        | _ -> arityErr "read-byte" 1 args

    let klOpen _ args =
        match args with
        | [Str path; Sym "in"] ->
            try let stream = File.OpenRead(path)
                InStream {
                    Name = "File: " + path
                    Read = stream.ReadByte
                    Close = stream.Close
                }
            with e -> err e.Message
        | [Str path; Sym "out"] ->
            try let stream = File.OpenWrite(path)
                OutStream {
                    Name = "File: " + path
                    Write = stream.WriteByte
                    Close = stream.Close
                }
            with e -> err e.Message
        | [Str _; Sym s] ->
            err(sprintf "open expects symbol 'in or 'out as 2nd argument, not '%s" s)
        | [_; _] -> typeErr "open" ["string"; "symbol"]
        | _ -> arityErr "open" 2 args

    let klClose _ args =
        match args with
        | [InStream stream] ->
            stream.Close()
            Empty
        | [OutStream stream] ->
            stream.Close()
            Empty
        | [_] -> typeErr "close" ["stream"]
        | _ -> arityErr "close" 1 args

    let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private startTime = DateTime.UtcNow
    let private stopwatch = Stopwatch.StartNew()

    /// <remarks>
    /// All returned values are in milliseconds
    /// </remarks>
    let klGetTime _ args =
        match args with
        | [Sym "run"] | [Sym "real"] -> Int(int (stopwatch.ElapsedTicks / 10000L))
        | [Sym "unix"] -> Int(int (DateTime.UtcNow - epoch).TotalSeconds)
        | [Sym s] -> err(sprintf "get-time expects symbols 'run or 'unix' as argument, not %s" s)
        | [_] -> typeErr "get-time" ["symbol"]
        | _ -> arityErr "get-time" 1 args

    let klAdd _ args =
        match args with
        | [Int x; Int y] -> Int(x + y)
        | [Int x; Dec y] -> Dec(decimal x + y)
        | [Dec x; Int y] -> Dec(x + decimal y)
        | [Dec x; Dec y] -> Dec(x + y)
        | [_; _] -> typeErr "+" ["int/decimal"; "int/decimal"]
        | _ -> arityErr "+" 2 args

    let klSubtract _ args =
        match args with
        | [Int x; Int y] -> Int(x - y)
        | [Int x; Dec y] -> Dec(decimal x - y)
        | [Dec x; Int y] -> Dec(x - decimal y)
        | [Dec x; Dec y] -> Dec(x - y)
        | [_; _] -> typeErr "-" ["int/decimal"; "int/decimal"]
        | _ -> arityErr "-" 2 args

    let klMultiply _ args =
        match args with
        | [Int x; Int y] -> Int(x * y)
        | [Int x; Dec y] -> Dec(decimal x * y)
        | [Dec x; Int y] -> Dec(x * decimal y)
        | [Dec x; Dec y] -> Dec(x * y)
        | [_; _] -> typeErr "*" ["int/decimal"; "int/decimal"]
        | _ -> arityErr "*" 2 args

    let klDivide _ args =
        match args with
        | [_; Int 0] -> err "Division by zero"
        | [_; Dec 0m] -> err "Division by zero"
        | [Int x; Int y] -> Dec(decimal x / decimal y)
        | [Int x; Dec y] -> Dec(decimal x / y)
        | [Dec x; Int y] -> Dec(x / decimal y)
        | [Dec x; Dec y] -> Dec(x / y)
        | [_; _] -> typeErr "/" ["int/decimal"; "int/decimal"]
        | _ -> arityErr "/" 2 args

    let klGreaterThan _ args =
        match args with
        | [Int x; Int y] -> Bool(x > y)
        | [Int x; Dec y] -> Bool(decimal x > y)
        | [Dec x; Int y] -> Bool(x > decimal y)
        | [Dec x; Dec y] -> Bool(x > y)
        | [_; _] -> typeErr ">" ["int/decimal"; "int/decimal"]
        | _ -> arityErr ">" 2 args

    let klLessThan _ args =
        match args with
        | [Int x; Int y] -> Bool(x < y)
        | [Int x; Dec y] -> Bool(decimal x < y)
        | [Dec x; Int y] -> Bool(x < decimal y)
        | [Dec x; Dec y] -> Bool(x < y)
        | [_; _] -> typeErr "<" ["int/decimal"; "int/decimal"]
        | _ -> arityErr "<" 2 args

    let klGreaterThanEqual _ args =
        match args with
        | [Int x; Int y] -> Bool(x >= y)
        | [Int x; Dec y] -> Bool(decimal x >= y)
        | [Dec x; Int y] -> Bool(x >= decimal y)
        | [Dec x; Dec y] -> Bool(x >= y)
        | [_; _] -> typeErr ">=" ["int/decimal"; "int/decimal"]
        | _ -> arityErr ">=" 2 args

    let klLessThanEqual _ args =
        match args with
        | [Int x; Int y] -> Bool(x <= y)
        | [Int x; Dec y] -> Bool(decimal x <= y)
        | [Dec x; Int y] -> Bool(x <= decimal y)
        | [Dec x; Dec y] -> Bool(x <= y)
        | [_; _] -> typeErr "<=" ["int/decimal"; "int/decimal"]
        | _ -> arityErr "<=" 2 args

    let klIsNumber _ args =
        match args with
        | [Int _] | [Dec _] -> truev
        | [_] -> falsev
        | _ -> arityErr "number?" 1 args

    let stinput =
        let consoleIn = new ConsoleIn(Console.OpenStandardInput())
        InStream {
            Name = "Console"
            Read = consoleIn.Read
            Close = consoleIn.Close
        }

    let stoutput =
        let consoleOutStream = Console.OpenStandardOutput()
        OutStream {
            Name = "Console"
            Write = consoleOutStream.WriteByte
            Close = consoleOutStream.Close
        }

    let klIsBoolean _ args =
        match args with
        | [Bool _] -> truev
        | [_] -> falsev
        | _ -> arityErr "boolean?" 1 args

    let klIsSymbol _ args =
        match args with
        | [Sym _] -> truev
        | [_] -> falsev
        | _ -> arityErr "symbol?" 1 args

    let klPrint _ args =
        match args with
        | [x] -> Console.Write(string x)
                 Empty
        | _ -> arityErr "print" 1 args

    let klFillVector _ args =
        match args with
        | [Vec array as vector; Int start; Int stop; fillValue] ->
            Array.fill array start (stop - start) fillValue
            vector
        | [_; _; _; _] -> typeErr "shen.fillvector" ["vector"; "int"; "int"; "value"]
        | _ -> arityErr "shen.fillvector" 4 args

    let rec klElement globals args =
        match args with
        | [_; Empty] -> falsev
        | [key; Cons(head, _)] when key = head -> truev
        | [key; Cons(_, tail)] -> klElement globals [key; tail]
        | [_; _] -> typeErr "element?" ["value"; "list"]
        | _ -> arityErr "element?" 2 args

    let klReverse _ args =
        let rec reverseHelp v r =
            match v with
            | Cons(head, tail) -> reverseHelp tail (Cons(head, r))
            | x -> x

        match args with
        | [Empty] -> Empty
        | [Cons _ as v] -> reverseHelp v Empty
        | [_] -> typeErr "reverse" ["list"]
        | _ -> arityErr "reverse" 1 args

    let klModulus _ args =
        match args with
        | [Int x; Int y] -> Int(x % y)
        | [Int x; Dec y] -> Dec(decimal x % y)
        | [Dec x; Int y] -> Dec(x % decimal y)
        | [Dec x; Dec y] -> Dec(x % y)
        | [_; _] -> typeErr "shen.mod" ["int/decimal"; "int/decimal"]
        | _ -> arityErr "shen.mod" 2 args

    let klIsAlpha _ args =
        match args with
        | [Str s] ->
            if s.Length <> 1
                then err "String must be 1 character long"
                else Bool(('A' <= s.[0] && s.[0] <= 'Z') || ('a' <= s.[0] && 'z' <= s.[0]))
        | [_] -> typeErr "shen.alpha?" ["string"]
        | _ -> arityErr "shen.alpha?" 1 args

    let klIsDigit _ args =
        match args with
        | [Str s] ->
            if s.Length <> 1
                then err "String must be 1 character long"
                else Bool('0' <= s.[0] && s.[0] <= '9')
        | [_] -> typeErr "shen.digit?" ["string"]
        | _ -> arityErr "shen.digit?" 1 args

    let rec klAppend globals args =
        match args with
        | [Empty; Empty] -> Empty
        | [Empty; Cons _ as cons] -> cons
        | [Cons _ as cons; Empty] -> cons
        | [Cons(a, b); Cons _ as cons] -> Cons(a, klAppend globals [b; cons])
        | [_; _] -> typeErr "append" ["list"; "list"]
        | _ -> arityErr "append" 2 args

    let klCd (globals: Globals) args =
        match args with
        | [Str s] ->
            let path = Str(Path.Combine(vstr(globals.Symbols.["*home-directory*"]), s))
            globals.Symbols.["*home-directory*"] <- path
            path
        | [_] -> typeErr "cd" ["string"]
        | _ -> arityErr "cd" 1 args

    let klHash _ args =
        match args with
        | [Str s; Int d] ->
            match s.GetHashCode() % d with
            | 0 -> Int 1
            | h -> Int h
        | [_; _] -> typeErr "hash" ["string"; "int"]
        | _ -> arityErr "hash" 2 args
