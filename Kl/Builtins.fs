namespace Kl

open Extensions
open System
open System.Diagnostics
open System.IO
open Kl.Values
open Kl.Evaluator

module Builtins =

    let private argsErr name (types: string list) (args: Value list) =
        if types.Length = args.Length then
            errf "%s expected %A arguments, given %A" name types.Length args.Length
        else
            match types with
            | [] -> errf "%s expected no arguments" name
            | _  -> errf "%s expected arguments of type(s): %s" name (String.Join(", ", types))

    let klIntern _ = function
        | [Str s] -> Sym s
        | args -> argsErr "intern" ["string"] args

    let klStringPos _ = function
        | [Str s; Int index] ->
            if index >= 0 && index < s.Length
                then Str(string s.[index])
                else errf "Index %i out of bounds for string of length %i" index s.Length
        | args -> argsErr "pos" ["string"; "integer"] args

    let klStringTail _ = function
        | [Str s] ->
            if (s.Length > 0)
                then Str(s.Substring 1)
                else err "strtl expects a non-empty string"
        | args -> argsErr "tlstr" ["string"] args

    let klStringConcat _ = function
        | [Str x; Str y] -> Str(x + y)
        | args -> argsErr "cn" ["string"; "string"] args

    let klToString _ = function
        | [x] -> Str(string x)
        | args -> argsErr "str" ["value"] args

    let klIsString _ = function
        | [Str _] -> truev
        | [_] -> falsev
        | args -> argsErr "string?" ["value"] args

    let klIntToString _ = function
        | [Int n] -> Str(string(char n))
        | args -> argsErr "n->string" ["integer"] args

    let klStringToInt _ = function
        | [Str s] -> Int(int s.[0])
        | args -> argsErr "string->n" ["string"] args

    let klSet globals = function
        | [Sym s; x] ->
            globals.Symbols.[s] <- x
            x
        | args -> argsErr "set" ["symbol"; "value"] args

    let klValue globals = function
        | [Sym s] ->
            match globals.Symbols.GetMaybe s with
            | Some x -> x
            | None -> errf "Symbol \"%s\" is undefined" s
        | args -> argsErr "value" ["symbol"] args

    let klSimpleError _ = function
        | [Str s] -> err s
        | args -> argsErr "simple-error" ["string"] args

    let klErrorToString _ = function
        | [Err s] -> Str s
        | args -> argsErr "error-to-string" ["error"] args

    let klNewCons _ = function
        | [x; y] -> Cons(x, y)
        | args -> argsErr "cons" ["value"; "value"] args

    let klHead _ = function
        | [Cons (x, _)] -> x
        | args -> argsErr "hd" ["cons"] args

    let klTail _ = function
        | [Cons (_, y)] -> y
        | args -> argsErr "tl" ["cons"] args

    let klIsCons _ = function
        | [Cons _] -> truev
        | [_] -> falsev
        | args -> argsErr "cons?" ["value"] args

    let klEquals _ = function
        | [x; y] -> boolv(x = y)
        | args -> argsErr "=" ["value"; "value"] args

    let klEval globals = function
        | [x] -> eval globals x
        | args -> argsErr "eval-kl" ["value"] args

    let klType _ = function
        | [x; _] -> x
        | args -> argsErr "type" ["symbol"; "value"] args

    let klNewVector _ = function
        | [Int length] -> Vec(Array.create length Empty)
        | args -> argsErr "absvector" ["integer"] args

    let klReadVector _ = function
        | [Vec vector; Int index] ->
            if index >= 0 && index < vector.Length
                then vector.[index]
                else errf "Index %i out of bounds for vector of length %i" index vector.Length
        | args -> argsErr "<-address" ["vector"; "integer"] args

    let klWriteVector _ = function
        | [Vec array as vector; Int index; value] ->
            if index >= 0 && index < array.Length
                then array.[index] <- value
                     vector
                else errf "Index %i out of bounds for vector of length %i" index array.Length
        | args -> argsErr "address->" ["vector"; "integer"; "value"] args

    let klIsVector _ = function
        | [Vec _] -> truev
        | [_] -> falsev
        | args -> argsErr "absvector?" ["value"] args

    let klWriteByte _ = function
        | [Int i; OutStream stream] ->
            if 0 <= i && i <= 255
                then let b = byte i
                     stream.Write(b)
                     Int(int b)
                else errf "integer value %i is exceeds the range of a byte" i
        | args -> argsErr "write-byte" ["integer"; "out-stream"] args

    let klReadByte _ = function
        | [InStream stream] -> Int(stream.Read())
        | args -> argsErr "read-byte" ["in-stream"] args

    let klOpen _ = function
        | [Str path; Sym "in"] ->
            let stream =
                try File.OpenRead path
                with e -> err e.Message
            InStream {
                Name = "File: " + path
                Read = stream.ReadByte
                Close = stream.Close
            }
        | [Str path; Sym "out"] ->
            let stream =
                try File.OpenWrite path
                with e -> err e.Message
            OutStream {
                Name = "File: " + path
                Write = stream.WriteByte
                Close = stream.Close
            }
        | [Str _; Sym s] -> errf "open expects symbol 'in or 'out as 2nd argument, not '%s" s
        | args -> argsErr "open" ["string"; "symbol"] args

    let klClose _ = function
        | [InStream stream] ->
            stream.Close()
            Empty
        | [OutStream stream] ->
            stream.Close()
            Empty
        | args -> argsErr "close" ["stream"] args

    let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private startTime = DateTime.UtcNow
    let private stopwatch = Stopwatch.StartNew()

    /// <remarks>
    /// All returned values are in milliseconds
    /// </remarks>
    let klGetTime _ = function
        | [Sym "run"] | [Sym "real"] -> Int(int (stopwatch.ElapsedTicks / 10000L))
        | [Sym "unix"] -> Int(int (DateTime.UtcNow - epoch).TotalSeconds)
        | [Sym s] -> errf "get-time expects symbols 'run or 'unix' as argument, not %s" s
        | args -> argsErr "get-time" ["symbol"] args

    let klAdd _ = function
        | [Int x; Int y] -> Int(x + y)
        | [Int x; Dec y] -> Dec(decimal x + y)
        | [Dec x; Int y] -> Dec(x + decimal y)
        | [Dec x; Dec y] -> Dec(x + y)
        | args -> argsErr "+" ["number"; "number"] args

    let klSubtract _ = function
        | [Int x; Int y] -> Int(x - y)
        | [Int x; Dec y] -> Dec(decimal x - y)
        | [Dec x; Int y] -> Dec(x - decimal y)
        | [Dec x; Dec y] -> Dec(x - y)
        | args -> argsErr "-" ["number"; "number"] args

    let klMultiply _ = function
        | [Int x; Int y] -> Int(x * y)
        | [Int x; Dec y] -> Dec(decimal x * y)
        | [Dec x; Int y] -> Dec(x * decimal y)
        | [Dec x; Dec y] -> Dec(x * y)
        | args -> argsErr "*" ["number"; "number"] args

    let klDivide _ = function
        | [_; Int 0] -> err "Division by zero"
        | [_; Dec 0m] -> err "Division by zero"
        | [Int x; Int y] -> Dec(decimal x / decimal y)
        | [Int x; Dec y] -> Dec(decimal x / y)
        | [Dec x; Int y] -> Dec(x / decimal y)
        | [Dec x; Dec y] -> Dec(x / y)
        | args -> argsErr "/" ["number"; "number"] args

    let klGreaterThan _ = function
        | [Int x; Int y] -> boolv(x > y)
        | [Int x; Dec y] -> boolv(decimal x > y)
        | [Dec x; Int y] -> boolv(x > decimal y)
        | [Dec x; Dec y] -> boolv(x > y)
        | args -> argsErr ">" ["number"; "number"] args

    let klLessThan _ = function
        | [Int x; Int y] -> boolv(x < y)
        | [Int x; Dec y] -> boolv(decimal x < y)
        | [Dec x; Int y] -> boolv(x < decimal y)
        | [Dec x; Dec y] -> boolv(x < y)
        | args -> argsErr "<" ["number"; "number"] args

    let klGreaterThanEqual _ = function
        | [Int x; Int y] -> boolv(x >= y)
        | [Int x; Dec y] -> boolv(decimal x >= y)
        | [Dec x; Int y] -> boolv(x >= decimal y)
        | [Dec x; Dec y] -> boolv(x >= y)
        | args -> argsErr ">=" ["number"; "number"] args

    let klLessThanEqual _ = function
        | [Int x; Int y] -> boolv(x <= y)
        | [Int x; Dec y] -> boolv(decimal x <= y)
        | [Dec x; Int y] -> boolv(x <= decimal y)
        | [Dec x; Dec y] -> boolv(x <= y)
        | args -> argsErr "<=" ["number"; "number"] args

    let klIsNumber _ = function
        | [Int _] | [Dec _] -> truev
        | [_] -> falsev
        | args -> argsErr "number?" ["value"] args

    let stinput =
        InStream {
            Name = "Console"
            Read = (new ConsoleReader()).ReadByte
            Close = fun () -> ()
        }

    let stoutput =
        OutStream {
            Name = "Console"
            Write = (new ConsoleWriter()).WriteByte
            Close = fun () -> ()
        }

    let klIsSymbol _ = function
        | [Sym _] -> truev
        | [_] -> falsev
        | args -> argsErr "symbol?" ["value"] args

    let klFillVector _ = function
        | [Vec array as vector; Int start; Int stop; fillValue] ->
            Array.fill array start (stop - start) fillValue
            vector
        | args -> argsErr "shen.fillvector" ["vector"; "integer"; "integer"; "value"] args

    let klModulus _ = function
        | [Int x; Int y] -> Int(x % y)
        | [Int x; Dec y] -> Dec(decimal x % y)
        | [Dec x; Int y] -> Dec(x % decimal y)
        | [Dec x; Dec y] -> Dec(x % y)
        | args -> argsErr "shen.mod" ["number"; "number"] args

    let klIsAlpha _ = function
        | [Str s] ->
            if s.Length <> 1
                then err "String must be 1 character long"
                else boolv(('A' <= s.[0] && s.[0] <= 'Z') || ('a' <= s.[0] && 'z' <= s.[0]))
        | args -> argsErr "shen.alpha?" ["string"] args

    let klIsDigit _ = function
        | [Str s] ->
            if s.Length <> 1
                then err "String must be 1 character long"
                else boolv('0' <= s.[0] && s.[0] <= '9')
        | args -> argsErr "shen.digit?" ["string"] args

    let rec klAppend globals = function
        | [Empty; Empty] -> Empty
        | [Empty; Cons _ as cons] -> cons
        | [Cons _ as cons; Empty] -> cons
        | [Cons(a, b); Cons _ as cons] -> Cons(a, klAppend globals [b; cons])
        | args -> argsErr "append" ["list"; "list"] args

    let klHash _ = function
        | [x; Int d] ->
            match hash x % d with
            | 0 -> Int 1
            | h -> Int h
        | args -> argsErr "hash" ["value"; "integer"] args

    let klExit _ = function
        | [Int x] ->
            Environment.Exit x
            Empty
        | args -> argsErr "exit" ["integer"] args
