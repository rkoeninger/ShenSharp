namespace Kl

open Extensions
open System
open System.Diagnostics
open System.IO
open Kl.Values
open Kl.Evaluator

module Builtins =

    let private argsErr name types args =
        if List.length types <> List.length args
            then errf "%s expected %i arguments, given %i" name types.Length args.Length
            else errf "%s expected arguments of type(s): %s" name (String.Join(", ", types))

    let klIf _ = function
        | [Bool c; x; y] -> if c then x else y
        | args -> argsErr "if" ["boolean"; "value"; "value"] args

    let klAnd _ = function
        | [Bool x; Bool y] -> Bool(x && y)
        | args -> argsErr "and" ["boolean"; "boolean"] args

    let klOr _ = function
        | [Bool x; Bool y] -> Bool(x || y)
        | args -> argsErr "or" ["boolean"; "boolean"] args

    let klIntern _ = function
        | [Str s] -> Sym s
        | args -> argsErr "intern" ["string"] args

    let klStringPos _ = function
        | [Str s; Int index] when inRange 0 s.Length index -> Str(string s.[index])
        | [Str s; Int index] -> errf "Index %i out of bounds for string of length %i" index s.Length
        | args -> argsErr "pos" ["string"; "integer"] args

    let klStringTail _ = function
        | [Str ""] -> err "tlstr expects a non-empty string"
        | [Str s] -> Str(s.Substring 1)
        | args -> argsErr "tlstr" ["string"] args

    let klStringConcat _ = function
        | [Str x; Str y] -> Str(x + y)
        | args -> argsErr "cn" ["string"; "string"] args

    let klToString _ = function
        | [x: Value] -> Str(string x)
        | args -> argsErr "str" ["value"] args

    let klIsString _ = function
        | [Str _] -> True
        | [_] -> False
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
        | [Cons _] -> True
        | [_] -> False
        | args -> argsErr "cons?" ["value"] args

    let klEquals _ = function
        | [x; y] -> Bool(x = y)
        | args -> argsErr "=" ["value"; "value"] args

    let klEval globals = function
        | [x] -> eval globals x
        | args -> argsErr "eval-kl" ["value"] args

    let klType _ = function
        | [x; _] -> x
        | args -> argsErr "type" ["value"; "symbol"] args

    let klNewVector _ = function
        | [Int length] -> Vec(Array.create length Empty)
        | args -> argsErr "absvector" ["integer"] args

    let klReadVector _ = function
        | [Vec array; Int index] when inRange 0 array.Length index -> array.[index]
        | [Vec array; Int index] -> errf "Index %i out of bounds for vector of length %i" index array.Length
        | args -> argsErr "<-address" ["vector"; "integer"] args

    let klWriteVector _ = function
        | [Vec array as vector; Int index; value] when inRange 0 array.Length index ->
            array.[index] <- value
            vector
        | [Vec array; Int index] -> errf "Index %i out of bounds for vector of length %i" index array.Length
        | args -> argsErr "address->" ["vector"; "integer"; "value"] args

    let klIsVector _ = function
        | [Vec _] -> True
        | [_] -> False
        | args -> argsErr "absvector?" ["value"] args

    let klWriteByte _ = function
        | [Int x; Pipe io] when inRange 0 256 x ->
            let b = byte x
            io.Write b
            Int(int b)
        | [Int x; Pipe _] -> errf "integer value %i is exceeds the range of a byte" x
        | args -> argsErr "write-byte" ["byte"; "stream"] args

    let klReadByte _ = function
        | [Pipe io] -> Int(io.Read())
        | args -> argsErr "read-byte" ["stream"] args

    let klOpen _ = function
        | [Str path; Sym s] ->
            let stream =
                try match s with
                    | "in" -> File.OpenRead path
                    | "out" -> File.OpenWrite path
                    | _ -> errf "open expects symbol 'in or 'out as 2nd argument, not '%s" s
                with e -> err e.Message
            Pipe {
                Name = "File: " + path
                Read = stream.ReadByte
                Write = stream.WriteByte
                Close = stream.Close
            }
        | args -> argsErr "open" ["string"; "symbol"] args

    let klClose _ = function
        | [Pipe io] ->
            io.Close()
            Empty
        | args -> argsErr "close" ["stream"] args

    let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private startTime = DateTime.UtcNow
    let private stopwatch = Stopwatch.StartNew()

    let klGetTime _ = function
        | [Sym "run"] | [Sym "real"] -> Num(decimal stopwatch.Elapsed.TotalSeconds)
        | [Sym "unix"] -> Num(decimal (DateTime.UtcNow - epoch).TotalSeconds)
        | [Sym s] -> errf "get-time expects symbols 'run or 'unix as argument, not %s" s
        | args -> argsErr "get-time" ["symbol"] args

    let klAdd _ = function
        | [Num x; Num y] -> Num(x + y)
        | args -> argsErr "+" ["number"; "number"] args

    let klSubtract _ = function
        | [Num x; Num y] -> Num(x - y)
        | args -> argsErr "-" ["number"; "number"] args

    let klMultiply _ = function
        | [Num x; Num y] -> Num(x * y)
        | args -> argsErr "*" ["number"; "number"] args

    let klDivide _ = function
        | [_; Num 0m] -> err "Division by zero"
        | [Num x; Num y] -> Num(x / y)
        | args -> argsErr "/" ["number"; "number"] args

    let klGreaterThan _ = function
        | [Num x; Num y] -> Bool(x > y)
        | args -> argsErr ">" ["number"; "number"] args

    let klLessThan _ = function
        | [Num x; Num y] -> Bool(x < y)
        | args -> argsErr "<" ["number"; "number"] args

    let klGreaterThanEqual _ = function
        | [Num x; Num y] -> Bool(x >= y)
        | args -> argsErr ">=" ["number"; "number"] args

    let klLessThanEqual _ = function
        | [Num x; Num y] -> Bool(x <= y)
        | args -> argsErr "<=" ["number"; "number"] args

    let klIsNumber _ = function
        | [Num _] -> True
        | [_] -> False
        | args -> argsErr "number?" ["value"] args

    let console = Pipe {
            Name = "Console"
            Read = (new ConsoleReader()).ReadByte
            Write = Console.OpenStandardOutput().WriteByte
            Close = fun () -> err "Can't close Console"
        }

    let klFillVector _ = function
        | [Vec array as vector; Int start; Int stop; fillValue] ->
            Array.fill array start (stop - start) fillValue
            vector
        | args -> argsErr "shen.fillvector" ["vector"; "integer"; "integer"; "value"] args

    let klModulus _ = function
        | [_; Num 0m] -> err "Modulus by zero"
        | [Num x; Num y] -> Num(x % y)
        | args -> argsErr "shen.mod" ["number"; "number"] args

    let klHash _ = function
        | [x; Int i] ->
            match hash x % i with
            | 0 -> Int 1
            | h -> Int h
        | args -> argsErr "hash" ["value"; "integer"] args

    let klExit _ = function
        | [Int x] ->
            Environment.Exit x
            Empty
        | args -> argsErr "exit" ["integer"] args

    let klCd globals = function
        | [Str path] ->
            let current =
                match globals.Symbols.["*home-directory*"] with
                | Str s -> s
                | _ -> ""
            let fullPath = Path.GetFullPath(Path.Combine(current, path))
            try Environment.CurrentDirectory <- fullPath
            with e -> err e.Message
            globals.Symbols.["*home-directory*"] <- Str fullPath
            Str fullPath
        | args -> argsErr "cd" ["string"] args
