namespace Kl

open Extensions
open System
open System.Diagnostics
open System.IO
open Kl.Values
open Kl.Evaluator

module Builtins =

    let kl_if _ = function
        | [Bool c; x; y] -> if c then x else y
        | args -> argsErr "if" ["boolean"; "value"; "value"] args

    let kl_and _ = function
        | [Bool x; Bool y] -> Bool(x && y)
        | args -> argsErr "and" ["boolean"; "boolean"] args

    let kl_or _ = function
        | [Bool x; Bool y] -> Bool(x || y)
        | args -> argsErr "or" ["boolean"; "boolean"] args

    let kl_intern _ = function
        | [Str s] -> Sym s
        | args -> argsErr "intern" ["string"] args

    let kl_pos _ = function
        | [Str s; Int index] when inRange 0 s.Length index -> Str(string s.[index])
        | [Str s; Int index] -> failwithf "Index %i out of bounds for string of length %i" index s.Length
        | args -> argsErr "pos" ["string"; "integer"] args

    let kl_tlstr _ = function
        | [Str ""] -> failwithf "tlstr expects a non-empty string"
        | [Str s] -> Str(s.Substring 1)
        | args -> argsErr "tlstr" ["string"] args

    let kl_cn _ = function
        | [Str x; Str y] -> Str(x + y)
        | args -> argsErr "cn" ["string"; "string"] args

    let kl_str _ = function
        | [x: Value] -> Str(string x)
        | args -> argsErr "str" ["value"] args

    let ``kl_n->string`` _ = function
        | [Int n] -> Str(string(char n))
        | args -> argsErr "n->string" ["integer"] args

    let ``kl_string->n`` _ = function
        | [Str s] -> Int(int s.[0])
        | args -> argsErr "string->n" ["string"] args

    let kl_set globals = function
        | [Sym s; x] ->
            globals.Symbols.[s] <- x
            x
        | args -> argsErr "set" ["symbol"; "value"] args

    let kl_value globals = function
        | [Sym s] ->
            match globals.Symbols.GetMaybe s with
            | Some x -> x
            | None -> failwithf "Symbol \"%s\" is undefined" s
        | args -> argsErr "value" ["symbol"] args

    let ``kl_simple-error`` _ = function
        | [Str s] -> failwith s
        | args -> argsErr "simple-error" ["string"] args

    let ``kl_error-to-string`` _ = function
        | [Err s] -> Str s
        | args -> argsErr "error-to-string" ["error"] args

    let kl_cons _ = function
        | [x; y] -> Cons(x, y)
        | args -> argsErr "cons" ["value"; "value"] args

    let kl_hd _ = function
        | [Cons (x, _)] -> x
        | args -> argsErr "hd" ["cons"] args

    let kl_tl _ = function
        | [Cons (_, y)] -> y
        | args -> argsErr "tl" ["cons"] args

    let ``kl_eval-kl`` globals = function
        | [x] -> eval globals x
        | args -> argsErr "eval-kl" ["value"] args

    let kl_type _ = function
        | [x; _] -> x
        | args -> argsErr "type" ["value"; "symbol"] args

    let kl_absvector _ = function
        | [Int length] -> Vec(Array.create length Empty)
        | args -> argsErr "absvector" ["integer"] args

    let ``kl_<-address`` _ = function
        | [Vec array; Int index] when inRange 0 array.Length index -> array.[index]
        | [Vec array; Int index] -> failwithf "Index %i out of bounds for vector of length %i" index array.Length
        | args -> argsErr "<-address" ["vector"; "integer"] args

    let ``kl_address->`` _ = function
        | [Vec array as vector; Int index; value] when inRange 0 array.Length index ->
            array.[index] <- value
            vector
        | [Vec array; Int index] -> failwithf "Index %i out of bounds for vector of length %i" index array.Length
        | args -> argsErr "address->" ["vector"; "integer"; "value"] args

    let ``kl_write-byte`` _ = function
        | [Int x; Pipe io] when inRange 0 256 x ->
            let b = byte x
            io.Write b
            Int(int b)
        | [Int x; Pipe _] -> failwithf "integer value %i is exceeds the range of a byte" x
        | args -> argsErr "write-byte" ["byte"; "stream"] args

    let ``kl_read-byte`` _ = function
        | [Pipe io] -> Int(io.Read())
        | args -> argsErr "read-byte" ["stream"] args

    let kl_open _ = function
        | [Str path; Sym s] ->
            let stream =
                match s with
                | "in" -> File.OpenRead path
                | "out" -> File.OpenWrite path
                | _ -> failwithf "open expects symbol 'in or 'out as 2nd argument, not '%s" s
            Pipe {
                Name = "File: " + path
                Read = stream.ReadByte
                Write = stream.WriteByte
                Close = stream.Close
            }
        | args -> argsErr "open" ["string"; "symbol"] args

    let kl_close _ = function
        | [Pipe io] ->
            io.Close()
            Empty
        | args -> argsErr "close" ["stream"] args

    let kl_cd globals = function
        | [Str path] ->
            let current =
                match globals.Symbols.["*home-directory*"] with
                | Str s -> s
                | _ -> ""
            let fullPath = Path.GetFullPath(Path.Combine(current, path))
            Environment.CurrentDirectory <- fullPath
            globals.Symbols.["*home-directory*"] <- Str fullPath
            Str fullPath
        | args -> argsErr "cd" ["string"] args

    let kl_pwd globals = function
        | [] -> globals.Symbols.["*home-directory*"]
        | args -> argsErr "pwd" [] args

    let kl_ls globals = function
        | [] ->
            match globals.Symbols.["*home-directory*"] with
            | Str s ->
                Directory.GetFileSystemEntries s
                |> Array.toList
                |> List.map (Path.GetFileName >> Str)
                |> toCons
            | _ -> Empty
        | args -> argsErr "ls" [] args

    let console = Pipe {
        Name = "Console"
        Read = (new ConsoleReader()).ReadByte
        Write = Console.OpenStandardOutput().WriteByte
        Close = fun () -> failwith "Can't close Console"
    }

    let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private startTime = DateTime.UtcNow
    let private stopwatch = Stopwatch.StartNew()

    let ``kl_get-time`` _ = function
        | [Sym "run"] | [Sym "real"] -> Num(decimal stopwatch.Elapsed.TotalSeconds)
        | [Sym "unix"] -> Num(decimal (DateTime.UtcNow - epoch).TotalSeconds)
        | [Sym s] -> failwithf "get-time expects symbols 'run or 'unix as argument, not %s" s
        | args -> argsErr "get-time" ["symbol"] args

    let ``kl_+`` _ = function
        | [Num x; Num y] -> Num(x + y)
        | args -> argsErr "+" ["number"; "number"] args

    let ``kl_-`` _ = function
        | [Num x; Num y] -> Num(x - y)
        | args -> argsErr "-" ["number"; "number"] args

    let ``kl_*`` _ = function
        | [Num x; Num y] -> Num(x * y)
        | args -> argsErr "*" ["number"; "number"] args

    let ``kl_/`` _ = function
        | [Num x; Num y] -> Num(x / y)
        | args -> argsErr "/" ["number"; "number"] args

    let ``kl_>`` _ = function
        | [Num x; Num y] -> Bool(x > y)
        | args -> argsErr ">" ["number"; "number"] args

    let ``kl_<`` _ = function
        | [Num x; Num y] -> Bool(x < y)
        | args -> argsErr "<" ["number"; "number"] args

    let ``kl_>=`` _ = function
        | [Num x; Num y] -> Bool(x >= y)
        | args -> argsErr ">=" ["number"; "number"] args

    let ``kl_<=`` _ = function
        | [Num x; Num y] -> Bool(x <= y)
        | args -> argsErr "<=" ["number"; "number"] args

    let ``kl_=`` _ = function
        | [x; y] -> Bool(x = y)
        | args -> argsErr "=" ["value"; "value"] args

    let ``kl_number?`` _ = function
        | [Num _] -> True
        | [_] -> False
        | args -> argsErr "number?" ["value"] args

    let ``kl_string?`` _ = function
        | [Str _] -> True
        | [_] -> False
        | args -> argsErr "string?" ["value"] args

    let ``kl_cons?`` _ = function
        | [Cons _] -> True
        | [_] -> False
        | args -> argsErr "cons?" ["value"] args

    let ``kl_absvector?`` _ = function
        | [Vec _] -> True
        | [_] -> False
        | args -> argsErr "absvector?" ["value"] args

    let kl_exit _ = function
        | [Int x] -> exit x
        | args -> argsErr "exit" ["integer"] args
