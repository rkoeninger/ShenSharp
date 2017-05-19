module Kl.Builtins

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Reflection
open Values
open Interop
open Evaluator
open ShenSharp.Shared

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
        assign globals s x
        x
    | args -> argsErr "set" ["symbol"; "value"] args

let kl_value globals = function
    | [Sym s] -> retrieve globals s
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
            match retrieve globals "*home-directory*" with
            | Str s -> s
            | _ -> Environment.CurrentDirectory
        let fullPath = Path.GetFullPath(combine [current; path])
        Environment.CurrentDirectory <- fullPath
        assign globals "*home-directory*" (Str fullPath)
        Str fullPath
    | args -> argsErr "cd" ["string"] args

let console = Pipe {
    Name = "Console"
    Read = ConsoleReader().ReadByte
    Write = Console.OpenStandardOutput().WriteByte
    Close = fun () -> failwith "Can't close Console"
}

let private epoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
let private startTime = DateTime.UtcNow
let private stopwatch = Stopwatch.StartNew()

let ``kl_get-time`` _ = function
    | [Sym "run"] -> Num(decimal stopwatch.Elapsed.TotalSeconds)
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

let ``kl_clr.alias`` globals = function
    | [Sym alias; Sym original] ->
        setAlias globals alias original
        Empty
    | args -> argsErr "alias" ["symbol"; "symbol"] args

let ``kl_clr.unbox`` _ = function
    | [Obj x] ->
        match x with
        | null -> Empty
        | :? int as i -> Int i
        | :? double as d -> Num(decimal d)
        | :? decimal as d -> Num d
        | :? string as s -> Str s
        | :? bool as b -> Bool b
        | _ -> failwithf "CLR Object %O cannot be converted to a Shen value" x
    | args -> argsErr "clr.unbox" ["clr.obj"] args

let ``kl_clr.null`` _ = function
    | [] -> Obj null
    | args -> argsErr "clr.null" [] args

let ``kl_clr.int`` _ = function
    | [Num x] -> Obj(int x)
    | args -> argsErr "clr.int" ["number"] args

let ``kl_clr.double`` _ = function
    | [Num x] -> Obj(double x)
    | args -> argsErr "clr.double" ["number"] args

let ``kl_clr.decimal`` _ = function
    | [Num x] -> Obj x
    | args -> argsErr "clr.decimal" ["number"] args

let ``kl_clr.string`` _ = function
    | [Str s] -> Obj s
    | args -> argsErr "clr.string" ["string"] args

let ``kl_clr.bool`` _ = function
    | [Bool b] -> Obj b
    | args -> argsErr "clr.bool" ["boolean"] args

let ``kl_clr.new`` globals = function
    | [Sym name; klArgs] -> create globals name klArgs
    | args -> argsErr "clr.new" ["clr.obj"; "(list clr.obj)"] args

let ``kl_clr.get`` _ = function
    | [Obj target; Sym name] ->
        let property = findInstanceProperty target name
        Obj(property.GetValue target)
    | args -> argsErr "clr.get" ["clr.obj"; "symbol"] args

let ``kl_clr.set`` _ = function
    | [Obj target; Sym name; Obj value] ->
        let property = findInstanceProperty target name
        property.SetValue(target, value)
        Empty
    | args -> argsErr "clr.set" ["clr.obj"; "symbol"; "clr.obj"] args

let ``kl_clr.get-index`` _ = function
    | [Obj target; klArgs] ->
        let clrArgs = toList klArgs |> List.map asObj
        let property = findIndexProperty target
        Obj(property.GetValue(target, List.toArray clrArgs))
    | args -> argsErr "clr.get-index" ["clr.obj"; "(list clr.obj)"] args

let ``kl_clr.set-index`` _ = function
    | [Obj target; klArgs; Obj value] ->
        let clrArgs = toList klArgs |> List.map asObj
        let property = findIndexProperty target
        property.SetValue(target, value, List.toArray clrArgs)
        Empty
    | args -> argsErr "clr.set-index" ["clr.obj"; "(list clr.obj)"; "clr.obj"] args

let ``kl_clr.get-static`` globals = function
    | [Sym className; Sym name] ->
        let property = findStaticProperty globals className name
        Obj(property.GetValue null)
    | args -> argsErr "clr.get-static" ["symbol"; "symbol"] args

let ``kl_clr.set-static`` globals = function
    | [Sym className; Sym name; Obj value] ->
        let property = findStaticProperty globals className name
        property.SetValue(null, value)
        Empty
    | args -> argsErr "clr.set-static" ["symbol"; "symbol"; "clr.obj"] args

let ``kl_clr.invoke`` globals = function
    | [Obj target; Sym methodName; klArgs] ->
        let clrArgs = toList klArgs |> List.map asObj
        let methodInfo = findInstanceMethod globals target methodName clrArgs
        Obj(methodInfo.Invoke(target, List.toArray clrArgs))
    | args -> argsErr "clr.invoke-static" ["clr.obj"; "symbol"; "(list clr.obj)"] args

let ``kl_clr.invoke-static`` globals = function
    | [Sym className; Sym methodName; klArgs] ->
        let clrArgs = toList klArgs |> List.map asObj
        let methodInfo = findStaticMethod globals className methodName clrArgs
        Obj(methodInfo.Invoke(null, List.toArray clrArgs))
    | args -> argsErr "clr.invoke-static" ["symbol"; "symbol"; "(list clr.obj)"] args

let ``kl_shen-sharp.globals`` globals = function
    | [] -> Obj globals
    | args -> argsErr "shen-sharp.globals" [] args

let ``kl_shen-sharp.open-socket`` _ = function
    | [Str url] ->
        let client = new WebClient()
        let stream = client.OpenRead url
        Pipe {
            Name = "Socket: " + url
            Read = stream.ReadByte
            Write = stream.WriteByte
            Close = stream.Close
        }
    | args -> argsErr "shen-sharp.open-socket" ["string"] args

let ``kl_shen-sharp.download`` _ = function
    | [Str url] ->
        use client = new WebClient()
        Str(client.DownloadString(url))
    | args -> argsErr "shen-sharp.download" ["string"] args
