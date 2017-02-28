﻿namespace Kl

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading

module Values =

    let rec combine = function
        | [] -> "."
        | [x] -> x
        | x :: xs -> Path.Combine(x, combine xs)

    // Runs continuation on separate thread with 16MB of stack space
    let separateThread (f: unit -> unit) =
        let thread = new Thread(f, 16777216)
        thread.Start()
        thread.Join()
        0

    // Booleans are just these two particular symbols.
    let True = Sym "true"
    let False = Sym "false"

    let Bool b = if b then True else False
    let (|Bool|_|) = function
        | x when x = True -> Some true
        | x when x = False -> Some false
        | _ -> None

    let isTrue = function
        | Bool b -> b
        | _ -> failwith "Conditional must evaluate to boolean"

    let Int = decimal >> Num
    let (|Int|_|) = function
        | Num x when x % 1.0m = 0.0m -> Some(int x)
        | _ -> None

    let inRange min max value = min <= value && value < max

    let argsErr name types args =
        if List.length types <> List.length args
            then failwithf "%s expected %i arguments, given %i" name types.Length args.Length
            else failwithf "%s expected arguments of type(s): %s" name (String.Join(", ", types))

    let newGlobals() = {
        Symbols = new ConcurrentDictionary<string, Symbol>()
        PrimitiveSymbols = new HashSet<string>()
        PrimitiveFunctions = new HashSet<string>()
    }

    let intern id (globals: Globals) =
        globals.Symbols.GetOrAdd(id, fun _ -> id, ref None, ref None)

    let assign globals id value =
        let (_, sref, _) = intern id globals
        sref := Some value

    let retrieve globals id =
        let (_, sref, _) = intern id globals
        match sref.Value with
        | Some value -> value
        | None -> failwithf "Symbol \"%s\" has no value" id

    let define globals id f =
        let (_, _, fref) = intern id globals
        fref := Some f

    /// <summary>
    /// Looks up id in the global function namespace.
    /// Raises an error if function not defined.
    /// </summary>
    let lookup globals id =
        let (_, _, fref) = intern id globals
        match fref.Value with
        | Some f -> f
        | None -> failwithf "Function \"%s\" is not defined" id

    let localIndex id locals =
        List.tryFindIndex ((=) id) locals |> Option.map (fun x -> locals.Length - x - 1)

    let localInsert id locals =
        List.length locals, (id :: locals)

    let localLookup x (locals: 'a list) =
        locals.[locals.Length - x - 1]

    let merge m0 m1 = Map.fold (fun m k v -> Map.add k v m) m0 m1

    let rec toCons = function
        | [] -> Empty
        | x :: xs -> Cons(x, toCons xs)

    let pipeString (s: string) =
        let stream = new MemoryStream(Encoding.UTF8.GetBytes s)
        Pipe {
            Name = "String"
            Read = stream.ReadByte
            Write = stream.WriteByte
            Close = stream.Close
        }

    let private sequenceOption xs =
        let combine x xs = Option.bind (fun v -> Option.map (fun vs -> v :: vs) xs) x
        List.foldBack combine xs (Some [])

    let rec private toListOption cons =
        match cons with
        | Empty -> Some []
        | Cons(x, y) -> Option.map (fun xs -> x :: xs) (toListOption y)
        | _ -> None

    let (|ConsExpr|_|) = toListOption

    let private condClause = function
        | ConsExpr [x; y] -> Some(x, y)
        | _ -> None

    let private (|CondClauses|_|) = List.map condClause >> sequenceOption

    let (|CondExpr|_|) = function
        | ConsExpr(Sym "cond" :: CondClauses clauses) -> Some clauses
        | _ -> None

    let private param = function
        | Sym s -> Some s
        | _ -> None

    let private (|ParamList|_|) = toListOption >> Option.bind (List.map param >> sequenceOption)

    let (|DefunExpr|_|) = function
        | ConsExpr [Sym "defun"; Sym name; ParamList paramz; body] -> Some(name, paramz, body)
        | _ -> None

    let (|DoExpr|_|) = function
        | ConsExpr [Sym "do"; first; second] -> Some(first, second)
        | _ -> None

module Extensions =
    type IDictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue key with
            | true, x -> Some x
            | false, _ -> None

// Console reader is an adapter that buffers input by line to provide
// character stream to Shen REPL in expected format.
type internal ConsoleReader() =
    let reader = new StreamReader(Console.OpenStandardInput())
    let mutable line: byte[] = [||]
    let mutable index = 0
    member this.ReadByte() =
        if index >= line.Length then
            line <- Encoding.ASCII.GetBytes(reader.ReadLine() + Environment.NewLine)
            index <- 0
        index <- index + 1
        int (line.[index - 1])
