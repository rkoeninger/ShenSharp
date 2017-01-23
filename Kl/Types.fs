namespace Kl

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text

/// <summary>
/// A wrapper around a stream. Has a Name property
/// for easy reference in the REPL.
/// </summary>
type [<ReferenceEquality>] IO = {
    Name: string
    Read: unit -> int
    Write: byte -> unit
    Close: unit -> unit
}

/// <summary>
/// A mutable dictionary that maps symbols to values of some type <c>'a</c>.
/// </summary>
type Defines<'a> = Dictionary<string, 'a>

/// <summary>
/// A global, mutable set of symbol definitions that contains separate
/// symbol and function namespaces.
/// </summary>
type Globals = {Symbols: Defines<Value>; Functions: Defines<Function>}

/// <summary>
/// An immutable map of local variable definitions.
/// </summary>
and Locals = Map<string, Value>

/// <summary>
/// The different types of functions in KL.
/// </summary>
and [<ReferenceEquality; DebuggerDisplay("{ToString(),nq}")>] Function =
    | Native  of string * int * (Globals -> Value list -> Value)
    | Defun   of string * string list * Value
    | Lambda  of string * Locals * Value
    | Freeze  of Locals * Value
    | Partial of Function * Value list
    override this.ToString() =
        match this with
        | Native(name, arity, _) -> sprintf "%s" name
        | Defun(name, paramz, _) -> sprintf "%s" name
        | Lambda(param, _, body) -> sprintf "<Lambda (%s) %O>" param body
        | Freeze(_, body)        -> sprintf "<Freeze %O>" body
        | Partial(f, args)       -> sprintf "<Partial %O (%i)>" f args.Length

/// <summary>
/// A value in KL.
/// </summary>
and [<DebuggerDisplay("{ToString(),nq}")>] Value =
    | Empty
    | Num  of decimal
    | Str  of string
    | Sym  of string
    | Cons of Value * Value
    | Vec  of Value array
    | Err  of string
    | Func of Function
    | Pipe of IO
    override this.ToString() =
        let rec toList = function
            | Cons(x, y) -> x :: toList y
            | _ -> []
        match this with
        | Empty   -> "()"
        | Num x   -> x.ToString("0.################")
        | Str s   -> sprintf "\"%s\"" s
        | Sym s   -> s
        | Cons _  -> sprintf "(%s)" (String.Join(" ", toList this))
        | Vec a   -> sprintf "<Vector (%i)>" a.Length
        | Err s   -> sprintf "<Error (%s)>" s
        | Func f  -> string f
        | Pipe io -> sprintf "<Stream (%s)>" io.Name

/// <summary>
/// Exception type that represents KL errors raised by (simple-error).
/// </summary>
type SimpleError(message) =
    inherit Exception(message)

type ConsoleReader() =
    let reader = new StreamReader(Console.OpenStandardInput())
    let mutable line: byte[] = [||]
    let mutable index = 0
    member this.ReadByte() =
        if index >= line.Length then
            line <- Encoding.ASCII.GetBytes(reader.ReadLine() + Environment.NewLine)
            index <- 0
        index <- index + 1
        int (line.[index - 1])
    member this.Close = reader.Close

type ConsoleWriter() =
    let stream = Console.OpenStandardOutput()
    member this.WriteByte = stream.WriteByte
    member this.Close = stream.Close

module Extensions =
    type IDictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue(key) with
            | true, x -> Some x
            | false, _ -> None

    let (|Greater|Equal|Lesser|) (x, y) =
        if x > y
            then Greater
        elif x < y
            then Lesser
        else Equal

module Values =
    let truev = Sym "true"
    let falsev = Sym "false"
    let boolv b = if b then truev else falsev
    let err s = raise(SimpleError s)
    let errf format = Printf.ksprintf err format
    let newGlobals() = {Symbols = new Defines<Value>(); Functions = new Defines<Function>()}
    let rec toCons = function
        | [] -> Empty
        | x :: xs -> Cons(x, toCons xs)
    let (|Int|_|) = function
        | Num x when x % 1.0m = 0.0m -> Some(int x)
        | _ -> None
    let Int = Num << decimal
