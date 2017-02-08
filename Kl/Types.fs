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
/// A global, mutable set of symbol definitions that contains separate
/// symbol and function namespaces.
/// </summary>
type Globals = {
    Symbols: Dictionary<string, Value>
    Functions: Dictionary<string, Function>
}

/// <summary>
/// An immutable map of local variable definitions.
/// </summary>
and Locals = Map<string, Value>

and [<ReferenceEquality>] DefunImpl =
    | CompiledDefun of (Globals -> Value list -> Value)
    | InterpretedDefun of string list * Value

and [<ReferenceEquality>] LambdaImpl =
    | CompiledLambda of (Globals -> Value -> Value)
    | InterpretedLambda of Locals * string * Value

and [<ReferenceEquality>] FreezeImpl =
    | CompiledFreeze of (Globals -> Value)
    | InterpretedFreeze of Locals * Value

/// <summary>
/// The different types of functions in KL.
/// </summary>
and [<DebuggerDisplay("{ToString(),nq}")>] Function =
    | Defun   of string * int * DefunImpl
    | Lambda  of LambdaImpl
    | Freeze  of FreezeImpl
    | Partial of Function * Value list
    override this.ToString() =
        match this with
        | Defun(name, _, _)                         -> sprintf "%s" name
        | Lambda(CompiledLambda _)                  -> "<Lambda Compiled>"
        | Lambda(InterpretedLambda(_, param, body)) -> sprintf "<Lambda %s %O>" param body
        | Freeze(CompiledFreeze _)                  -> "<Freeze Compiled>"
        | Freeze(InterpretedFreeze(_, body))        -> sprintf "<Freeze %O>" body
        | Partial(f, args)                          -> sprintf "<Partial %O %s>" f (String.Join(" ", args))

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
        | Vec a   -> sprintf "<Vector %i>" a.Length
        | Err s   -> sprintf "<Error \"%s\">" s
        | Func f  -> string f
        | Pipe io -> sprintf "<Stream %s>" io.Name

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

module Extensions =
    type IDictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue(key) with
            | true, x -> Some x
            | false, _ -> None

    let (|Greater|Equal|Lesser|) = function
        | x, y when x > y -> Greater
        | x, y when x < y -> Lesser
        | _ -> Equal
