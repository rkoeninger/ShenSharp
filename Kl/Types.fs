namespace Kl

open System
open System.Collections.Generic
open System.Diagnostics

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
/// Primitives is a list of symbols that bootstrap the KL environment
/// which should not be re-defined.
/// </summary>
type Globals = {
    Symbols: Dictionary<string, Value>
    Functions: Dictionary<string, Function>
    PrimitiveSymbols: HashSet<string>
    PrimitiveFunctions: HashSet<string>
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
