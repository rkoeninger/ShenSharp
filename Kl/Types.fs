namespace Kl

open System
open System.Collections.Generic

/// <summary>
/// Exception type that embodies KL errors.
/// </summary>
type SimpleError(message) =
    inherit Exception(message)

type [<ReferenceEquality>] Input =  {Name: string; Read: unit -> int;   Close: unit -> unit}
type [<ReferenceEquality>] Output = {Name: string; Write: byte -> unit; Close: unit -> unit}

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
/// A map of local variable definitions.
/// </summary>
and Locals = Map<string, Value>

/// <summary>
/// The different types of functions in KL.
/// </summary>
and [<ReferenceEquality>] Function =
    | Native  of string * int * (Globals -> Value list -> Value)
    | Defun   of string * string list * Value
    | Lambda  of string * Locals * Value
    | Freeze  of Locals * Value
    | Partial of Function * Value list
    override this.ToString() =
        match this with
        | Native(name, arity, _) -> sprintf "%s/%i" name arity
        | Defun(name, paramz, _) -> sprintf "%s/%i" name paramz.Length
        | Lambda _               -> "<Lambda>"
        | Freeze _               -> "<Freeze>"
        | Partial(f, args)       -> sprintf "<Partial (%O)/%i>" f args.Length

/// <summary>
/// A value in KL.
/// </summary>
and Value =
    | Empty
    | Bool      of bool
    | Int       of int
    | Dec       of decimal
    | Str       of string
    | Sym       of string
    | Cons      of Value * Value
    | Vec       of Value array
    | Err       of string
    | Func      of Function
    | InStream  of Input
    | OutStream of Output
    override this.ToString() =
        let rec toList = function
            | Cons(x, y) -> x :: toList y
            | _ -> []
        match this with
        | Empty       -> "()"
        | Bool b      -> if b then "true" else "false"
        | Int i       -> string i
        | Dec d       -> string d
        | Str s       -> sprintf "\"%s\"" s
        | Sym s       -> s
        | Cons _      -> sprintf "(%s)" (String.Join(" ", toList this))
        | Vec a       -> sprintf "<Vector/%i>" a.Length
        | Err s       -> sprintf "<Error \"%s\">" s
        | Func f      -> string f
        | InStream  i -> sprintf "<InStream %s>" i.Name
        | OutStream o -> sprintf "<OutStream %s>" o.Name

/// <summary>
/// A potentially deferred computation yielding a value of type <c>'a</c>.
/// </summary>
type Work =
    | Done    of Value
    | Pending of Thunk

/// <summary>
/// A deferred computation. Thunks are used to defer the evaluation
/// of tail calls.
/// </summary>
and Thunk(cont: unit -> Work) =
    member this.Run = cont

/// <summary>
/// A KL environment state, with a reference to global definitions
/// and local variable bindings.
/// </summary>
type Env = {Globals: Globals; Locals: Locals}
