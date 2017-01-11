namespace Kl

open System
open System.Collections.Generic

/// <summary>
/// Exception type that represents KL errors raised by (simple-error).
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
/// An immutable map of local variable definitions.
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
        | Native(name, arity, _) -> sprintf "%s" name
        | Defun(name, paramz, _) -> sprintf "%s" name
        | Lambda _               -> "<Lambda>"
        | Freeze _               -> "<Freeze>"
        | Partial(f, args)       -> sprintf "<Partial %O [%i]>" f args.Length

/// <summary>
/// A value in KL.
/// </summary>
and [<CustomEquality; NoComparison>] Value =
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
    override this.Equals(that: obj) =
        match that with
        | :? Value as that ->
            match this, that with
            | Empty, Empty               -> true
            | Bool x, Bool y             -> x = y
            | Int x, Int y               -> x = y
            | Int x, Dec y               -> decimal x = y
            | Dec x, Int y               -> x = decimal y
            | Dec x, Dec y               -> x = y
            | Str x, Str y               -> x = y
            | Sym x, Sym y               -> x = y
            | Cons(x1, x2), Cons(y1, y2) -> x1 = y1 && x2 = y2
            | Vec xs, Vec ys             -> xs.Length = ys.Length && Array.forall2 (=) xs ys
            | Err x, Err y               -> x = y
            | Func x, Func y             -> x = y
            | InStream x, InStream y     -> x = y
            | OutStream x, OutStream y   -> x = y
            | _, _ -> false
        | _ -> false
    override this.GetHashCode() =
        match this with
        | Empty        -> 1
        | Bool b       -> if b then 2 else 4
        | Int i        -> hash i
        | Dec d        -> hash d
        | Str s        -> hash s
        | Sym s        -> hash s
        | Cons(x1, x2) -> hash x1 ^^^ hash x2
        | Vec xs       -> hash xs
        | Err x        -> hash x
        | Func x       -> hash x
        | InStream x   -> hash x
        | OutStream x  -> hash x
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
        | Vec a       -> sprintf "<Vector [%i]>" a.Length
        | Err s       -> sprintf "<Error [%s]>" s
        | Func f      -> string f
        | InStream  i -> sprintf "<InStream [%s]>" i.Name
        | OutStream o -> sprintf "<OutStream [%s]>" o.Name

/// <summary>
/// Work that may be deferred. Used as trampolines for tail-call optimization.
/// </summary>
type Work =
    | Done    of Value
    | Pending of Thunk

/// <summary>
/// A deferred computation. Thunks are used to defer the evaluation of tail calls.
/// </summary>
and Thunk(cont: unit -> Work) =
    member this.Run = cont

/// <summary>
/// A KL environment state, with a reference to global definitions
/// and local variable bindings.
/// </summary>
type Env = {Globals: Globals; Locals: Locals}

module Overrides =
    let overrides = new Defines<Function>()
