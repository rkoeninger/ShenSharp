namespace Kl

open System
open System.Collections.Generic

/// <summary>
/// Head/Tail position of an expression.
/// Used for tail call optimization.
/// </summary>
type Position = Head | Tail

// TODO: these just show up as "exception of type Kl.SimpleError raised"
//       needs to show actual message

/// <summary>
/// Exception type that embodies KL errors.
/// </summary>
exception SimpleError of string

type [<ReferenceEquality>] Input = {Read: unit -> int; Close: unit -> unit}
type [<ReferenceEquality>] Output = {Write: byte -> unit; Close: unit -> unit}

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
    | Native of string * int * (Globals -> Value list -> Value)
    | Defun of string * string list * Value
    | Lambda of string * Locals * Value
    | Freeze of Locals * Value
    | Partial of Function * Value list
    
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
    | Func      of Function
    | Vec       of Value array
    | Cons      of Value * Value
    | Err       of string
    | InStream  of Input
    | OutStream of Output

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
