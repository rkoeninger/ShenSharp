namespace Kl

open System
open System.Collections.Generic

/// <summary>
/// A node in a KL syntax tree.
/// </summary>
type Token =
    | BoolToken    of bool
    | IntToken     of int
    | DecimalToken of decimal
    | StringToken  of string
    | SymbolToken  of string
    | ComboToken   of Token list

/// <summary>
/// Head/Tail position of an expression.
/// Used for tail call optimization.
/// </summary>
type Position = Head | Tail

/// <summary>
/// A KL expression.
/// </summary>
type Expr =
    | EmptyExpr
    | BoolExpr    of bool
    | IntExpr     of int
    | DecimalExpr of decimal
    | StringExpr  of string
    | SymbolExpr  of string
    | AndExpr     of Expr * Expr
    | OrExpr      of Expr * Expr
    | IfExpr      of Expr * Expr * Expr
    | CondExpr    of (Expr * Expr) list
    | LetExpr     of string * Expr * Expr
    | LambdaExpr  of string * Expr
    | FreezeExpr  of Expr
    | TrapExpr    of Position * Expr * Expr
    | AppExpr     of Position * Expr * Expr list

/// <summary>
/// Exception type that embodies KL errors.
/// </summary>
exception SimpleError of string

/// <summary>
/// A separate type used to enforce the fact that <c>DefunExpr</c>s
/// can only appear at the root level.
/// </summary>
type RootExpr =
    | DefunExpr of string * string list * Expr
    | OtherExpr of Expr

type [<ReferenceEquality>] InStream = {Read: unit -> int; Close: unit -> unit}
type [<ReferenceEquality>] OutStream = {Write: byte -> unit; Close: unit -> unit}

/// <summary>
/// A mutable dictionary that maps symbols to values of some type <c>'a</c>.
/// </summary>
type Defines<'a> = Dictionary<string, 'a>

/// <summary>
/// A global, mutable set of symbol definitions that contains separate
/// symbol and function namespaces.
/// </summary>
and Globals = {Symbols: Defines<Value>; Functions: Defines<Function>}

/// <summary>
/// A stack of local variable definitions.
/// </summary>
and Locals = Map<string, Value> list

/// <summary>
/// The different types of functions in KL.
/// </summary>
and [<ReferenceEquality>] Function =
    | Primitive of string * int * (Globals -> Value list -> Value) // TODO: remove arity?
    | Defun of string * string list * Expr
    | Lambda of string * Locals * Expr
    | Freeze of Locals * Expr
    | Partial of Function * Value list

/// <summary>
/// A value in KL.
/// </summary>
and Value =
    | EmptyValue
    | BoolValue      of bool
    | IntValue       of int
    | DecimalValue   of decimal
    | StringValue    of string
    | SymbolValue    of string
    | FunctionValue  of Function
    | VectorValue    of Value array
    | ConsValue      of Value * Value
    | ErrorValue     of string
    | InStreamValue  of InStream
    | OutStreamValue of OutStream

/// <summary>
/// A potentially deferred computation yielding a value of type <c>'a</c>.
/// </summary>
and Work<'a> =
    | Done    of 'a
    | Pending of Thunk<'a>

/// <summary>
/// A deferred computation. Thunks are used to defer the evaluation
/// of tail calls.
/// </summary>
and Thunk<'a>(cont: unit -> Work<'a>) =
    member this.Run = cont

/// <summary>
/// A KL environment state, with a reference to global definitions
/// and local variable bindings.
/// </summary>
type Env = {Globals: Globals; Locals: Locals}
