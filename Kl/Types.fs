﻿namespace Kl

open System
open System.Collections.Concurrent
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
    Symbols: ConcurrentDictionary<string, Symbol>
    PrimitiveSymbols: HashSet<string>
    PrimitiveFunctions: HashSet<string>
}

/// <summary>
/// An immutable map of local variable definitions.
/// </summary>
and Locals = Map<string, Value>

/// <summary>
/// The different types of functions in KL.
/// </summary>
and [<ReferenceEquality; DebuggerDisplay("{ToString(),nq}")>] Function =
    | Interpreted of Locals * string list * Expr
    | Compiled    of int * (Globals -> Value list -> Value)
    | Partial     of Function * Value list
    override this.ToString() =
        match this with
        | Interpreted _ -> "<Interpreted Function>"
        | Compiled _    -> "<Compiled Function>"
        | Partial(f, _) -> "<Partial Application>"

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

/// <summary>
/// An interned symbol reference with optional values in
/// the global symbol and global function namespaces.
/// </summary>
and Symbol = string * Value option ref * Function option ref

/// <summary>
/// An optimized KL expression.
/// </summary>
and Expr =
    | Constant    of Value
    | Conjunction of Expr * Expr
    | Disjunction of Expr * Expr
    | Conditional of Expr * Expr * Expr
    | Binding     of string * Expr * Expr
    | Anonymous   of string option * Expr
    | Catch       of Expr * Expr
    | Sequential  of Expr list * Expr
    | Assignment  of Symbol * Expr
    | Retrieval   of Symbol
    | Definition  of Symbol * string list * Expr
    | GlobalCall  of Symbol * Expr list
    | Application of Expr * Expr list
