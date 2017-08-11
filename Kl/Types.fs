namespace Kl

open System
open System.Collections.Concurrent
open System.IO
open System.Text
open Microsoft.FSharp.Core.Printf

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
/// An interned symbol reference with optional values in
/// the global symbol and global function namespaces.
/// </summary>
type Symbol = {
    Name: string
    Val: Value option ref
    Fun: Function option ref
}

/// <summary>
/// A global, mutable set of symbol definitions that contains separate
/// symbol and function namespaces.
/// Also contains bi-directional mapping of aliases for CLR types.
/// </summary>
and Globals = {
    InstallingKl: Value option ref
    Symbols: ConcurrentDictionary<string, Symbol>
    ClrAliases: ConcurrentDictionary<string, string>
    ClrReverseAliases: ConcurrentDictionary<string, string>
}

/// <summary>
/// The different types of functions in KL.
/// </summary>
and [<ReferenceEquality>] Function =
    | Interpreted of string list * Expr
    | Compiled    of int * (Globals -> Value list -> Value) * string list
    | Partial     of Function * Value list
    override this.ToString() =
        match this with
        | Interpreted _ -> "<Interpreted Function>"
        | Compiled _    -> "<Compiled Function>"
        | Partial _     -> "<Partial Application>"

/// <summary>
/// A value in KL.
/// </summary>
and Value =
    | Empty
    | Num  of decimal
    | Str  of string
    | Sym  of string
    | Cons of Value * Value
    | Vec  of Value array
    | Err  of string
    | Func of Function
    | Pipe of IO
    | Obj  of obj
    override this.ToString() =
        match this with
        | Empty      -> "[]"
        | Num x      -> x.ToString "0.################"
        | Str s      -> sprintf "\"%s\"" s
        | Sym s      -> s
        | Vec a      -> sprintf "<Vector %i>" a.Length
        | Err s      -> sprintf "<Error \"%s\">" s
        | Func f     -> string f
        | Pipe io    -> sprintf "<Stream %s>" io.Name
        | Obj null   -> "<CLR null>"
        | Obj x      -> sprintf "<CLR %s %O>" (x.GetType().Name) x
        | Cons(x, y) ->
            let builder = StringBuilder()
            bprintf builder "[%O" x
            let rec build = function
                | Empty -> ()
                | Cons(x, y) -> bprintf builder " %O" x; build y
                | x -> bprintf builder " | %O" x
            build y
            bprintf builder "]"
            builder.ToString()

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
