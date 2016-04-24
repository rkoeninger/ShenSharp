namespace Kl

open System.Collections.Generic

type Token =
    | BoolToken   of bool
    | NumberToken of decimal
    | StringToken of string
    | SymbolToken of string
    | ComboToken  of Token list

type Position = Head | Tail

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

type RootExpr =
    | DefunExpr of string * string list * Expr
    | OtherExpr of Expr

type [<ReferenceEquality>] InStream = {Read: unit -> int; Close: unit -> unit}
type [<ReferenceEquality>] OutStream = {Write: byte -> unit; Close: unit -> unit}

type Defines = Dictionary<string, Value>
and Globals = {Symbols: Defines; Functions: Defines}
and Locals = Map<string, Value> list
and Function(name: string, arity: int, locals: Locals, f: Globals -> Value list -> Work<Value>) =
    static member func n a l f = new Function(n, a, l, f)
    member this.Name = name
    member this.Arity = arity
    member this.Locals = locals
    member this.Apply(globals: Globals, args: Value list) = f globals args
    override this.ToString() = this.Name
and Thunk(cont: unit -> Work<Value>) =
    member this.Run = cont
and Value =
    | EmptyValue
    | BoolValue      of bool
    | IntValue       of int
    | DecimalValue   of decimal
    | StringValue    of string
    | SymbolValue    of string
    | FunctionValue  of Function
    // TODO: break function out into multiple cases
    //| Primitive of Function : {Globals -> Value list -> Result, Arity}
    //| Defun of Defun : {Body, Params, Name}
    //| Lambda of Lambda : {Body, Param, Locals}
    //| Freeze of Freeze : {Body, Locals}
    | VectorValue    of Value array
    | ConsValue      of Value * Value
    | ErrorValue     of string
    | InStreamValue  of InStream
    | OutStreamValue of OutStream
and Result<'a> =
    | Ok  of 'a
    | Err of string
and Work<'a> =
    | Done    of Result<'a>
    | Pending of Thunk

type Env = {Globals: Globals; Locals: Locals}
