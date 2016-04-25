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

type Defines<'a> = Dictionary<string, 'a>
and Globals = {Symbols: Defines<Value>; Functions: Defines<Function>}
and Locals = Map<string, Value> list
and [<ReferenceEquality>] Function =
    // TODO: ??? add Native of int * (Globals -> Value list -> Result<Value>)
    //               Primitive of string * Native
    // or just remove names from Primitive and Defun?
    | Primitive of string * int * (Globals -> Value list -> Result<Value>)
    | Defun of string * string list * Expr
    | Lambda of string * Locals * Expr
    | Freeze of Locals * Expr
    | Partial of Value list * Function
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

module Values =
    let truev = BoolValue true
    let falsev = BoolValue false
    let truer = Ok truev
    let falser = Ok falsev
    let truew = Done truer
    let falsew = Done falser
    let thunkw f = new Thunk(f) |> Pending
    let isVar (s: string) = System.Char.IsUpper(s.Chars 0)
    let newGlobals() = {Symbols = new Defines<Value>(); Functions = new Defines<Function>()}
    let newEnv() = {Globals = newGlobals(); Locals = []}
    let vbool v =
        match v with
        | BoolValue b -> b
        | _ -> failwith "Boolean expected"
    let primitiver name arity f = Primitive(name, arity, f)
    let primitivev name arity f = primitiver name arity (fun globals args -> Ok(f globals args))


