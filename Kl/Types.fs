namespace Kl

open System
open System.Collections.Generic

type Token =
    | BoolToken    of bool
    | IntToken     of int
    | DecimalToken of decimal
    | StringToken  of string
    | SymbolToken  of string
    | ComboToken   of Token list

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
    let rec go work =
        match work with
        | Pending thunk -> thunk.Run() |> go
        | Done result -> result
    let isVar (s: string) = System.Char.IsUpper(s.Chars 0)
    let newGlobals() = {Symbols = new Defines<Value>(); Functions = new Defines<Function>()}
    let newEnv() = {Globals = newGlobals(); Locals = []}
    let vbool v =
        match v with
        | BoolValue b -> b
        | _ -> failwith "Boolean expected"
    let primitiver name arity f = Primitive(name, arity, f)
    let primitivev name arity f = primitiver name arity (fun globals args -> Ok(f globals args))
    let rec eq a b =
        match a, b with
        | EmptyValue,         EmptyValue         -> true
        | BoolValue x,        BoolValue y        -> x = y
        | IntValue x,         IntValue y         -> x = y
        | DecimalValue x,     DecimalValue y     -> x = y
        | IntValue x,         DecimalValue y     -> decimal x = y
        | DecimalValue x,     IntValue y         -> x = decimal y
        | StringValue x,      StringValue y      -> x = y
        | SymbolValue x,      SymbolValue y      -> x = y
        | InStreamValue x,    InStreamValue y    -> x = y
        | OutStreamValue x,   OutStreamValue y   -> x = y
        | FunctionValue x,    FunctionValue y    -> x = y
        | ErrorValue x,       ErrorValue y       -> x = y
        | ConsValue (x1, x2), ConsValue (y1, y2) -> eq x1 y1 && eq x2 y2
        | VectorValue xs,     VectorValue ys     -> xs.Length = ys.Length && Array.forall2 eq xs ys
        | (_, _) -> false
    let rec toStr value =
        match value with
        | EmptyValue -> "()"
        | BoolValue b -> if b then "true" else "false"
        | IntValue n -> n.ToString()
        | DecimalValue n -> n.ToString()
        | StringValue s -> "\"" + s + "\""
        | SymbolValue s -> s
        | ConsValue (head, tail) -> sprintf "(cons %s %s)" (toStr head) (toStr tail)
        | VectorValue value -> sprintf "(@v%s)" (String.Join("", (Array.map (fun s -> " " + toStr s) value)))
        | ErrorValue message -> sprintf "(simple-error \"%s\")" message
        | FunctionValue f -> sprintf "<Function %s>" (f.ToString())
        | InStreamValue s -> sprintf "<InStream %s>" (s.ToString())
        | OutStreamValue s -> sprintf "<OutStream %s>" (s.ToString())
    let rec toToken value =
        match value with
        | EmptyValue -> ComboToken []
        | BoolValue b -> BoolToken b
        | IntValue i -> IntToken i
        | DecimalValue d -> DecimalToken d
        | StringValue s -> StringToken s
        | SymbolValue s -> SymbolToken s
        | ConsValue _ as cons ->
            let generator value =
                match value with
                | ConsValue (head, tail) -> Some(toToken head, tail)
                | EmptyValue -> None
                | _ -> failwith "Cons chains must form linked lists to be converted to syntax"
            cons |> Seq.unfold generator |> Seq.toList |> ComboToken
        | x -> invalidArg "_" <| x.ToString()

    let (>>=) result f =
        match result with
        | Ok value -> f value
        | Err _ as error -> Done error
