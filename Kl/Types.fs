namespace Kl

type KlToken = BoolToken   of bool
             | NumberToken of decimal
             | StringToken of string
             | SymbolToken of string
             | ComboToken  of KlToken list

type Position = Head | Tail

type KlExpr = EmptyExpr
            | BoolExpr    of bool
            | IntExpr     of int
            | DecimalExpr of decimal
            | StringExpr  of string
            | SymbolExpr  of string
            | AndExpr     of KlExpr * KlExpr
            | OrExpr      of KlExpr * KlExpr
            | IfExpr      of KlExpr * KlExpr * KlExpr
            | CondExpr    of (KlExpr * KlExpr) list
            | LetExpr     of string * KlExpr * KlExpr
            | LambdaExpr  of string * KlExpr
            | DefunExpr   of string * string list * KlExpr
            | FreezeExpr  of KlExpr
            | TrapExpr    of Position * KlExpr * KlExpr
            | AppExpr     of Position * KlExpr * KlExpr list
  
// TODO: make defun top-level only (thanks, shentong)
//type KlTopLevelExpr = DefunExpr of string
//                    | OtherExpr of KlExpr

type [<ReferenceEquality>] InStream = {Read: unit -> int; Close: unit -> unit}
type [<ReferenceEquality>] OutStream = {Write: byte -> unit; Close: unit -> unit}

type Defines = System.Collections.Generic.Dictionary<string, KlValue>
and Globals = {Symbols: Defines; Functions: Defines}
and Locals = Map<string, KlValue> list
and Function(name: string, arity: int, locals: Locals, f: Globals -> KlValue list -> Work) =
    static member func n a l f = new Function(n, a, l, f)
    member this.Name = name
    member this.Arity = arity
    member this.Locals = locals
    member this.Apply(globals: Globals, args: KlValue list) = f globals args
    override this.ToString() = this.Name
and Thunk(cont: unit -> Work) =
    member this.Run = cont
and KlValue = EmptyValue
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
            | VectorValue    of KlValue array
            | ConsValue      of KlValue * KlValue
            | ErrorValue     of string
            | InStreamValue  of InStream
            | OutStreamValue of OutStream
and Result = ValueResult of KlValue
           | ErrorResult of string
and Work = Completed of Result
         | Pending   of Thunk

type Env = {Globals: Globals; Locals: Locals}

// TODO: find some other way to do this (?)
type FunctionResolveResult = FunctionResult of Function
                           | FunctionResolveError of string
