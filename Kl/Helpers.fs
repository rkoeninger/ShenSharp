namespace Kl

open System
open System.IO
open System.Collections.Generic

type ConsoleIn(stream: Stream) =
    let reader = new StreamReader(stream)
    let mutable currentLine = ""
    let mutable currentPos = 0
    member this.Read() = 
        if currentPos >= currentLine.Length then
            currentLine <- reader.ReadLine()
            if Object.ReferenceEquals(currentLine, null) then
                -1
            else
                currentLine <- currentLine + "\n"
                currentPos <- 0
                let ch = currentLine.[currentPos]
                currentPos <- currentPos + 1
                (int) ch
        else
            let ch = currentLine.[currentPos]
            currentPos <- currentPos + 1
            (int) ch
    member this.Close() = stream.Close()

module Extensions =
    type Dictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue(key) with
            | true, x -> Some x
            | false, _ -> None

    let (|Greater|Equal|Lesser|) (x, y) =
        if x > y
            then Greater
        elif x < y
            then Lesser
        else Equal

module Values =
    let truev = Bool true
    let falsev = Bool false
    let truew = Done truev
    let falsew = Done falsev

    let err s = raise(SimpleError s)

    let trap f handler =
        try f()
        with SimpleError e -> handler(Err e)

    let thunkw f = Pending(new Thunk(f))

    let rec go work =
        match work with
        | Pending thunk -> go(thunk.Run())
        | Done result -> result

    let isVar (s: string) = Char.IsUpper s.[0]

    let newGlobals() = {Symbols = new Defines<Value>(); Functions = new Defines<Function>()}
    let newEnv() = {Globals = newGlobals(); Locals = Map.empty}

    let vbool v =
        match v with
        | Bool b -> b
        | _ -> err "Boolean expected"

    let vsym v =
        match v with
        | Sym s -> s
        | _ -> err "Symbol expected"

    let vstr v =
        match v with
        | Str s -> s
        | _ -> err "String expected"

    let vfunc v =
        match v with
        | Func f -> f
        | _ -> err "Function expected"

    let rec eq a b =
        match a, b with
        | Empty,        Empty        -> true
        | Bool x,       Bool y       -> x = y
        | Int x,        Int y        -> x = y
        | Dec x,        Dec y        -> x = y
        | Int x,        Dec y        -> decimal x = y
        | Dec x,        Int y        -> x = decimal y
        | Str x,        Str y        -> x = y
        | Sym x,        Sym y        -> x = y
        | InStream x,   InStream y   -> x = y
        | OutStream x,  OutStream y  -> x = y
        | Func x,       Func y       -> x = y
        | Err x,        Err y        -> x = y
        | Cons(x1, x2), Cons(y1, y2) -> eq x1 y1 && eq x2 y2
        | Vec xs,       Vec ys       -> xs.Length = ys.Length && Array.forall2 eq xs ys
        | _, _ -> false

    let rec toStr value =
        let join values = (String.Join("", (Seq.map (fun s -> " " + toStr s) values)))
        match value with
        | Empty -> "()"
        | Bool b -> if b then "true" else "false"
        | Int n -> n.ToString()
        | Dec n -> n.ToString()
        | Str s -> sprintf "\"%s\"" s
        | Sym s -> s
        | Cons(head, tail) -> sprintf "(cons %s %s)" (toStr head) (toStr tail)
        | Vec array -> sprintf "(@v%s)" (join array)
        | Err message -> sprintf "(simple-error \"%s\")" message
        | Func(Defun(name, _, _)) -> name
        | Func(Native(name, _, _)) -> name
        | Func(Lambda(param, _, _)) -> sprintf "<Lambda (%s)>" param
        | Func(Freeze _) -> "<Freeze>"
        | Func(Partial(f, args)) -> sprintf "<Partial %s%s>" (toStr (Func f)) (join args)
        | InStream s -> sprintf "<InStream %s>" (s.ToString())
        | OutStream s -> sprintf "<OutStream %s>" (s.ToString())

    let cons x y = Cons(x, y)

    let uncons v =
        match v with
        | Cons(x, y) -> x, y
        | _ -> failwith "not a Cons"

    let rec toCons list =
        match list with
        | [] -> Empty
        | x :: xs -> Cons(x, toCons xs)

    let rec toListOption cons =
        match cons with
        | Empty -> Some []
        | Cons(x, y) -> Option.map (fun xs -> List.Cons(x, xs)) (toListOption y)
        | _ -> None

    let toList value =
        match toListOption value with
        | Some cons -> cons
        | None -> err "Invalid value in Cons list"

    let arityErr name expected (args: Value list) =
        err(sprintf "%s expected %i arguments, but given %i" name expected args.Length)
    
    let typeErr name (types: string list) =
        if types.IsEmpty
            then err(sprintf "%s expected no arguments" name)
            else err(sprintf "%s expected arguments of type(s): %s" name (String.Join(" ", types)))

open Values

module ExpressionPatterns =
    let (|Expr|_|) = toListOption

    let (|AndExpr|_|) = function
        | Expr [Sym "and"; left; right] -> Some(left, right)
        | _ -> None

    let (|OrExpr|_|) = function
        | Expr [Sym "or"; left; right] -> Some(left, right)
        | _ -> None

    let (|IfExpr|_|) = function
        | Expr [Sym "if"; condition; consequent; alternative] -> Some(condition, consequent, alternative)
        | _ -> None

    let (|CondExpr|_|) = function
        | Expr(Sym "cond" :: clauses) -> Some(List.map uncons clauses)
        | _ -> None

    let (|LetExpr|_|) = function
        | Expr [Sym "let"; Sym symbol; binding; body] -> Some(symbol, binding, body)
        | _ -> None

    let (|LambdaExpr|_|) = function
        | Expr [Sym "lambda"; Sym symbol; body] -> Some(symbol, body)
        | _ -> None

    let (|FreezeExpr|_|) = function
        | Expr [Sym "freeze"; body] -> Some body
        | _ -> None

    let (|TrapExpr|_|) = function
        | Expr [Sym "trap-error"; body; handler] -> Some(body, handler)
        | _ -> None

    let (|DefunExpr|_|) = function
        | Expr [Sym "defun"; Sym name; Expr paramz; body] -> Some(name, List.map vsym paramz, body)
        | _ -> None

    let (|DoExpr|_|) = function
        | Expr(Sym "do" :: exprs) -> Some exprs
        | _ -> None

    let (|AppExpr|_|) = function
        | Expr(f :: args) -> Some(f, args)
        | _ -> None
