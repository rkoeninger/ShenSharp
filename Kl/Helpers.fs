﻿namespace Kl

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

    let err s = raise(SimpleError s)

    let thunkw f = Pending(new Thunk(f))

    /// <summary>
    /// Runs Work repeated until a final Value is returned.
    /// </summary>
    let rec go work =
        match work with
        | Pending thunk -> go(thunk.Run())
        | Done result -> result

    let newGlobals() = {Symbols = new Defines<Value>(); Functions = new Defines<Function>()}
    let newEnv globals locals = {Globals = globals; Locals = locals}
    let emptyEnv() = newEnv (newGlobals()) Map.empty

    let vbool v =
        match v with
        | Bool b -> b
        | _ -> err "Boolean expected"

    let vstr v =
        match v with
        | Str s -> s
        | _ -> err "String expected"

    let value2sym v =
        match v with
        | Sym s -> Some s
        | _ -> None

    let list2tuple v =
        match v with
        | Cons(x, Cons(y, Empty)) -> Some(x, y)
        | _ -> None

    let sequenceOption xs =
        let combine x lst =
            match x with
            | None -> None
            | Some v ->
                match lst with
                | None -> None
                | Some vs -> Some(v :: vs)
        List.foldBack combine xs (Some [])

    let rec toCons list =
        match list with
        | [] -> Empty
        | x :: xs -> Cons(x, toCons xs)

    let rec toListOption cons =
        match cons with
        | Empty -> Some []
        | Cons(x, y) -> Option.map (fun xs -> List.Cons(x, xs)) (toListOption y)
        | _ -> None

    let rec butLast xs =
        match xs with
        | [] -> failwith "butLast: empty list"
        | [_] -> []
        | h :: t -> h :: butLast t

open Values

module ExpressionPatterns =
    let (|Expr|_|) = toListOption

    let (|Last|_|) = List.tryLast

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
        | Expr(Sym "cond" :: clauses) -> sequenceOption(List.map list2tuple clauses)
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

    let (|ParamList|_|) = toListOption >> Option.bind (List.map value2sym >> sequenceOption)

    let (|DefunExpr|_|) = function
        | Expr [Sym "defun"; Sym name; ParamList paramz; body] -> Some(name, paramz, body)
        | _ -> None

    let (|DoExpr|_|) = function
        | Expr(Sym "do" :: exprs) -> Some exprs
        | _ -> None

    let (|AppExpr|_|) = function
        | Expr(f :: args) -> Some(f, args)
        | _ -> None
