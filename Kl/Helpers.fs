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
    let (|Greater|Equal|Lesser|) (x, y) =
        if x > y
            then Greater
        elif x < y
            then Lesser
        else Equal
    type Dictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue(key) with
            | true, x -> Some x
            | false, _ -> None
            
module Values =
    let truev = BoolValue true
    let falsev = BoolValue false
    let truew = Done truev
    let falsew = Done falsev

    let err s = raise(SimpleError s)

    let thunkw f = new Thunk<Value>(f) |> Pending

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

    let primitivev name arity f = Primitive(name, arity, f)

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
