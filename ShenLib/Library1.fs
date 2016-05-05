namespace Shen

open FParsec

exception SimpleError of string

type Value =
    | Empty
    | Boolean of bool
    | Symbol of string
    | String of char list
    | Number of decimal
    | Cons of Value * Value
    | Absvector of Value array

type Result<'a> =
    | Ok of 'a
    | Uncaught of string

module Result =
    let map f r =
        match r with
        | Ok x -> Ok(f x)
        | Uncaught _ as e -> e
    let bind f r =
        match r with
        | Ok x -> f x
        | Uncaught _ as e -> e
    let mapError f r =
        match r with
        | Ok _ as o -> o
        | Uncaught e -> f e

type Expr = Unit

type Pattern = Unit

type Clause = (Pattern list * Expr * Expr option) list

type PackageMemberExpr =
    DefineExpr of string * Clause

type RootExpr =
    PackageExpr of string * string list * PackageMemberExpr list

type Function(f: Value list -> Result<Value>) =
    member this.Apply(args: Value list) = f args

type Defines<'a> = System.Collections.Generic.Dictionary<string, 'a>

type Env() =
    member val Symbols = new Defines<Value>() with get
    member val Functions = new Defines<Function>() with get
    member val Properties = new Defines<Value>() with get
    member val ArityTable = new Defines<int>() with get
    member val TypeChecking = false with get, set
    member val GenSymCounter = 0 with get, set
    member val History: Value list = [] with get, set

module Ascii =
    let tab = 9
    let newline = 10
    let carriageReturn = 13
    let space = 32
    let exclamation = 33
    let percent = 37
    let leftRound = 40
    let hat = 94

module Core =
    let thaw(f: Function) = f.Apply []
    let rec isPrefix xs ys =
        match xs with
        | [] -> false
        | x :: xs ->
            match ys with
            | y :: ys when x = y -> isPrefix xs ys
            | _ -> false
    let isAbsvector v =
        match v with
        | Absvector _ -> true
        | _ -> false
    let isEmpty v =
        match v with
        | Empty -> true
        | _ -> false
    let isBoolean v =
        match v with
        | Boolean _ -> true
        | _ -> false
    let isNumber v =
        match v with
        | Number _ -> true
        | _ -> false
    let isString v =
        match v with
        | String _ -> true
        | _ -> false
    let isSymbol v =
        match v with
        | Symbol _ -> true
        | _ -> false
    let isCons v =
        match v with
        | Cons _ -> true
        | _ -> false
    let createVector n =
        let array = Array.create<Value>(n + 1)(Symbol "shen.fail!")
        array.[0] <- Number(decimal n)
        Absvector array
    let isVector v =
        match v with
        | Absvector array -> not(Array.isEmpty array)
        | _ -> false
    let setVector (array: Value array) index value =
        if index <= 0 || index >= array.Length
            then Uncaught ""
            else
                array.[index] <- value
                Ok value
    let vectorLimit (array: Value array) = array.[0]
    let isNonEmptyVector (array: Value array) = array.Length > 0
    let isPositiveInt v =
        match v with
        | Number n -> n % 1m = 0m && n > 0m
        | _ -> false
    let rec append left right =
        match left with
        | [] -> right
        | x :: xs -> Cons(x, append xs right)
    let rec consMap f c =
        match c with
        | Empty -> Empty
        | Cons(x, y) -> Cons(f x, consMap f y)
        | _ -> failwith ""
    let inline isUppercase ch = 'A' <= ch && ch <= 'Z'
    let inline digitChar ch = '0' <= ch && ch <= '9'
    let inline alphaChar ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')
    let inline alphaNumChar ch = digitChar ch || alphaChar ch
    let inline alphaNum s = String.forall alphaNumChar s
    let inline isVariable s = if s = "" then false else (isUppercase s.[0]) && (alphaNum s)
    let inline gensym (env: Env) s =
        let i = env.GenSymCounter
        env.GenSymCounter <- i + 1
        Symbol(s + (string i))
    let protect = id
    
module Yacc =
    let inline cons1 x = Cons(x, Empty)
    let inline cons2 x y = Cons(x, Cons(y, Empty))
    let inline cons3 x y z = Cons(x, Cons(y, Cons(z, Empty)))
    let inline cons4 x y z w = Cons(x, Cons(y, Cons(z, Cons(w, Empty))))
    let syntax code stream guardExpr : Value =
        match code, stream, guardExpr with
        | Empty, _, Cons(Symbol "where", Cons(guard, Cons(semantics, Empty))) -> Empty
        | Empty, _, semantics -> Empty
        | Cons(first, rest), _, semantics -> Empty
        | _ -> failwith ""
    let isTerminal code =
        match code with
        | Symbol s -> not(Core.isVariable s)
        | _ -> true
    let isJumpStream code =
        match code with
        | Symbol "_" -> true
        | _ -> false
    let stripPathName (s: string) =
        let dotIndex = s.LastIndexOf('.')
        if dotIndex >= 0
            then s.Substring dotIndex
            else s
    let isGrammarSymbol code =
        match code with
        | Symbol s ->
            let id = stripPathName s
            id.[0] = '<' && id.[id.Length - 1] = '>'
        | _ -> false
    let rec semantics v =
        match v with
        | Empty -> Empty
        | Symbol s as sym ->
            if isGrammarSymbol sym then Cons(Symbol "hdtl", Cons(Symbol("Parse_" + s), Empty))
            elif Core.isVariable s then Symbol("Parse_" + s)
            else sym
        | Cons _ -> Core.consMap semantics v
        | x -> x
    let checkStream ss stream semans =
        match ss with
        | Cons(_, syn) ->
            let test =
                cons3
                    (Symbol "and")
                    (cons2 (Symbol "cons?") (cons2 (Symbol "hd") stream))
                    (cons3 (Symbol "=") (Symbol "S") (cons2 (Symbol "hd") (cons2 (Symbol "hd") stream)))
            let action =
                syntax syn
                    (cons3 (Symbol "pair") (cons2 (Symbol "tl") (cons2 (Symbol "hd") stream))
                    (cons2 (Symbol "hdtl") stream))
                    semans
            let el = cons1(Symbol "fail")
            cons4 (Symbol "if") test action el
        | _ -> failwith ""
    let jumpStream ss stream semans =
        match ss with
        | Cons(_, syn) ->
            let test = cons2 (Symbol "cons?") (cons2 (Symbol "hd") stream)
            let action =
                syntax syn
                    (cons3 (Symbol "pair") (cons2 (Symbol "tl") (cons2 (Symbol "hd") stream))
                    (cons2 (Symbol "hdtl") stream))
                    semans
            let el = cons1(Symbol "fail")
            cons4 (Symbol "if") test action el
        | _ -> failwith ""

module TStar =
    let isSpecial x =
        List.contains x [
            Symbol "@p"
            Symbol "@s"
            Symbol "@v"
            Symbol "cons"
            Symbol "lambda"
            Symbol "let"
            Symbol "where"
            Symbol "set"
            Symbol "open"
        ]
    let isExtraSpecial x =
        List.contains x [
            Symbol "define"
            Symbol "process-datatype"
            Symbol "input+"
            Symbol "defcc"
            Symbol "read+"
            Symbol "defmacro"
        ]
    let rec curry code =
        match code with
        | Cons(f, x) when isSpecial f -> Cons(f, Core.consMap curry x)
        | Cons(d, Cons(f, x)) when isExtraSpecial d -> Cons(d, Cons(f, x))
        | Cons(Symbol "type", Cons(x, Cons(a, Empty))) -> Cons(Symbol "type", Cons(curry x, Cons(a, Empty)))
        | Cons(f, Cons(x, Cons(y, z))) -> curry (Cons(Cons(f, Cons(x, Empty)), Cons(y, z)))
        | Cons(f, Cons(x, Empty)) -> Cons(curry f, Cons(curry x, Empty))
        | x -> x

module Types =
    let declare env sym = ()

module TopLevel =
    let destroy env sym = Types.declare env sym
    let initEnv (env: Env) =
        env.Symbols.["*call*"] <- Number 0m
        env.Symbols.["*infs*"] <- Number 0m
        env.Symbols.["*process-counter*"] <- Number 0m
        env.Symbols.["*catch*"] <- Number 0m
    let prompt (env: Env) =
        printf "(%i%s) " (env.History.Length) (if env.TypeChecking then "+" else "-")
    let rec repl env = // Return false to exit
        initEnv env
        prompt env
        try ()
        with SimpleError e -> printfn "%s" e
        repl env
    let printCredits() =
        let onMono = System.Type.GetType("Mono.Runtime") <> null
        printfn "Shen, copyright (C) 2010-2015 Mark Tarver"
        printfn "www.shenlanguage.com, Shen 19.2"
        printfn "running under CLR/%s, implementation %s" (if onMono then "Mono" else "Microsoft.NET") "4.5"
        printfn "port 0.1, ported by Robert Koeninger"
        printfn ""
    let shen env =
        printCredits()
        repl env
        