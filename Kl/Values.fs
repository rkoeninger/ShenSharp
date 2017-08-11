module Kl.Values

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading

/// <summary>
/// Runs continuation on separate thread with given amount of stack space.
/// </summary>
let separateThread stackSize (f: unit -> unit) =
    let thread = Thread(f, stackSize)
    thread.Start()
    thread.Join()
    0

/// <summary>
/// Runs continuation on separate thread with 16MB of stack space.
/// </summary>
let separateThread16MB = separateThread 16777216

let rec removeAll keys m =
    match keys with
    | [] -> m
    | k :: ks -> removeAll ks (Map.remove k m)

let rec butLast = function
    | [] | [_] -> []
    | x :: xs -> x :: butLast xs

let rec filterSome = function
    | [] -> []
    | Some x :: xs -> x :: filterSome xs
    | None :: xs -> filterSome xs

// Booleans are just these two particular symbols.
let True = Sym "true"
let False = Sym "false"

let Bool b = if b then True else False
let (|Bool|_|) = function
    | x when x = True -> Some true
    | x when x = False -> Some false
    | _ -> None

let asBool = function
    | Bool b -> b
    | _ -> failwith "Conditional must evaluate to boolean"

let Int = decimal >> Num
let (|Int|_|) = function
    | Num x when x % 1.0m = 0.0m -> Some(int x)
    | _ -> None

let asFunction = function
    | Func f -> f
    | _ -> failwith "Operator must be a function"

let rec functionArity = function
    | Interpreted(paramz, _) -> List.length paramz
    | Compiled(arity, _, _) -> arity
    | Partial(f, args) -> functionArity f - args.Length

let inRange min max value = min <= value && value < max

let argsErr name types args =
    if List.length types <> List.length args
        then failwithf "%s expected %i arguments, given %i" name types.Length args.Length
        else failwithf "%s expected arguments of type(s): %s" name (String.Join(", ", types))

let setAlias globals alias original =
    globals.ClrAliases.[alias] <- original
    globals.ClrReverseAliases.[original] <- alias

let newGlobals() =
    let symbols = new ConcurrentDictionary<string, Symbol>()
    let installingKl =
        symbols.GetOrAdd("shen.*installing-kl*",
            fun _ ->
                {Name = "shen.*installing-kl*"
                 Val = ref (Some False)
                 Fun = ref None})
    let globals = {
        InstallingKl = installingKl.Val
        Symbols = new ConcurrentDictionary<string, Symbol>()
        ClrAliases = new ConcurrentDictionary<string, string>()
        ClrReverseAliases = new ConcurrentDictionary<string, string>()
    }
    setAlias globals "object"  typedefof<obj>.FullName
    setAlias globals "string"  typedefof<string>.FullName
    setAlias globals "char"    typedefof<char>.FullName
    setAlias globals "byte"    typedefof<byte>.FullName
    setAlias globals "short"   typedefof<int16>.FullName
    setAlias globals "int"     typedefof<int>.FullName
    setAlias globals "long"    typedefof<int64>.FullName
    setAlias globals "sbyte"   typedefof<sbyte>.FullName
    setAlias globals "ushort"  typedefof<uint16>.FullName
    setAlias globals "uint"    typedefof<uint32>.FullName
    setAlias globals "ulong"   typedefof<uint64>.FullName
    setAlias globals "float"   typedefof<float>.FullName
    setAlias globals "double"  typedefof<double>.FullName
    setAlias globals "decimal" typedefof<decimal>.FullName
    setAlias globals "bool"    typedefof<bool>.FullName
    globals

let funkv f (kv: KeyValuePair<_, _>) = f kv.Key kv.Value

let definedSymbols (globals: Globals) =
    globals.Symbols
    |> Seq.map (funkv (fun k v -> Option.map (fun f -> (k, f)) !v.Val))
    |> Seq.toList
    |> filterSome

let someInterpreted = function
    | Some(Interpreted(paramz, body)) -> Some(paramz, body)
    | _ -> None

let prependTuple x (y, z) = (x, y, z)

let interpretedFunctions (globals: Globals) =
    globals.Symbols
    |> Seq.map (funkv (fun k v -> Option.map (prependTuple k) (someInterpreted (!v.Fun))))
    |> Seq.toList
    |> filterSome

let intern (globals: Globals) id =
    globals.Symbols.GetOrAdd(id,
        fun _ ->
            {Name = id
             Val = ref None
             Fun = ref None})

let getValue symbol =
    match !symbol.Val with
    | Some value -> value
    | None -> failwithf "Symbol \"%s\" is not defined" symbol.Name

let getFunction symbol =
    match !symbol.Fun with
    | Some f -> f
    | None -> failwithf "Function \"%s\" is not defined" symbol.Name

let setValue symbol value =
    let (_, _, sref, _) = symbol
    sref := Some value

let setFunction symbol f =
    let (_, _, _, fref) = symbol
    fref := Some f

let assign globals id value = (intern globals id).Val := Some value

let retrieve globals id = getValue(intern globals id)

let define globals id f = (intern globals id).Fun := Some f

/// <summary>
/// Looks up id in the global function namespace.
/// Raises an error if function not defined.
/// </summary>
let lookup globals id = getFunction(intern globals id)

let localIndex id locals =
    List.tryFindIndex ((=) id) locals |> Option.map (fun x -> locals.Length - x - 1)

let localInsert id locals =
    List.length locals, (id :: locals)

let localLookup x (locals: 'a list) =
    locals.[locals.Length - x - 1]

let rec toCons = function
    | [] -> Empty
    | x :: xs -> Cons(x, toCons xs)

let rec toList = function
    | Empty -> []
    | Cons(x, y) -> x :: toList y
    | _ -> failwith "Malformed list"

let rec klCons = function
    | [] -> Empty
    | x :: xs -> Cons(Sym "cons", Cons(x, Cons(klCons xs, Empty)))

let rec typeSig args result =
    match args with
    | [] -> klCons [Sym "-->"; result]
    | [x] -> klCons [x; Sym "-->"; result]
    | x :: xs -> klCons [x; Sym "-->"; typeSig xs result]

let declareArity name arity =
    toCons [
        Sym "put"
        Sym name
        Sym "arity"
        Num(decimal arity)
        toCons [
            Sym "value"
            Sym "*property-vector*"
        ]
    ]

let declareType name args result = 
    toCons [
        Sym "do"
        declareArity name (List.length args)
        toCons [
            Sym "declare"
            Sym name
            typeSig args result
        ]
    ]

let asObj = function
    | Obj x -> x
    | _ -> failwith "CLR Object expected"

let pipeString (s: string) =
    let stream = new MemoryStream(Encoding.UTF8.GetBytes s)
    Pipe {
        Name = "String"
        Read = stream.ReadByte
        Write = stream.WriteByte
        Close = stream.Close
    }

let private sequenceOption xs =
    let combine x xs = Option.bind (fun v -> Option.map (fun vs -> v :: vs) xs) x
    List.foldBack combine xs (Some [])

let rec private toListOption cons =
    match cons with
    | Empty -> Some []
    | Cons(x, y) -> Option.map (fun xs -> x :: xs) (toListOption y)
    | _ -> None

let (|Form|_|) = toListOption

let private condClause = function
    | Form [x; y] -> Some(x, y)
    | _ -> None

let private (|CondClauses|_|) = List.map condClause >> sequenceOption

let (|CondForm|_|) = function
    | Form(Sym "cond" :: CondClauses clauses) -> Some clauses
    | _ -> None

let private param = function
    | Sym s -> Some s
    | _ -> None

let private (|ParamList|_|) = toListOption >> Option.bind (List.map param >> sequenceOption)

let (|DefunForm|_|) = function
    | Form [Sym "defun"; Sym name; ParamList paramz; body] -> Some(name, paramz, body)
    | _ -> None

type IDictionary<'a, 'b> with
    member this.GetMaybe(key: 'a) =
        match this.TryGetValue key with
        | true, x -> Some x
        | false, _ -> None
