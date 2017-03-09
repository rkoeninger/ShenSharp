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
    | Compiled(arity, _) -> arity
    | Partial(f, args) -> functionArity f - args.Length

let inRange min max value = min <= value && value < max

let argsErr name types args =
    if List.length types <> List.length args
        then failwithf "%s expected %i arguments, given %i" name types.Length args.Length
        else failwithf "%s expected arguments of type(s): %s" name (String.Join(", ", types))

let newGlobals() = new ConcurrentDictionary<string, Symbol>()

let nonPrimitiveSymbols (globals: Globals) =
    let ps (kv: KeyValuePair<_, _>) =
        if !kv.Value.IsProtected
            then None
            else Option.map (fun value -> (kv.Key, value)) !kv.Value.Val
    filterSome(Seq.toList(Seq.map ps globals))

let nonPrimitiveFunctions (globals: Globals) =
    let pf (kv: KeyValuePair<_, _>) =
        if !kv.Value.IsProtected
            then None
            else Option.map (fun f -> (kv.Key, f)) !kv.Value.Fun
    filterSome(Seq.toList(Seq.map pf globals))

let intern (globals: Globals) id =
    globals.GetOrAdd(id,
        fun _ ->
            {Name = id
             IsProtected = ref false
             Val = ref None
             Fun = ref None})

let unprotectAll (globals: Globals) =
    for kv in globals do
        kv.Value.IsProtected := false
    globals

let getValueOption symbol = !symbol.Val

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

let assignProtected globals id value =
    let symbol = intern globals id
    symbol.Val := Some value
    symbol.IsProtected := true

let defineProtected globals id f =
    let symbol = intern globals id
    symbol.Fun := Some f
    symbol.IsProtected := true

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
