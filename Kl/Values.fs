namespace Kl

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading

module Values =

    // Runs continuation on separate thread with 16MB of stack space
    let separateThread (f: unit -> unit) =
        let thread = Thread(f, 16777216)
        thread.Start()
        thread.Join()
        0

    // Booleans are just these two particular symbols.
    let True = Sym "true"
    let False = Sym "false"

    let Bool b = if b then True else False
    let (|Bool|_|) = function
        | x when x = True -> Some true
        | x when x = False -> Some false
        | _ -> None

    let isTrue = function
        | Bool b -> b
        | _ -> failwith "Conditional must evaluate to boolean"

    let Int = decimal >> Num
    let (|Int|_|) = function
        | Num x when x % 1.0m = 0.0m -> Some(int x)
        | _ -> None

    let asFunction = function
        | Func f -> f
        | _ -> failwith "Operator must be a function"

    let inRange min max value = min <= value && value < max

    let argsErr name types args =
        if List.length types <> List.length args
            then failwithf "%s expected %i arguments, given %i" name types.Length args.Length
            else failwithf "%s expected arguments of type(s): %s" name (String.Join(", ", types))

    let newGlobals() = new ConcurrentDictionary<string, Symbol>()

    let intern (globals: Globals) id =
        globals.GetOrAdd(id,
            fun _ ->
                {Name = id
                 IsProtected = ref false
                 Val = ref None
                 Func = ref None})

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
        match !symbol.Func with
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
        symbol.Func := Some f
        symbol.IsProtected := true

    let assign globals id value = (intern globals id).Val := Some value

    let retrieve globals id = getValue(intern globals id)

    let define globals id f = (intern globals id).Func := Some f

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

    let pipeString (s: string) =
        let stream = new MemoryStream(Encoding.UTF8.GetBytes s)
        Pipe {
            Name = "String"
            Read = stream.ReadByte
            Write = stream.WriteByte
            Close = stream.Close
        }

    let rec filterSome = function
        | [] -> []
        | Some x :: xs -> x :: filterSome xs
        | None :: xs -> filterSome xs

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

// Console reader is an adapter that buffers input by line to provide
// character stream to Shen REPL in expected format.
type internal ConsoleReader() =
    let reader = new StreamReader(Console.OpenStandardInput())
    let mutable line: byte[] = [||]
    let mutable index = 0
    member this.ReadByte() =
        if index >= line.Length then
            line <- Encoding.ASCII.GetBytes(reader.ReadLine() + Environment.NewLine)
            index <- 0
        index <- index + 1
        int (line.[index - 1])
