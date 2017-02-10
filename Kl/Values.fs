namespace Kl

open System
open System.Collections.Generic
open System.IO
open System.Text

module Values =

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

    let inRange min max value = min <= value && value < max

    let argsErr name types args =
        if List.length types <> List.length args
            then failwithf "%s expected %i arguments, given %i" name types.Length args.Length
            else failwithf "%s expected arguments of type(s): %s" name (String.Join(", ", types))

    let newGlobals() = {
        Symbols = new Dictionary<string, Value>()
        Functions = new Dictionary<string, Function>()
        Primitives = new HashSet<string>()
    }

    let rec toCons = function
        | [] -> Empty
        | x :: xs -> Cons(x, toCons xs)

    let private sequenceOption xs =
        let combine x xs = Option.bind (fun v -> Option.map (fun vs -> v :: vs) xs) x
        List.foldBack combine xs (Some [])

    let rec private toListOption cons =
        match cons with
        | Empty -> Some []
        | Cons(x, y) -> Option.map (fun xs -> x :: xs) (toListOption y)
        | _ -> None

    let private (|Expr|_|) = toListOption

    let (|AndExpr|_|) = function
        | Expr [Sym "and"; left; right] -> Some(left, right)
        | _ -> None

    let (|OrExpr|_|) = function
        | Expr [Sym "or"; left; right] -> Some(left, right)
        | _ -> None

    let (|IfExpr|_|) = function
        | Expr [Sym "if"; condition; consequent; alternative] -> Some(condition, consequent, alternative)
        | _ -> None

    let private condClause = function
        | Expr [x; y] -> Some(x, y)
        | _ -> None

    let private (|CondClauses|_|) = List.map condClause >> sequenceOption

    let (|CondExpr|_|) = function
        | Expr(Sym "cond" :: CondClauses clauses) -> Some clauses
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

    let private param = function
        | Sym s -> Some s
        | _ -> None

    let private (|ParamList|_|) = toListOption >> Option.bind (List.map param >> sequenceOption)

    let (|DefunExpr|_|) = function
        | Expr [Sym "defun"; Sym name; ParamList paramz; body] -> Some(name, paramz, body)
        | _ -> None

    let (|DoExpr|_|) = function
        | Expr [Sym "do"; first; second] -> Some(first, second)
        | _ -> None

    let (|AppExpr|_|) = function
        | Expr(f :: args) -> Some(f, args)
        | _ -> None

module Extensions =
    type IDictionary<'a, 'b> with
        member this.GetMaybe(key: 'a) =
            match this.TryGetValue(key) with
            | true, x -> Some x
            | false, _ -> None

    let (|Greater|Equal|Lesser|) = function
        | x, y when x > y -> Greater
        | x, y when x < y -> Lesser
        | _ -> Equal

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
