module internal Writer

open FSharp.Compiler.Ast

let private join (sep: string) (strings: string list) = System.String.Join(sep, strings |> List.toArray)

let private writeIdent (x: Ident) = if String.forall (System.Char.IsLetter) x.idText then x.idText else sprintf "``%s``" x.idText

let private writeLongIdent (x: LongIdentWithDots) = List.map writeIdent x.Lid |> join "."

let private escapeChar = function
    | x when x < ' ' -> int x |> sprintf "\\u%02x"
    | x when x > '~' -> int x |> sprintf "\\u%04x"
    | x -> sprintf "%c" x

let private writeString x = String.collect escapeChar x |> sprintf "\"%s\""

let private writeConst = function
    | SynConst.Unit -> "()"
    | SynConst.Bool true -> "true"
    | SynConst.Bool false -> "false"
    | SynConst.Int32 x -> sprintf "%i" x
    | SynConst.Decimal x -> sprintf "%Mm" x
    | SynConst.String(x, _) -> writeString x
    | _ -> failwith "SynConst case not supported"

let private writeType = function
    | SynType.LongIdent x -> writeLongIdent x
    | _ -> failwith "SynType case not supported"

let rec private writeSimplePat = function
    | SynSimplePat.Id(ident, _, _, _, _, _) -> writeIdent ident
    | SynSimplePat.Typed(pat, typ, _) -> sprintf "(%s: %s)" (writeSimplePat pat) (writeType typ)
    | _ -> failwith "SynSimplePat case not supported"

let rec private writePat = function
    | SynPat.LongIdent(x, _, _, _, _, _) -> writeLongIdent x
    | SynPat.Named(_, ident, _, _, _) -> writeIdent ident
    | SynPat.Paren(x, _) -> writePat x |> sprintf "(%s)"
    | SynPat.ArrayOrList(false, pats, _) -> List.map writePat pats |> join "; " |> sprintf "[%s]"
    | x -> failwithf "SynPat case not supported: %O" x

let rec private writeExpr = function
    | SynExpr.Paren(x, _, _, _) -> writeExpr x |> sprintf "(%s)"
    | SynExpr.Ident x -> writeIdent x
    | SynExpr.LongIdent(_, x, _, _) -> writeLongIdent x
    | SynExpr.Const(x, _) -> writeConst x
    | SynExpr.Tuple(false, xs, _, _) -> List.map writeExpr xs |> join ", " |> sprintf "(%s)"
    | SynExpr.ArrayOrList(false, xs, _) -> List.map writeExpr xs |> join "; " |> sprintf "[%s]"
    | SynExpr.Sequential(_, _, x, y, _) -> sprintf "(%s; %s)" (writeExpr x) (writeExpr y)
    | SynExpr.LetOrUse(_, _, [SynBinding.Binding(_, _, _, _, _, _, _, pat, _, value, _, _)], body, _) ->
        sprintf "(let %s = %s in %s)" (writePat pat) (writeExpr value) (writeExpr body)
    | SynExpr.IfThenElse(ifExpr, thenExpr, Some elseExpr, _, _, _, _) ->
        sprintf "(if (%s) then (%s) else (%s))" (writeExpr ifExpr) (writeExpr thenExpr) (writeExpr elseExpr)
    | SynExpr.TryWith(body, _, [SynMatchClause.Clause(pat, _, handler, _, _)], _, _, _, _) ->
        sprintf "(try %s; with %s -> %s)" (writeExpr body) (writePat pat) (writeExpr handler)
    | SynExpr.MatchLambda(_, _, clauses, _, _) -> List.map writeClause clauses |> join "; " |> sprintf "(function %s)"
    | SynExpr.Lambda(_, _, SynSimplePats.SimplePats([], _), body, _) -> sprintf "(fun () -> %s)" (writeExpr body)
    | SynExpr.Lambda(_, _, SynSimplePats.SimplePats(pats, _), body, _) -> sprintf "(fun %s -> %s)" (List.map writeSimplePat pats |> join " ") (writeExpr body)
    | SynExpr.App(_, true, f, x, _) -> sprintf "%s %s" (writeExpr x) (writeExpr f)
    | SynExpr.App(_, _, f, x, _) -> sprintf "(%s %s)" (writeExpr f) (writeExpr x)
    | x -> failwithf "SynExpr case not supported: %O" x

and private writeClause = function
    | SynMatchClause.Clause(pat, _, body, _, _) -> sprintf "| %s -> %s" (writePat pat) (writeExpr body)

let private writeBinding = function
    | SynBinding.Binding(_, _, _, _, _, _, _, pat, _, value, _, _) ->
        sprintf "%s = %s" (writePat pat) (writeExpr value)

let private writeDecl = function
    | SynModuleDecl.Open(x, _) -> writeLongIdent x |> sprintf "open %s"
    | SynModuleDecl.Let(recursive, binding :: bindings, _) ->
        sprintf "let%s %s%s" (if recursive then " rec" else "") (writeBinding binding) (List.map (writeBinding >> sprintf "\r\nand %s") bindings |> join "")
    | _ -> failwith "SynModuleDecl case not supported"

let private writeModule = function
    | SynModuleOrNamespace.SynModuleOrNamespace(_, _, _, decls, _, _, _, _) ->
        sprintf "module Shen.Kernel\r\n\r\n%s" (List.map writeDecl decls |> join "\r\n\r\n")

let writeFile = function
    | ParsedInput.ImplFile(
                            ParsedImplFileInput.ParsedImplFileInput(
                                _,
                                _,
                                _,
                                _,
                                _,
                                modules,
                                _)) ->
        List.map writeModule modules |> join "\r\n\r\n"
    | _ -> failwith "ParsedInput case not supported"
