module internal Writer

open FSharp.Compiler.Ast

let private join (sep: string) (strings: string list) = System.String.Join(sep, strings |> List.toArray)

let private writeIdent (x: Ident) = x.idText // TODO: escape with ``

let private writeConst = function
    | SynConst.Unit -> "()"
    | SynConst.Bool true -> "true"
    | SynConst.Bool false -> "false"
    | SynConst.Int32 x -> sprintf "%i" x
    | SynConst.Decimal x -> sprintf "%Mm" x
    | SynConst.String(x, _) -> sprintf "\"%s\"" x // TODO: escape chars
    | _ -> failwith "SynConst case not supported"

let private writeType = function
    | SynType.LongIdent x -> List.map writeIdent x.Lid |> join "." // TODO: repeated
    | _ -> failwith "SynType case not supported"

let rec private writeSimplePat = function
    | SynSimplePat.Id(ident, _, _, _, _, _) -> writeIdent ident
    | SynSimplePat.Typed(pat, typ, _) -> sprintf "(%s: %s)" (writeSimplePat pat) (writeType typ)
    | _ -> failwith "SynSimplePat case not supported"

let rec private writePat = function
    | SynPat.Named(_, ident, _, _, _) -> writeIdent ident
    | SynPat.ArrayOrList(true, pats, _) -> List.map writePat pats |> join "; " |> sprintf "[%s]"
    | _ -> failwith "SynPat case not supported"

let rec private writeExpr = function
    | SynExpr.Paren(x, _, _, _) -> writeExpr x |> sprintf "(%s)"
    | SynExpr.Ident x -> writeIdent x
    | SynExpr.LongIdent(_, x, _, _) -> List.map writeIdent x.Lid |> join "."
    | SynExpr.Const(x, _) -> writeConst x
    | SynExpr.Tuple(false, xs, _, _) -> List.map writeExpr xs |> join ", " |> sprintf "(%s)"
    | SynExpr.ArrayOrList(true, xs, _) -> List.map writeExpr xs |> join "; " |> sprintf "[%s]"
    | SynExpr.Sequential(_, _, x, y, _) -> sprintf "(%s; %s)" (writeExpr x) (writeExpr y)
    | SynExpr.LetOrUse(_, _, [SynBinding.Binding(_, _, _, _, _, _, _, SynPat.Named(_, ident, _, _, _), _, value, _, _)], body, _) ->
        sprintf "(let %s = %s in %s)" (writeIdent ident) (writeExpr value) (writeExpr body)
    | SynExpr.IfThenElse(ifExpr, thenExpr, Some elseExpr, _, _, _, _) ->
        sprintf "(if (%s) then (%s) else (%s))" (writeExpr ifExpr) (writeExpr thenExpr) (writeExpr elseExpr)
    | SynExpr.TryWith(body, _, [SynMatchClause.Clause(SynPat.Named(_, ident, _, _, _), _, handler, _, _)], _, _, _, _) ->
        sprintf "(try %s; with %s -> %s)" (writeExpr body) (writeIdent ident) (writeExpr handler)
    | SynExpr.MatchLambda(_, _, clauses, _, _) -> List.map writeClause clauses |> join "; " |> sprintf "(function %s)"
    | SynExpr.Lambda(_, _, SynSimplePats.SimplePats([], _), body, _) -> sprintf "(fun () -> %s)" (writeExpr body)
    | SynExpr.Lambda(_, _, SynSimplePats.SimplePats(pats, _), body, _) -> sprintf "(fun %s -> %s)" (List.map writeSimplePat pats |> join " ") (writeExpr body)
    | SynExpr.App(_, true, f, x, _) -> sprintf "%s %s" (writeExpr x) (writeExpr f)
    | SynExpr.App(_, _, f, x, _) -> sprintf "(%s %s)" (writeExpr f) (writeExpr x)
    | _ -> failwith "SynExpr case not supported"

and private writeClause = function
    | SynMatchClause.Clause(pat, _, body, _, _) -> sprintf "| %s -> %s" (writePat pat) (writeExpr body)

let private writeDecl = function
    | SynModuleDecl.Open(longDotId, _) -> List.map writeIdent longDotId.Lid |> join "." |> sprintf "open %s"
    | SynModuleDecl.Let(recursive, bindings, _) -> "" // TODO: put each on its own line
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
