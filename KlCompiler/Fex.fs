namespace KlCompiler

open System.Text.RegularExpressions

type Fex =
    | Tup of Fex list
    | Lst of Fex list
    | Id of string
    | Boolex of bool
    | Intex of int
    | Decex of decimal
    | Strex of string
    | If of Fex * Fex * Fex
    | Infix of string * Fex * Fex
    | App of Fex * Fex list
    | Let of string * Fex * Fex
    | Try of Fex * Fex
    | Lamb of string * string * Fex
    | Match of Fex * (Fex * Fex) list
    | Wild

module FexFormat =
    let keywords =
        Set.ofList [
            "abstract"
            "and"
            "as"
            "assert"
            "base"
            "begin"
            "class"
            "default"
            "delegate"
            "do"
            "done"
            "downcast"
            "downto"
            "elif"
            "else"
            "end"
            "exception"
            "extern"
            "false"
            "finally"
            "for"
            "fun"
            "function"
            "global"
            "if"
            "in"
            "inherit"
            "inline"
            "interface"
            "internal"
            "lazy"
            "let"
            "let!"
            "match"
            "member"
            "module"
            "mutable"
            "namespace"
            "new"
            "not"
            "null"
            "of"
            "open"
            "or"
            "override"
            "private"
            "public"
            "rec"
            "return"
            "return!"
            "select"
            "static"
            "struct"
            "then"
            "to"
            "true"
            "try"
            "type"
            "upcast"
            "use"
            "use!"
            "val"
            "void"
            "when"
            "while"
            "with"
            "yield"
            "yield!"
            "atomic"
            "break"
            "checked"
            "component"
            "const"
            "constraint"
            "constructor"
            "continue"
            "eager"
            "event"
            "external"
            "fixed"
            "functor"
            "include"
            "method"
            "mixin"
            "object"
            "parallel"
            "process"
            "protected"
            "pure"
            "sealed"
            "tailcall"
            "trait"
            "virtual"
            "volatile"
            "asr"
            "land"
            "lor"
            "lsl"
            "lsr"
            "lxor"
            "mod"
            "sig"
        ]
    let join s (xs: string list) = System.String.Join(s, xs)
    let reg = new Regex("^[a-zA-Z_][a-zA-Z0-9_]*$")
    let escapeId (id: string) =
        let id = id.Replace("@", "_AT_")
        if reg.IsMatch id && not(Set.contains id keywords) then
            sprintf "%s" id
        else
            sprintf "``%s``" id
    let escapeStr (s: string) =
        s.Replace("\n", "\\n")
         .Replace("\r", "\\r")
         .Replace("\t", "\\t")
         .Replace("\"", "\\\"")
    let isIf e =
        match e with
        | If _ -> true
        | _ -> false
    let wrap (a, s) = sprintf (if a then "%s" else "(%s)") s
    let rec format fex =
        match fex with
        | Tup exprs -> true, sprintf "(%s)" (join ", " (List.map formatw exprs))
        | Lst exprs -> true, sprintf "[%s]" (join "; " (List.map formatw exprs))
        | Wild -> true, "_"
        | Id id -> true, escapeId id
        | Boolex b -> true, if b then "true" else "false"
        | Intex i -> true, sprintf "%i" i
        | Decex d -> true, sprintf "%fm" d
        | Strex s -> true, sprintf "\"%s\"" (escapeStr s)
        | If(c, t, f) -> false, sprintf "if %s then %s else %s" (formatw c) (formatw t) (formatw f)
        | Infix(op, l, r) -> false, sprintf "%s %s %s" (formatw l) op (formatw r)
        | App(f, xs) -> false, sprintf "%s %s" (formatw f) (join " " (List.map formatw xs))
        | Let(id, v, b) -> false, sprintf "let %s = %s in %s" (escapeId id) (formatw v) (formatw b)
        | Try(body, handler) -> false, sprintf "trap (fun () -> %s) (fun v -> go(apply Head _globals (new PInfo()) (vfunc %s) [v]))" (formatw body) (formatw handler)
        | Lamb(arg, typeName, body) -> false, sprintf "fun (%s: %s) -> %s" arg typeName (formatw body)
        | Match(expr, clauses) -> false, sprintf "match %s with %s" (formatw expr) (join "" (List.map formatClause clauses))
    and formatClause(pat, body) = sprintf " | %s -> %s" (formatw pat) (formatw body)
    and formatw = format >> wrap
    and formatnow = snd
    let formatFunction first (name, paramz, body) =
        sprintf
            "    %s %s (_globals: Globals) (_args: Value list) : Value = match _args with | [%s] -> %s | _ -> (err \"args\")"
            (if first then "let rec" else "and")
            (escapeId name)
            (join "; " (List.map (fun id -> escapeId("_" + id)) paramz))
            (formatw body)
    let formatFunctions funs =
        match funs with
        | [] -> []
        | first :: rest -> List.Cons(formatFunction true first, List.map (formatFunction false) rest)
    let formatInit ex = sprintf "        %s |> ignore" (formatw ex)
    let formatModule funs inits =
        sprintf
            """namespace Shen
open Kl
open Kl.Builtins
open Kl.Evaluator
open Kl.Values
module Core =
%s
    let _initEnv (_globals: Globals) =
%s
        ()"""
            (join "\r\n" (formatFunctions funs))
            (join "\r\n" (List.map formatInit inits))