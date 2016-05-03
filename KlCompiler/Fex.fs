namespace KlCompiler

open System.Text.RegularExpressions

type Fex =
    | Tup of Fex list
    | Lst of Fex list
    | SafeId of string
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
        if reg.IsMatch id && not(Set.contains id keywords) then
            sprintf "%s" (id.Replace("@", "_AT_"))
        else
            sprintf "``%s``" (id.Replace("@", "_AT_"))
    let escapeStr (s: string) =
        s.Replace("\n", "\\n")
         .Replace("\r", "\\r")
         .Replace("\t", "\\t")
         .Replace("\"", "\\\"")
    let isIf e =
        match e with
        | If _ -> true
        | _ -> false
    let wrap s = sprintf "(%s)" s
    let rec format fex =
        match fex with
        | Tup exprs -> sprintf "(%s)" (join ", " (List.map (format >> wrap) exprs))
        | Lst exprs -> sprintf "[%s]" (join "; " (List.map (format >> wrap) exprs))
        | Wild -> "_"
        | SafeId id -> id
        | Id id -> escapeId id
        | Boolex b -> if b then "true" else "false"
        | Intex i -> sprintf "%i" i
        | Decex d -> sprintf "%fm" d
        | Strex s -> sprintf "\"%s\"" (escapeStr s)
        | If(c, t, f) -> sprintf "if (%s) then (%s) else (%s)" (format c) (format t) (format f)
        | Infix(op, l, r) -> sprintf "(%s) %s (%s)" (format l) op (format r)
        | App(f, xs) -> sprintf "(%s) %s" (format f) (join " " (List.map (fun x -> "(" + format x + ")") xs))
        | Let(id, v, b) -> sprintf "let %s = (%s) in (%s)" (escapeId id) (format v) (format b)
        | Try(body, handler) -> sprintf "trap (fun () -> (%s)) (fun v -> go(apply Head _globals (new PInfo()) (vfunc(%s)) [v]))" (format body) (format handler)
        | Lamb(arg, typeName, body) -> sprintf "(fun (%s: %s) -> (%s))" arg typeName (format body)
        | Match(expr, clauses) -> sprintf "match %s with %s" (format expr) (join "" (List.map formatClause clauses))
    and formatClause(pat, body) = sprintf " | %s -> (%s)" (format pat) (format body)
    let formatFunction first (name, paramz, body) =
        sprintf
            """    %s %s (_globals: Globals) (_args: Value list) : Value = match _args with | [%s] -> (%s) | _ -> (err "args") """
            (if first then "let rec" else "and")
            (escapeId name)
            (join "; " (List.map escapeId paramz))
            (format body)
    let formatFunctions funs =
        match funs with
        | [] -> []
        | first :: rest -> List.Cons(formatFunction true first, List.map (formatFunction false) rest)
    let formatInit ex = sprintf "        (%s) |> ignore" (format ex)
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