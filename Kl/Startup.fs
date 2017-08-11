module Kl.Startup

open System
open System.Reflection
open Values
open Evaluator
open Builtins
open ShenSharp.Shared

let private def globals (name, (arity, f)) =
    define globals name (Compiled(arity, f, ["Kl"; "Builtins"; "kl_" + name]))

/// <summary>
/// Creates a new global scope with the KL primitives installed.
/// </summary>
let baseGlobals () =
    let onMono = Type.GetType "Mono.Runtime" <> null
    let fsVersion = typedefof<unit>.Assembly.GetName().Version
    let symbols = [
        "*language*",       Str(sprintf "F# %i.%i" fsVersion.Minor fsVersion.MajorRevision)
        "*implementation*", Str(if onMono then "Mono" else "Microsoft.NET")
        "*release*",        Str(string Environment.Version)
        "*os*",             Str(string Environment.OSVersion.Platform)
        "*port*",           Str Revision
        "*porters*",        Str Author
        "*stinput*",        console
        "*stoutput*",       console
        "*home-directory*", Str Environment.CurrentDirectory
    ]
    let functions = [
        "if",                           (3, kl_if)
        "and",                          (2, kl_and)
        "or",                           (2, kl_or)
        "intern",                       (1, kl_intern)
        "pos",                          (2, kl_pos)
        "tlstr",                        (1, kl_tlstr)
        "cn",                           (2, kl_cn)
        "str",                          (1, kl_str)
        "string?",                      (1, ``kl_string?``)
        "n->string",                    (1, ``kl_n->string``)
        "string->n",                    (1, ``kl_string->n``)
        "set",                          (2, kl_set)
        "value",                        (1, kl_value)
        "simple-error",                 (1, ``kl_simple-error``)
        "error-to-string",              (1, ``kl_error-to-string``)
        "cons",                         (2, kl_cons)
        "hd",                           (1, kl_hd)
        "tl",                           (1, kl_tl)
        "cons?",                        (1, ``kl_cons?``)
        "=",                            (2, ``kl_=``)
        "type",                         (2, kl_type)
        "eval-kl",                      (1, ``kl_eval-kl``)
        "absvector",                    (1, kl_absvector)
        "<-address",                    (2, ``kl_<-address``)
        "address->",                    (3, ``kl_address->``)
        "absvector?",                   (1, ``kl_absvector?``)
        "write-byte",                   (2, ``kl_write-byte``)
        "read-byte",                    (1, ``kl_read-byte``)
        "open",                         (2, kl_open)
        "close",                        (1, kl_close)
        "get-time",                     (1, ``kl_get-time``)
        "+",                            (2, ``kl_+``)
        "-",                            (2, ``kl_-``)
        "*",                            (2, ``kl_*``)
        "/",                            (2, ``kl_/``)
        ">",                            (2, ``kl_>``)
        "<",                            (2, ``kl_<``)
        ">=",                           (2, ``kl_>=``)
        "<=",                           (2, ``kl_<=``)
        "number?",                      (1, ``kl_number?``)
        "exit",                         (1, kl_exit)
        "cd",                           (1, kl_cd)
        "clr.alias",                    (2, ``kl_clr.alias``)
        "clr.unbox",                    (1, ``kl_clr.unbox``)
        "clr.null",                     (0, ``kl_clr.null``)
        "clr.int",                      (1, ``kl_clr.int``)
        "clr.double",                   (1, ``kl_clr.double``)
        "clr.decimal",                  (1, ``kl_clr.decimal``)
        "clr.string",                   (1, ``kl_clr.string``)
        "clr.bool",                     (1, ``kl_clr.bool``)
        "clr.reference",                (1, ``kl_clr.reference``)
        "clr.new",                      (2, ``kl_clr.new``)
        "clr.get",                      (2, ``kl_clr.get``)
        "clr.set",                      (3, ``kl_clr.set``)
        "clr.get-index",                (2, ``kl_clr.get-index``)
        "clr.set-index",                (3, ``kl_clr.set-index``)
        "clr.get-static",               (2, ``kl_clr.get-static``)
        "clr.set-static",               (3, ``kl_clr.set-static``)
        "clr.invoke",                   (3, ``kl_clr.invoke``)
        "clr.invoke-static",            (3, ``kl_clr.invoke-static``)
        "shen-sharp.globals",           (0, ``kl_shen-sharp.globals``)
        "shen-sharp.http-post",         (2, ``kl_shen-sharp.http-post``)
        "shen-sharp.curl",              (1, ``kl_shen-sharp.curl``)
        "shen-sharp.save-application",  (2, ``kl_shen-sharp.save-application``)
    ]
    let globals = newGlobals()
    List.iter ((<||) (assign globals)) symbols
    List.iter (def globals) functions
    globals

/// <summary>
/// Declarations to be run after kernel is loaded.
/// </summary>
let postImport globals =
    let declarations = [
        declareArity "clr.alias"         2
        declareArity "clr.unbox"         1
        declareArity "clr.null"          0
        declareArity "clr.int"           1
        declareArity "clr.double"        1
        declareArity "clr.decimal"       1
        declareArity "clr.string"        1
        declareArity "clr.bool"          1
        declareArity "clr.reference"     1
        declareArity "clr.new"           2
        declareArity "clr.get"           2
        declareArity "clr.set"           3
        declareArity "clr.get-index"     2
        declareArity "clr.set-index"     3
        declareArity "clr.get-static"    2
        declareArity "clr.set-static"    3
        declareArity "clr.invoke"        3
        declareArity "clr.invoke-static" 3
        declareType "shen-sharp.http-post" [Sym "string"; Sym "string"] (Sym "string")
        declareType "shen-sharp.curl" [Sym "string"] (Sym "string")
        declareType "shen-sharp.save-application" [Sym "symbol"; Sym "string"] (Sym "string")
    ]
    List.iter (eval globals >> ignore) declarations
    globals
