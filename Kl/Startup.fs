module Kl.Startup

open System
open System.Reflection
open Values
open Builtins
open ShenSharp.Shared

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
        "if",                           Compiled(3, kl_if)
        "and",                          Compiled(2, kl_and)
        "or",                           Compiled(2, kl_or)
        "intern",                       Compiled(1, kl_intern)
        "pos",                          Compiled(2, kl_pos)
        "tlstr",                        Compiled(1, kl_tlstr)
        "cn",                           Compiled(2, kl_cn)
        "str",                          Compiled(1, kl_str)
        "string?",                      Compiled(1, ``kl_string?``)
        "n->string",                    Compiled(1, ``kl_n->string``)
        "string->n",                    Compiled(1, ``kl_string->n``)
        "set",                          Compiled(2, kl_set)
        "value",                        Compiled(1, kl_value)
        "simple-error",                 Compiled(1, ``kl_simple-error``)
        "error-to-string",              Compiled(1, ``kl_error-to-string``)
        "cons",                         Compiled(2, kl_cons)
        "hd",                           Compiled(1, kl_hd)
        "tl",                           Compiled(1, kl_tl)
        "cons?",                        Compiled(1, ``kl_cons?``)
        "=",                            Compiled(2, ``kl_=``)
        "type",                         Compiled(2, kl_type)
        "eval-kl",                      Compiled(1, ``kl_eval-kl``)
        "absvector",                    Compiled(1, kl_absvector)
        "<-address",                    Compiled(2, ``kl_<-address``)
        "address->",                    Compiled(3, ``kl_address->``)
        "absvector?",                   Compiled(1, ``kl_absvector?``)
        "write-byte",                   Compiled(2, ``kl_write-byte``)
        "read-byte",                    Compiled(1, ``kl_read-byte``)
        "open",                         Compiled(2, kl_open)
        "close",                        Compiled(1, kl_close)
        "get-time",                     Compiled(1, ``kl_get-time``)
        "+",                            Compiled(2, ``kl_+``)
        "-",                            Compiled(2, ``kl_-``)
        "*",                            Compiled(2, ``kl_*``)
        "/",                            Compiled(2, ``kl_/``)
        ">",                            Compiled(2, ``kl_>``)
        "<",                            Compiled(2, ``kl_<``)
        ">=",                           Compiled(2, ``kl_>=``)
        "<=",                           Compiled(2, ``kl_<=``)
        "number?",                      Compiled(1, ``kl_number?``)
        "exit",                         Compiled(1, kl_exit)
        "cd",                           Compiled(1, kl_cd)
        "clr.alias",                    Compiled(2, ``kl_clr.alias``)
        "clr.unbox",                    Compiled(1, ``kl_clr.unbox``)
        "clr.null",                     Compiled(0, ``kl_clr.null``)
        "clr.int",                      Compiled(1, ``kl_clr.int``)
        "clr.double",                   Compiled(1, ``kl_clr.double``)
        "clr.decimal",                  Compiled(1, ``kl_clr.decimal``)
        "clr.string",                   Compiled(1, ``kl_clr.string``)
        "clr.bool",                     Compiled(1, ``kl_clr.bool``)
        "clr.new",                      Compiled(2, ``kl_clr.new``)
        "clr.get",                      Compiled(2, ``kl_clr.get``)
        "clr.set",                      Compiled(3, ``kl_clr.set``)
        "clr.get-index",                Compiled(2, ``kl_clr.get-index``)
        "clr.set-index",                Compiled(3, ``kl_clr.set-index``)
        "clr.get-static",               Compiled(2, ``kl_clr.get-static``)
        "clr.set-static",               Compiled(3, ``kl_clr.set-static``)
        "clr.invoke",                   Compiled(3, ``kl_clr.invoke``)
        "clr.invoke-static",            Compiled(3, ``kl_clr.invoke-static``)
        "shen-sharp.globals",           Compiled(0, ``kl_shen-sharp.globals``)
        "shen-sharp.open-socket",       Compiled(1, ``kl_shen-sharp.open-socket``)
        "shen-sharp.download",          Compiled(1, ``kl_shen-sharp.download``)
    ]
    let globals = newGlobals()
    List.iter ((<||) (assignProtected globals)) symbols
    List.iter ((<||) (defineProtected globals)) functions
    globals
