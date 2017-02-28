module Kl.Startup

open System
open System.Reflection
open Values
open Builtins

/// <summary>
/// Creates a new global scope with the KL primitives installed.
/// </summary>
let baseGlobals () =
    let onMono = Type.GetType "Mono.Runtime" <> null
    let klAssembly = typedefof<Value>.Assembly
    let klVersion = klAssembly.GetName().Version
    let companyAttribute = klAssembly.GetCustomAttribute<AssemblyCompanyAttribute>()
    let author = if companyAttribute <> null then companyAttribute.Company else "Unknown"
    let fsVersion = typedefof<unit>.Assembly.GetName().Version
    let symbols = [
        "*language*",       Str(sprintf "F# %i.%i" fsVersion.Minor fsVersion.MajorRevision)
        "*implementation*", Str(if onMono then "Mono" else "Microsoft.NET")
        "*release*",        Str(string Environment.Version)
        "*os*",             Str(string Environment.OSVersion.Platform)
        "*port*",           Str(sprintf "%i.%i" klVersion.Major klVersion.Minor)
        "*porters*",        Str author
        "*stinput*",        console
        "*stoutput*",       console
        "*home-directory*", Str Environment.CurrentDirectory
    ]
    let functions = [
        "if",              Compiled(3, kl_if)
        "and",             Compiled(2, kl_and)
        "or",              Compiled(2, kl_or)
        "intern",          Compiled(1, kl_intern)
        "pos",             Compiled(2, kl_pos)
        "tlstr",           Compiled(1, kl_tlstr)
        "cn",              Compiled(2, kl_cn)
        "str",             Compiled(1, kl_str)
        "string?",         Compiled(1, ``kl_string?``)
        "n->string",       Compiled(1, ``kl_n->string``)
        "string->n",       Compiled(1, ``kl_string->n``)
        "set",             Compiled(2, kl_set)
        "value",           Compiled(1, kl_value)
        "simple-error",    Compiled(1, ``kl_simple-error``)
        "error-to-string", Compiled(1, ``kl_error-to-string``)
        "cons",            Compiled(2, kl_cons)
        "hd",              Compiled(1, kl_hd)
        "tl",              Compiled(1, kl_tl)
        "cons?",           Compiled(1, ``kl_cons?``)
        "=",               Compiled(2, ``kl_=``)
        "type",            Compiled(2, kl_type)
        "eval-kl",         Compiled(1, ``kl_eval-kl``)
        "absvector",       Compiled(1, kl_absvector)
        "<-address",       Compiled(2, ``kl_<-address``)
        "address->",       Compiled(3, ``kl_address->``)
        "absvector?",      Compiled(1, ``kl_absvector?``)
        "write-byte",      Compiled(2, ``kl_write-byte``)
        "read-byte",       Compiled(1, ``kl_read-byte``)
        "open",            Compiled(2, kl_open)
        "close",           Compiled(1, kl_close)
        "get-time",        Compiled(1, ``kl_get-time``)
        "+",               Compiled(2, ``kl_+``)
        "-",               Compiled(2, ``kl_-``)
        "*",               Compiled(2, ``kl_*``)
        "/",               Compiled(2, ``kl_/``)
        ">",               Compiled(2, ``kl_>``)
        "<",               Compiled(2, ``kl_<``)
        ">=",              Compiled(2, ``kl_>=``)
        "<=",              Compiled(2, ``kl_<=``)
        "number?",         Compiled(1, ``kl_number?``)
        "exit",            Compiled(1, kl_exit)
        "cd",              Compiled(1, kl_cd)
        "pwd",             Compiled(0, kl_pwd)
        "ls",              Compiled(0, kl_ls)
    ]
    let globals = newGlobals()
    List.iter ((<||) (assign globals)) symbols
    List.iter ((<||) (define globals)) functions
    List.map (fst >> globals.PrimitiveSymbols.Add) symbols |> ignore
    List.map (fst >> globals.PrimitiveFunctions.Add) functions |> ignore
    globals
