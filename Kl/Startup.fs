namespace Kl

open System
open System.Reflection
open Values
open Builtins

module Startup =

    let private fn name arity native = name, Compiled(arity, native)

    let private installBase globals =
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
            "*os*",             Str(Environment.OSVersion.Platform.ToString())
            "*port*",           Str(sprintf "%i.%i" klVersion.Major klVersion.Minor)
            "*porters*",        Str author
            "*stinput*",        console
            "*stoutput*",       console
            "*home-directory*", Str Environment.CurrentDirectory
        ]
        let functions = [
            fn "if"              3 kl_if
            fn "and"             2 kl_and
            fn "or"              2 kl_or
            fn "intern"          1 kl_intern
            fn "pos"             2 kl_pos
            fn "tlstr"           1 kl_tlstr
            fn "cn"              2 kl_cn
            fn "str"             1 kl_str
            fn "string?"         1 ``kl_string?``
            fn "n->string"       1 ``kl_n->string``
            fn "string->n"       1 ``kl_string->n``
            fn "set"             2 kl_set
            fn "value"           1 kl_value
            fn "simple-error"    1 ``kl_simple-error``
            fn "error-to-string" 1 ``kl_error-to-string``
            fn "cons"            2 kl_cons
            fn "hd"              1 kl_hd
            fn "tl"              1 kl_tl
            fn "cons?"           1 ``kl_cons?``
            fn "="               2 ``kl_=``
            fn "type"            2 kl_type
            fn "eval-kl"         1 ``kl_eval-kl``
            fn "absvector"       1 kl_absvector
            fn "<-address"       2 ``kl_<-address``
            fn "address->"       3 ``kl_address->``
            fn "absvector?"      1 ``kl_absvector?``
            fn "write-byte"      2 ``kl_write-byte``
            fn "read-byte"       1 ``kl_read-byte``
            fn "open"            2 kl_open
            fn "close"           1 kl_close
            fn "get-time"        1 ``kl_get-time``
            fn "+"               2 ``kl_+``
            fn "-"               2 ``kl_-``
            fn "*"               2 ``kl_*``
            fn "/"               2 ``kl_/``
            fn ">"               2 ``kl_>``
            fn "<"               2 ``kl_<``
            fn ">="              2 ``kl_>=``
            fn "<="              2 ``kl_<=``
            fn "number?"         1 ``kl_number?``
            fn "exit"            1 kl_exit
            fn "cd"              1 kl_cd
            fn "pwd"             0 kl_pwd
            fn "ls"              0 kl_ls
        ]
        List.iter ((<||) (assign globals)) symbols
        List.iter ((<||) (define globals)) functions
        List.map (fst >> globals.PrimitiveSymbols.Add) symbols |> ignore
        List.map (fst >> globals.PrimitiveFunctions.Add) functions |> ignore
        globals

    /// <summary>
    /// Creates a new global scope with the KL primitives installed.
    /// </summary>
    let baseGlobals() = installBase(newGlobals())
