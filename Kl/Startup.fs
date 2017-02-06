namespace Kl

open System
open Values
open Builtins

module Startup =

    let private fn name arity f = name, Defun(name, arity, CompiledDefun f)

    let private installBase globals =
        let onMono = Type.GetType "Mono.Runtime" <> null
        let platform = if onMono then "Mono" else "Microsoft.NET"
        let symbols = [
            "*language*",       Str "F# 4.0"
            "*implementation*", Str(sprintf "CLR/%s" platform)
            "*release*",        Str(string Environment.Version)
            "*port*",           Str "0.4"
            "*porters*",        Str "Robert Koeninger"
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
            fn "type"            1 kl_type
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
        ]
        List.iter globals.Symbols.Add symbols
        List.iter globals.Functions.Add functions
        globals

    /// <summary>
    /// Creates a new global scope with the KL primitives installed.
    /// </summary>
    [<CompiledName "BaseGlobals">]
    let baseGlobals() = installBase(newGlobals())
