namespace Kl

open System
open System.Collections.Generic
open Values
open Builtins

module Startup =

    let private install (functions: Dictionary<string, 'a>) = List.iter functions.Add

    let private fn name arity f = name, Native(name, arity, f)

    let private installBase globals =
        install globals.Functions [
            fn "if"              3 klIf
            fn "and"             2 klAnd
            fn "or"              2 klOr
            fn "intern"          1 klIntern
            fn "pos"             2 klStringPos
            fn "tlstr"           1 klStringTail
            fn "cn"              2 klStringConcat
            fn "str"             1 klToString
            fn "string?"         1 klIsString
            fn "n->string"       1 klIntToString
            fn "string->n"       1 klStringToInt
            fn "set"             2 klSet
            fn "value"           1 klValue
            fn "simple-error"    1 klSimpleError
            fn "error-to-string" 1 klErrorToString
            fn "cons"            2 klNewCons
            fn "hd"              1 klHead
            fn "tl"              1 klTail
            fn "cons?"           1 klIsCons
            fn "="               2 klEquals
            fn "type"            1 klType
            fn "eval-kl"         1 klEval
            fn "absvector"       1 klNewVector
            fn "<-address"       2 klReadVector
            fn "address->"       3 klWriteVector
            fn "absvector?"      1 klIsVector
            fn "write-byte"      2 klWriteByte
            fn "read-byte"       1 klReadByte
            fn "open"            2 klOpen
            fn "close"           1 klClose
            fn "get-time"        1 klGetTime
            fn "+"               2 klAdd
            fn "-"               2 klSubtract
            fn "*"               2 klMultiply
            fn "/"               2 klDivide
            fn ">"               2 klGreaterThan
            fn "<"               2 klLessThan
            fn ">="              2 klGreaterThanEqual
            fn "<="              2 klLessThanEqual
            fn "number?"         1 klIsNumber
        ]
        let onMono = Type.GetType("Mono.Runtime") <> null
        install globals.Symbols [
            "*language*",       Str "F# 4.0"
            "*implementation*", Str(sprintf "CLR/%s" (if onMono then "Mono" else "Microsoft.NET"))
            "*release*",        Str(string Environment.Version)
            "*port*",           Str "0.4"
            "*porters*",        Str "Robert Koeninger"
            "*stinput*",        console
            "*stoutput*",       console
            // We could set *home-directory* here, but it gets
            // overwritten in the KL distribution of Shen.
        ]
        globals

    /// <summary>
    /// Creates a new global scope with the KL primitives installed.
    /// </summary>
    [<CompiledName "BaseGlobals">]
    let baseGlobals() = installBase(newGlobals())
