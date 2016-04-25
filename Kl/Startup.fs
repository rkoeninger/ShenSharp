namespace Kl

open System
open Builtins

module Startup =
    
    let rec install (functions: Defines<'a>) = function
        | [] -> ()
        | (name, value) :: defs ->
            functions.[name] <- value
            install functions defs

    let private funv name arity f = name, Values.primitivev name arity f
    let private funr name arity f = name, Values.primitiver name arity f

    let installBase env =
        install env.Globals.Functions [
            funv "intern"          1 klIntern
            funr "pos"             2 klStringPos
            funv "tlstr"           1 klStringTail
            funv "cn"              2 klStringConcat
            funv "str"             1 klToString
            funv "string?"         1 klIsString
            funv "n->string"       1 klIntToString
            funv "string->n"       1 klStringToInt
            funv "set"             2 klSet
            funr "value"           1 klValue
            funr "simple-error"    1 klSimpleError
            funv "error-to-string" 1 klErrorToString
            funv "cons"            2 klNewCons
            funv "hd"              1 klHead
            funv "tl"              1 klTail
            funv "cons?"           1 klIsCons
            funv "="               2 klEquals
            funv "type"            1 klType
            funr "eval-kl"         1 klEval
            funv "absvector"       1 klNewVector
            funr "<-address"       2 klReadVector
            funr "address->"       3 klWriteVector
            funv "absvector?"      1 klIsVector
            funv "write-byte"      2 klWriteByte
            funv "read-byte"       1 klReadByte
            funr "open"            2 klOpen
            funv "close"           1 klClose
            funr "get-time"        1 klGetTime
            funv "+"               2 klAdd
            funv "-"               2 klSubtract
            funv "*"               2 klMultiply
            funv "/"               2 klDivide
            funv ">"               2 klGreaterThan
            funv "<"               2 klLessThan
            funv ">="              2 klGreaterThanEqual
            funv "<="              2 klLessThanEqual
            funv "number?"         1 klIsNumber
        ]
        let onMono = Type.GetType("Mono.Runtime") <> null
        let ver = Environment.Version
        install env.Globals.Symbols [
            "*language*",       StringValue "F# 3.1"
            "*implementation*", StringValue(if onMono then "Mono" else ".NET")
            "*release*",        StringValue(sprintf "%i.%i" ver.Major ver.Minor)
            "*version*",        StringValue "19.2"
            "*port*",           StringValue "0.1"
            "*porters*",        StringValue "Robert Koeninger"
            "*stinput*",        stinput
            "*stoutput*",       stoutput
            // TODO: *home-directory* is bound to a string which denotes the directory
            // relative to which all files are read or written.
            //
            // Gets overwritten in KL
        ]
        env

    let baseEnv() = installBase <| Values.newEnv()
