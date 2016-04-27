namespace Kl

open System
open Builtins

module Startup =
    
    let rec install (functions: Defines<'a>) defs =
        match defs with
        | [] -> ()
        | (name, value) :: rest ->
            functions.[name] <- value
            install functions rest

    let private fn name arity f = name, Values.primitivev name arity f

    let installBase env =
        install env.Globals.Functions [
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
        let clrImpl = if onMono then "Mono" else "Microsoft.NET"
        // TODO: identify CoreCLR?
        let ver = Environment.Version
        install env.Globals.Symbols [
            "*language*",       Str "F# 3.1"
            "*implementation*", Str(sprintf "CLR/%s" clrImpl)
            "*release*",        Str(sprintf "%i.%i" ver.Major ver.Minor)
            "*version*",        Str "19.2"
            "*port*",           Str "0.1"
            "*porters*",        Str "Robert Koeninger"
            "*stinput*",        stinput
            "*stoutput*",       stoutput
            // TODO: *home-directory* is bound to a string which denotes the directory
            // relative to which all files are read or written.
            //
            // Gets overwritten in KL
        ]
        env

    let baseEnv() = installBase <| Values.newEnv()
