namespace Kl

open System
open Builtins

module Startup =
    
    let private funW name arity f = name, new Function(name, arity, [], f)
    let private funR name arity f = funW name arity (fun globals args -> Done (f globals args))
    let private funV name arity f = funR name arity (fun globals args -> Ok (f globals args))
    // let primitive name arity f = name, new Primitive(name, arity, f)

    let rec install (functions: Defines<'a>) = function
        | [] -> ()
        | (name, value) :: defs ->
            functions.[name] <- value
            install functions defs

    let installBase env =
        install env.Globals.Functions [
            funV "intern"          1 klIntern
            funR "pos"             2 klStringPos
            funV "tlstr"           1 klStringTail
            funV "cn"              2 klStringConcat
            funV "str"             1 klToString
            funV "string?"         1 klIsString
            funV "n->string"       1 klIntToString
            funV "string->n"       1 klStringToInt
            funV "set"             2 klSet
            funR "value"           1 klValue
            funR "simple-error"    1 klSimpleError
            funV "error-to-string" 1 klErrorToString
            funV "cons"            2 klNewCons
            funV "hd"              1 klHead
            funV "tl"              1 klTail
            funV "cons?"           1 klIsCons
            funV "="               2 klEquals
            funV "type"            1 klType
            funR "eval-kl"         1 klEval
            funV "absvector"       1 klNewVector
            funR "<-address"       2 klReadVector
            funR "address->"       3 klWriteVector
            funV "absvector?"      1 klIsVector
            funV "write-byte"      2 klWriteByte
            funV "read-byte"       1 klReadByte
            funR "open"            2 klOpen
            funV "close"           1 klClose
            funV "get-time"        1 klGetTime
            funV "+"               2 klAdd
            funV "-"               2 klSubtract
            funV "*"               2 klMultiply
            funV "/"               2 klDivide
            funV ">"               2 klGreaterThan
            funV "<"               2 klLessThan
            funV ">="              2 klGreaterThanEqual
            funV "<="              2 klLessThanEqual
            funV "number?"         1 klIsNumber
        ]
        let onMono = Type.GetType("Mono.Runtime") <> null
        install env.Globals.Symbols [
            "*language*",       StringValue "F# 3.1"
            "*implementation*", StringValue(if onMono then "Mono" else ".NET")
            "*release*",        StringValue(Environment.Version.ToString())
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
