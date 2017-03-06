namespace Shen

open System.Runtime.CompilerServices
open Kl
open Shen.Language
open Shen.Runtime

[<Extension>]
type ExtensionMethods() =

// TODO: add doc comments

    [<Extension>]
    static member Eval(globals: Globals, syntax: string) =
        evalSyntax globals syntax

    [<Extension>]
    static member Load(globals: Globals, path: string) =
        load globals path

    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Globals -> Value) =
        defineFunction0 globals name native

    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Globals -> Value -> Value) =
        defineFunction1 globals name native

    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Globals -> Value -> Value -> Value) =
        defineFunction2 globals name native

    [<Extension>]
    static member DefineMacro(globals: Globals, name: string, native: Value -> Value) =
        defineMacro globals name native
