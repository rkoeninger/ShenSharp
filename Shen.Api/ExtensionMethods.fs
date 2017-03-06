namespace Shen

open System.Runtime.CompilerServices
open Kl
open Shen.Language
open Shen.Runtime

/// <summary>
/// Extension methods to make working with Shen environment
/// look more similiar to a conventional C# API.
/// </summary>
[<Extension>]
type ExtensionMethods() =

    /// <summary>
    /// Evaluates given Shen syntax and returns result as KL Value.
    /// </summary>
    [<Extension>]
    static member Eval(globals: Globals, syntax: string) =
        evalSyntax globals syntax

    /// <summary>
    /// Loads Shen file at given path.
    /// </summary>
    [<Extension>]
    static member Load(globals: Globals, path: string) =
        load globals path

    /// <summary>
    /// Defines a zero-parameter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Globals -> Value) =
        defineFunction0 globals name native

    /// <summary>
    /// Defines a one-paramter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Globals -> Value -> Value) =
        defineFunction1 globals name native

    /// <summary>
    /// Defines a two-paramter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Globals -> Value -> Value -> Value) =
        defineFunction2 globals name native

    /// <summary>
    /// Defines a Shen macro using a native function.
    /// </summary>
    [<Extension>]
    static member DefineMacro(globals: Globals, name: string, native: Value -> Value) =
        defineMacro globals name native
