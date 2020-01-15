namespace Shen

open System
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
                                 native: Func<Value>) =
        defineFunction0 globals name (fun _ -> native.Invoke())

    /// <summary>
    /// Defines a one-parameter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Value, Value>) =
        defineFunction1 globals name (fun _ x -> native.Invoke(x))

    /// <summary>
    /// Defines a two-parameter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Value, Value, Value>) =
        defineFunction2 globals name (fun _ x y -> native.Invoke(x, y))

    /// <summary>
    /// Defines a three-parameter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Value, Value, Value, Value>) =
        defineFunction3 globals name (fun _ x y z -> native.Invoke(x, y, z))
