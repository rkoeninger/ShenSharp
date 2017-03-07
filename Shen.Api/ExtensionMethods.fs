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
    /// Defines a zero-parameter native function that requires global environment.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Globals, Value>) =
        defineFunction0 globals name native.Invoke

    /// <summary>
    /// Defines a zero-parameter native function.
    /// </summary>
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Value>) =
        defineFunction0 globals name (fun _ -> native.Invoke())

    /// <summary>
    /// Defines a one-paramter native function that requires global environment.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Globals, Value, Value>) =
        defineFunction1 globals name (fun g x -> native.Invoke(g, x))

    /// <summary>
    /// Defines a one-paramter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Value, Value>) =
        defineFunction1 globals name (fun _ -> native.Invoke)

    /// <summary>
    /// Defines a two-paramter native function that requires global environment.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Globals, Value, Value, Value>) =
        defineFunction2 globals name (fun g x y -> native.Invoke(g, x, y))

    /// <summary>
    /// Defines a two-paramter native function.
    /// </summary>
    [<Extension>]
    static member DefineFunction(globals: Globals,
                                 name: string,
                                 native: Func<Value, Value, Value>) =
        defineFunction2 globals name (fun _ x y -> native.Invoke(x, y))
