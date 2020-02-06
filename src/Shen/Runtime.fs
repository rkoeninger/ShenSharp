module Shen.Runtime

open Kl
open Kl.Values
open Kl.Builtins
open Kl.Startup
open Shen.Kernel

/// <summary>
/// Creates a new Shen runtime environment.
/// </summary>
[<CompiledName "NewRuntime">]
let newRuntime = baseGlobals >> install

/// <summary>
/// Evaluates given Shen syntax and returns result as KL Value.
/// </summary>
[<CompiledName "EvalSyntax">]
let evalSyntax globals syntax = kl_eval globals [kl_read globals [pipeString syntax]]

/// <summary>
/// Loads Shen file at given path.
/// </summary>
[<CompiledName "Load">]
let load globals path = kl_load globals [Str path] |> ignore

/// <summary>
/// Changes working directory to given path.
/// </summary>
[<CompiledName "ChangeDirectory">]
let changeDirectory globals path = kl_cd globals [Str path] |> ignore

/// <summary>
/// Defines a zero-parameter native function.
/// </summary>
[<CompiledName "DefineFunction0">]
let defineFunction0 globals name native =
    let f globals = function
        | [] -> native globals
        | args -> argsErr name [] args
    Compiled(0, f) |> define globals name
    
/// <summary>
/// Defines a one-parameter native function.
/// </summary>
[<CompiledName "DefineFunction1">]
let defineFunction1 globals name native =
    let f globals = function
        | [x] -> native globals x
        | args -> argsErr name ["value"] args
    Compiled(1, f) |> define globals name
    
/// <summary>
/// Defines a two-parameter native function.
/// </summary>
[<CompiledName "DefineFunction2">]
let defineFunction2 globals name native =
    let f globals = function
        | [x; y] -> native globals x y
        | args -> argsErr name ["value"; "value"] args
    Compiled(2, f) |> define globals name
    
/// <summary>
/// Defines a three-parameter native function.
/// </summary>
[<CompiledName "DefineFunction3">]
let defineFunction3 globals name native =
    let f globals = function
        | [x; y; z] -> native globals x y z
        | args -> argsErr name ["value"; "value"; "value"] args
    Compiled(3, f) |> define globals name
    
/// <summary>
/// Defines a Shen macro using a native function.
/// </summary>
[<CompiledName "DefineMacro">]
let defineMacro globals name native =
    let f _ = function
        | [x] -> native x
        | args -> argsErr name ["value"] args
    ``kl_shen.add-macro`` globals [Func <| Compiled(1, f)] |> ignore
