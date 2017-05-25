module Shen.Runtime

open Kl
open Kl.Values
open Kl.Startup
open Shen.Language

/// <summary>
/// Creates a new Shen runtime environment.
/// </summary>
[<CompiledName "NewRuntime">]
let newRuntime() = install(baseGlobals())

/// <summary>
/// Evaluates given Shen syntax and returns result as KL Value.
/// </summary>
[<CompiledName "EvalSyntax">]
let evalSyntax globals syntax = kl_eval globals [kl_read globals [pipeString syntax]]

/// <summary>
/// Runs the standard Shen REPL.
/// </summary>
[<CompiledName "ShenRepl">]
let shenRepl globals = ``kl_shen.shen`` globals [] |> ignore

/// <summary>
/// Loads Shen file at given path.
/// </summary>
[<CompiledName "Load">]
let load globals path = kl_load globals [Str path] |> ignore

/// <summary>
/// Defines a zero-parameter native function.
/// </summary>
[<CompiledName "DefineFunction0">]
let defineFunction0 globals name native =
    let f globals = function
        | [] -> native globals
        | args -> argsErr name [] args
    define globals name (Compiled(0, f))
    
/// <summary>
/// Defines a one-parameter native function.
/// </summary>
[<CompiledName "DefineFunction1">]
let defineFunction1 globals name native =
    let f globals = function
        | [x] -> native globals x
        | args -> argsErr name ["value"] args
    define globals name (Compiled(1, f))
    
/// <summary>
/// Defines a two-parameter native function.
/// </summary>
[<CompiledName "DefineFunction2">]
let defineFunction2 globals name native =
    let f globals = function
        | [x; y] -> native globals x y
        | args -> argsErr name ["value"; "value"] args
    define globals name (Compiled(2, f))
    
/// <summary>
/// Defines a three-parameter native function.
/// </summary>
[<CompiledName "DefineFunction3">]
let defineFunction3 globals name native =
    let f globals = function
        | [x; y; z] -> native globals x y z
        | args -> argsErr name ["value"; "value"; "value"] args
    define globals name (Compiled(3, f))
    
/// <summary>
/// Defines a Shen macro using a native function.
/// </summary>
[<CompiledName "DefineMacro">]
let defineMacro globals name native =
    let f _ = function
        | [x] -> native x
        | args -> argsErr name ["value"] args
    ``kl_shen.add-macro`` globals [Func(Compiled(1, f))] |> ignore
