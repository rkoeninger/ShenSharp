module Shen.Runtime

open Kl
open Kl.Values
open Kl.Startup
open Shen.Language

[<CompiledName "NewRuntime">]
let newRuntime() = install(baseGlobals())

[<CompiledName "EvalSyntax">]
let evalSyntax globals syntax = kl_eval globals [kl_read globals [pipeString syntax]]

[<CompiledName "Load">]
let load globals path = kl_load globals [Str path] |> ignore

[<CompiledName "DefineFunction0">]
let defineFunction0 globals name native =
    let f globals = function
        | [] -> native globals
        | args -> argsErr name ["value"] args
    define globals name (Compiled(0, f))

[<CompiledName "DefineFunction1">]
let defineFunction1 globals name native =
    let f globals = function
        | [x] -> native globals x
        | args -> argsErr name ["value"] args
    define globals name (Compiled(1, f))

[<CompiledName "DefineFunction1">]
let defineFunction2 globals name native =
    let f globals = function
        | [x; y] -> native globals x y
        | args -> argsErr name ["value"] args
    define globals name (Compiled(2, f))

[<CompiledName "DefineMacro">]
let defineMacro globals name native =
    let f _ = function
        | [x] -> native x
        | args -> argsErr name ["value"] args
    ``kl_shen.add-macro`` globals [Func(Compiled(1, f))] |> ignore
