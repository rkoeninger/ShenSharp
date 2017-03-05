module Shen.Runtime

open Kl
open Kl.Values
open Kl.Startup
open Shen.Implementation

[<CompiledName "NewRuntime">]
let newRuntime() = install(baseGlobals())

[<CompiledName "EvalSyntax">]
let evalSyntax globals syntax = kl_eval globals [kl_read globals [pipeString syntax]]

[<CompiledName "Load">]
let load globals path = kl_load globals [Str path] |> ignore
