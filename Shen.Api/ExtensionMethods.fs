[<System.Runtime.CompilerServices.Extension>]
module Shen.ExtensionMethods

open System.Runtime.CompilerServices
open Kl
open Shen.Implementation
open Shen.Runtime

[<Extension>]
let Eval(globals: Globals, syntax: string) = evalSyntax globals syntax

[<Extension>]
let Load(globals: Globals, path: string) = load globals path
