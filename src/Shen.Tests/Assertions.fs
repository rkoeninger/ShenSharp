module Shen.Tests.Assertions

open NUnit.Framework
open Kl.Values
open Shen.Runtime

let assertTrue syntax =
    let globals = newRuntime()
    Assert.IsTrue((True = evalSyntax globals syntax))

let assertFalse syntax =
    let globals = newRuntime()
    Assert.IsTrue((False = evalSyntax globals syntax))
