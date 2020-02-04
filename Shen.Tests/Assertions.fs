module Shen.Tests.Assertions

open NUnit.Framework
open Kl.Values
open Shen.Runtime

let assertTrue globals syntax =
    Assert.IsTrue((True = evalSyntax globals syntax))

let assertFalse globals syntax =
    Assert.IsTrue((False = evalSyntax globals syntax))
