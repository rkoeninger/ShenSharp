module Kl.Tests.Assertions

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Startup
open Kl.Make.Reader

let runIn globals syntax = eval globals (read syntax)
let run = runIn(baseGlobals())
let runAll syntax =
    let globals = baseGlobals()
    List.fold (fun _ -> eval globals) Empty (readAll syntax)
let assertEq (expected: 'a) (actual: 'a) = Assert.AreEqual(expected, actual)
let assertNotEq (expected: 'a) (actual: 'a) = Assert.AreNotEqual(expected, actual)
let assertTrue syntax = assertEq True (runAll syntax)
let assertFalse syntax = assertEq False (runAll syntax)
let assertEach pairs =
    let globals = baseGlobals()
    List.iter (fun (expected, syntax) -> assertEq expected (runIn globals syntax)) pairs
let assertNoError syntax = runAll syntax |> ignore
let assertError syntax =
    try
        runAll syntax |> ignore
        Assert.Fail "Error expected"
    with
        | :? AssertionException as e -> raise e
        | _ -> ()
