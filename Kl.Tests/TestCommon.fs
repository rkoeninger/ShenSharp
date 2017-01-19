namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Extensions
open Kl.Evaluator
open Kl.Reader
open Kl.Startup

module TestCommon =
    
    let runIn globals syntax = rootEval globals (read syntax)
    let run = runIn (baseGlobals())
    let runAll syntax =
        let globals = baseGlobals()
        List.fold (fun _ -> rootEval globals) Empty (readAll syntax)
    let assertEq (expected: 'a) (actual: 'a) = Assert.AreEqual(expected, actual)
    let assertNotEq (expected: 'a) (actual: 'a) = Assert.AreNotEqual(expected, actual)
    let assertTrue syntax = assertEq truev (runAll syntax)
    let assertFalse syntax = assertEq falsev (runAll syntax)
    let assertEach pairs =
        let globals = baseGlobals()
        each (fun (expected, syntax) -> assertEq expected (runIn globals syntax)) pairs
    let assertNoError syntax = runAll syntax |> ignore
    let assertError syntax =
        try
            runAll syntax |> ignore
            Assert.Fail "Error expected"
        with
            | :? AssertionException as e -> raise e
            | _ -> ()
