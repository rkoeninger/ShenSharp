namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Extensions
open Kl.Evaluator
open Kl.Reader
open Kl.Startup

module TestCommon =
    
    let runIn env syntax = rootEval env.Globals (read syntax)
    let run = runIn (baseEnv())
    let runAll syntax =
        let env = baseEnv()
        List.fold (fun _ -> rootEval env.Globals) Empty (readAll syntax)
    let runEach syntax =
        let env = baseEnv()
        List.map (rootEval env.Globals) (readAll syntax)
    let assertEq expected actual = Assert.IsTrue((expected = actual))
    let assertNotEq expected actual = Assert.IsTrue((expected <> actual))
    let assertTrue syntax = assertEq truev (runAll syntax)
    let assertFalse syntax = assertEq falsev (runAll syntax)
    let assertEach pairs =
        let env = baseEnv()
        each (fun (expected, syntax) -> assertEq expected (runIn env syntax)) pairs
    let assertNoError syntax = run syntax |> ignore
    let assertError syntax =
        try
            run syntax |> ignore
            Assert.Fail "Error expected"
        with
            | :? AssertionException as e -> raise e
            | _ -> ()
    let assertErrorInEnv env syntax =
        try
            runIn env syntax |> ignore
            Assert.Fail "Error expected"
        with
            | :? AssertionException as e -> raise e
            | _ -> ()
