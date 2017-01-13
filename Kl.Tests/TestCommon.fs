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
    let assertTrue = assertEq Values.truev
    let assertFalse = assertEq Values.falsev
    let assertDec value =
        match value with
        | Dec _ -> ()
        | _ -> Assert.Fail "Decimal expected"
    let assertInt value =
        match value with
        | Int _ -> ()
        | _ -> Assert.Fail "Int expected"
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
    let assertEffect eff syntax =
        let env = baseEnv()
        runIn env "(defun effect (X) (do (set *effect* true) X))" |> ignore
        runIn env syntax |> ignore
        match env.Globals.Symbols.GetMaybe "*effect*" with
        | None -> if eff then Assert.Fail "Effect did not occurr" else ()
        | _ -> if not eff then Assert.Fail "Effect should not have occurred" else ()
