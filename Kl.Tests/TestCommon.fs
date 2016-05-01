namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Extensions
open Kl.Tokenizer
open Kl.Parser
open Kl.Evaluator
open Kl.Startup

module TestCommon =
    
    let runIn env syntax = rootEval env.Globals env.CallCounts (rootParse(tokenize syntax))
    let run = runIn (baseEnv())
    let assertEq (expected: 'a) (actual: 'a) = Assert.AreEqual(expected, actual)
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
        runIn env "(defun do (X Y) Y)" |> ignore
        runIn env "(defun effect () (set *effect* true))" |> ignore
        runIn env syntax |> ignore
        match env.Globals.Symbols.GetMaybe "*effect*" with
        | None -> if eff then Assert.Fail "Effect did not occurr" else ()
        | _ -> if not eff then Assert.Fail "Effect should not have occurred" else ()
