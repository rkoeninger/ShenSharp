namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Extensions
open Kl.Tokenizer
open Kl.Parser
open Kl.Evaluator
open Kl.Startup

module TestCommon =
    
    let runInEnv env syntax = rootEval env.Globals (rootParse(tokenize syntax))
    let runIt = runInEnv (baseEnv())
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
    let assertError syntax =
        try
            runIt syntax |> ignore
            Assert.Fail "Error expected"
        with
            _ -> ()
    let assertEffect eff syntax =
        let env = baseEnv()
        runInEnv env "(defun do (X Y) Y)" |> ignore
        runInEnv env "(defun effect () (set *effect* true))" |> ignore
        runInEnv env syntax |> ignore
        match env.Globals.Symbols.GetMaybe "*effect*" with
        | None -> if eff then Assert.Fail "Effect did not occurr" else ()
        | _ -> if not eff then Assert.Fail "Effect should not have occurred" else ()
