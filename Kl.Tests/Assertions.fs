module Kl.Tests.Assertions

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Evaluator
open Kl.Startup
open Kl.Make.Reader

let runIn globals syntax = eval globals (read syntax)
let run = runIn(unprotectAll(baseGlobals()))
let runAll syntax =
    let globals = unprotectAll(baseGlobals())
    List.fold (fun _ -> eval globals) Empty (readAll syntax)

let assertEq (expected: 'a) (actual: 'a) = Assert.AreEqual(expected, actual)
let assertNotEq (expected: 'a) (actual: 'a) = Assert.AreNotEqual(expected, actual)
let assertTrue syntax = assertEq True (runAll syntax)
let assertFalse syntax = assertEq False (runAll syntax)

let assertEach pairs =
    let globals = unprotectAll(baseGlobals())
    List.iter (fun (expected, syntax) -> assertEq expected (runIn globals syntax)) pairs

let assertNoError syntax = runAll syntax |> ignore

let assertError syntax =
    try
        runAll syntax |> ignore
        Assert.Fail "Error expected"
    with
        | :? AssertionException as e -> raise e
        | _ -> ()

let assertEffect eff syntax =
    let globals = baseGlobals()
    runIn globals "(defun effect (X) (do (set *effect* true) X))" |> ignore
    runIn globals syntax |> ignore
    match getValueOption (intern globals "*effect*") with
    | None -> if eff then Assert.Fail "Effect did not occurr" else ()
    | _ -> if not eff then Assert.Fail "Effect should not have occurred" else ()

let assertDec expr =
    match run expr with
    | Num _ -> ()
    | _ -> Assert.Fail "Decimal expected"

let assertInt expr =
    match run expr with
    | Int _ -> ()
    | _ -> Assert.Fail "Int expected"

let checkPattern recognizer samples =
    let checkSample (some, s) =
        let expect = if some then Option.isSome else Option.isNone
        Assert.True(s |> read |> recognizer |> expect, s)
    List.iter checkSample samples

let attempt body =
    assertTrue (sprintf "(defun count-down (X) %s) (count-down 20000)" body)
