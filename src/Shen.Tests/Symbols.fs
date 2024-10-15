module Shen.Tests.Symbols

open NUnit.Framework
open Shen.Runtime
open Assertions

let private globals = newRuntime ()

[<Test>]
let ``the symbols 'true and 'false should be recognized as booleans``() =
    assertTrue globals "(boolean? (intern \"true\"))"
    assertTrue globals "(boolean? (intern \"false\"))"
    assertTrue globals "(boolean? true)"
    assertTrue globals "(boolean? false)"
    assertTrue globals "(boolean? (= 0 0))"
    assertTrue globals "(boolean? (= 0 1))"
    assertFalse globals "(boolean? abc)"
    assertFalse globals "(boolean? (intern \"abc\"))"

[<Test>]
let ``only non-boolean symbols should be recognized as symbols``() =
    assertFalse globals "(symbol? ())"
    assertFalse globals "(symbol? (cons 0 ()))"
    assertFalse globals "(symbol? 0)"
    assertFalse globals "(symbol? \"abc\")"
    assertFalse globals "(symbol? true)"
    assertFalse globals "(symbol? false)"
    assertFalse globals "(symbol? (intern \"true\"))"
    assertFalse globals "(symbol? (intern \"false\"))"
    assertFalse globals "(symbol? (/. X X))"
    assertFalse globals "(symbol? (freeze 0))"
    assertFalse globals "(symbol? (stinput))"
    assertFalse globals "(symbol? (vector 0))"
    assertFalse globals "(symbol? (intern \"fsdf{}.$%2\"))"
    assertTrue globals "(symbol? abc)"
    assertTrue globals "(symbol? (intern \"abc\"))"
    assertTrue globals "(symbol? u87.dfg)"
