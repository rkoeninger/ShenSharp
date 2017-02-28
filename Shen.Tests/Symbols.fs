[<NUnit.Framework.TestFixture>]
module Shen.Tests.Symbols

open NUnit.Framework
open Assertions

[<Test>]
let ``the symbols 'true and 'false should be recognized as booleans``() =
    assertTrue "(boolean? (intern \"true\"))"
    assertTrue "(boolean? (intern \"false\"))"
    assertTrue "(boolean? true)"
    assertTrue "(boolean? false)"
    assertTrue "(boolean? (= 0 0))"
    assertTrue "(boolean? (= 0 1))"
    assertFalse "(boolean? abc)"
    assertFalse "(boolean? (intern \"abc\"))"

[<Test>]
let ``only non-boolean symbols should be recognized as symbols``() =
    assertFalse "(symbol? ())"
    assertFalse "(symbol? (cons 0 ()))"
    assertFalse "(symbol? 0)"
    assertFalse "(symbol? \"abc\")"
    assertFalse "(symbol? true)"
    assertFalse "(symbol? false)"
    assertFalse "(symbol? (intern \"true\"))"
    assertFalse "(symbol? (intern \"false\"))"
    assertFalse "(symbol? (/. X X))"
    assertFalse "(symbol? (freeze 0))"
    assertFalse "(symbol? (stinput))"
    assertFalse "(symbol? (vector 0))"
    assertTrue "(symbol? abc)"
    assertTrue "(symbol? (intern \"abc\"))"
    assertTrue "(symbol? u87.dfg)"
    assertTrue "(symbol? (intern \"fsdf{}.$%2\"))"
