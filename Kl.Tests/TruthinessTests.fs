namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open TestCommon

[<TestFixture>]
[<Ignore("removed truthiness")>]
type TruthinessTests() =

    [<Test>]
    member this.``and expression should evaluate conditionals truthy and always return Bool``() =
        assertTrue(run "(and 1 1)")
        assertTrue(run """(and "hi" -8.34)""")
        assertFalse(run "(and () true)")
        assertFalse(run "(and 5 false)")
        assertFalse(run "(and false ())")

    [<Test>]
    member this.``or expression should evaluate conditionals truthy and always return Bool``() =
        assertTrue(run "(or 1 false)")
        assertTrue(run """(or "hi" -8.34)""")
        assertTrue(run "(or () 5)")
        assertFalse(run "(or false ())")
        assertFalse(run "(or () ())")

    [<Test>]
    member this.``if expression should evaluate conditional truthy``() =
        assertEq (Int 2) (run "(if () 1 2)")
        assertEq (Int 1) (run """(if "hi" 1 2)""")

    [<Test>]
    member this.``cond expression should evaluate conditional truthy``() =
        assertEq (Int 2) (run "(cond (false 0) (() 1) (\"hi\" 2) (false 3))")
        assertEq (Int 1) (run "(cond (() 0) (true 1) (() 2))")
