namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Values
open TestCommon

[<TestFixture>]
type ValuesTests() =

    [<Test>]
    member this.``value equality``() =
        assertEq Empty Empty
        assertEq (Bool true) (Bool true)
        assertEq (Int 5) (Int 5)
        assertEq (Int -2) (Int -2)
        assertEq (Dec 12.6m) (Dec 12.6m)
        assertEq (Int 43) (Dec 43m)
        assertEq (Cons(Int 1, Int 2)) (Cons(Int 1, Int 2))
        assertEq (Vec [|Int 1|]) (Vec [|Int 1|])
        let s = new System.IO.MemoryStream()
        let input = { Name = "Buffer"; Read = s.ReadByte; Close = s.Close }
        assertEq (InStream input) (InStream input)
        assertEq (Err "whoops") (Err "whoops")
        let f = Freeze(Map.empty, Empty)
        assertEq (Func f) (Func f)

    [<Test>]
    member this.``value hash codes``() =
        assertEq (hash Empty) (hash Empty)
        assertEq (hash (Bool false)) (hash (Bool false))
        assertEq (hash (Int 46)) (hash (Int 46))
        assertEq (hash (Dec -4.3m)) (hash (Dec -4.3m))
