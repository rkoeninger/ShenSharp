namespace Kl.Tests

open System.IO
open NUnit.Framework
open Kl
open Kl.Values
open TestCommon

[<TestFixture>]
type ValuesTests() =

    [<Test>]
    member this.``Empty values are equal``() =
        assertEq Empty Empty

    [<Test>]
    member this.``Nums can be compared for equality``() =
        assertEq (Int 5) (Int 5)
        assertEq (Int -2) (Int -2)
        assertEq (Num 12.6m) (Num 12.6m)
        assertEq (Int 43) (Num 43m)
        assertEq (Int -104) (Num -104.0m)
        assertNotEq (Int 0) (Num 0.00000000001m)

    [<Test>]
    member this.``Strings can be compared for equality``() =
        assertEq (Str "abc") (Str "abc")

    [<Test>]
    member this.``Symbols can be compared for equality``() =
        assertEq (Sym "abc") (Sym "abc")

    [<Test>]
    member this.``Cons are equal if their contained values are equal``() =
        assertEq (Cons(Int 1, Int 2)) (Cons(Int 1, Int 2))
        assertNotEq (toCons [Int 1; Int 2; Int 3]) (toCons [Int 1; Int -2; Int 3])

    [<Test>]
    member this.``Vecs can be compared for equality``() =
        assertEq (Vec [|Int 1|]) (Vec [|Int 1|])
        assertEq (Vec [|Int 1; Int 2; Int 3|]) (Vec [|Int 1; Int 2; Int 3|])
        assertEq (Vec [||]) (Vec [||])
        assertNotEq (Vec [|Int -5|]) (Vec [|Int 5|])

    [<Test>]
    member this.``Streams can be compared for equality``() =
        let s = new MemoryStream()
        let io = { Name = "Buffer"; Read = s.ReadByte; Write = s.WriteByte; Close = s.Close }
        assertEq (Pipe io) (Pipe io)

    [<Test>]
    member this.``Err values can be compared for equality``() =
        assertEq (Err "whoops") (Err "whoops")

    [<Test>]
    member this.``Functions can be compared for equality``() =
        let f = Freeze(Map.empty, Empty)
        assertEq (Func f) (Func f)
        let l = Lambda("X", Map.empty, toCons [Sym "+"; Int 1; Sym "X"])
        assertEq (Func l) (Func l)
        let d = Defun("inc", ["X"], toCons [Sym "+"; Int 1; Sym "X"])
        assertEq (Func d) (Func d)
        let inc _ = function
            | [Int x] -> Int(x + 1)
            | _ -> err "Must be integer"
        let n = Native("inc", 1, inc)
        assertEq (Func n) (Func n)

    [<Test>]
    member this.``Hash codes can be generated for all value types``() =
        assertEq (hash Empty) (hash Empty)
        assertEq (hash (Int 46)) (hash (Int 46))
        assertEq (hash (Num -4.3m)) (hash (Num -4.3m))
        assertEq (hash (Sym "abc")) (hash (Sym "abc"))
        assertEq (hash (Str "abc")) (hash (Str "abc"))
        assertEq (hash (Cons(Int 12, Sym "a"))) (hash (Cons(Int 12, Sym "a")))
