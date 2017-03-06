module Kl.Tests.Equality

open System.IO
open NUnit.Framework
open Kl
open Kl.Values
open Assertions

[<Test>]
let ``Empty values are equal``() =
    assertEq Empty Empty

[<Test>]
let ``Nums can be compared for equality``() =
    assertEq (Int 5) (Int 5)
    assertEq (Int -2) (Int -2)
    assertEq (Num 12.6m) (Num 12.6m)
    assertEq (Int 43) (Num 43m)
    assertEq (Int -104) (Num -104.0m)
    assertNotEq (Int 0) (Num 0.00000000001m)

[<Test>]
let ``Strings can be compared for equality``() =
    assertEq (Str "abc") (Str "abc")

[<Test>]
let ``Symbols can be compared for equality``() =
    assertEq (Sym "abc") (Sym "abc")

[<Test>]
let ``Cons are equal if their contained values are equal``() =
    assertEq (Cons(Int 1, Int 2)) (Cons(Int 1, Int 2))
    assertNotEq (toCons [Int 1; Int 2; Int 3]) (toCons [Int 1; Int -2; Int 3])

[<Test>]
let ``Vecs can be compared for equality``() =
    assertEq (Vec [|Int 1|]) (Vec [|Int 1|])
    assertEq (Vec [|Int 1; Int 2; Int 3|]) (Vec [|Int 1; Int 2; Int 3|])
    assertEq (Vec [||]) (Vec [||])
    assertNotEq (Vec [|Int -5|]) (Vec [|Int 5|])

[<Test>]
let ``Streams can be compared for equality``() =
    let s = new MemoryStream()
    let io = {
        Name = "Buffer"
        Read = s.ReadByte
        Write = s.WriteByte
        Close = s.Close
    }
    assertEq (Pipe io) (Pipe io)

[<Test>]
let ``Err values can be compared for equality``() =
    assertEq (Err "whoops") (Err "whoops")

[<Test>]
let ``Functions can be compared for reference equality``() =
    let f = Interpreted([], Constant Empty)
    assertEq (Func f) (Func f)
    let l =
        Interpreted(
            ["X"],
            Application(Constant(Sym "+"), [Constant(Int 1); Constant(Sym "X")]))
    assertEq (Func l) (Func l)
    let inc _ = function
        | [Int x] -> Int(x + 1)
        | _ -> failwith "Must be integer"
    let n = Compiled(1, inc)
    assertEq (Func n) (Func n)
    assertNotEq (Func(Interpreted([], Constant Empty)))
                (Func(Interpreted([], Constant Empty)))

[<Test>]
let ``Hash codes can be generated for all value types``() =
    assertEq (hash Empty) (hash Empty)
    assertEq (hash (Int 46)) (hash (Int 46))
    assertEq (hash (Num -4.3m)) (hash (Num -4.3m))
    assertEq (hash (Sym "abc")) (hash (Sym "abc"))
    assertEq (hash (Str "abc")) (hash (Str "abc"))
    assertEq (hash (Cons(Int 12, Sym "a"))) (hash (Cons(Int 12, Sym "a")))
