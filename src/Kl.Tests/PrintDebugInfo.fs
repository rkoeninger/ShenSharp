module Kl.Tests.``Print Debug Info``

open NUnit.Framework
open Kl
open Kl.Values
open Kl.Builtins
open Kl.Startup

[<Test>]
let ``print platform information``() =
    let globals = baseGlobals()
    for pair in globals.Symbols do
        Option.iter (printfn "%s = %O" pair.Key) (getValueOption pair.Value)

[<Test>]
let ``demo ToString() for values``() =
    printfn "%O" (Int 12)
    printfn "%O" (Num 34.12m)
    printfn "%O" (Sym "hello")
    printfn "%O" (Str "hello")
    printfn "%O" Empty
    printfn "%O" (Cons(Int 1, Empty))
    printfn "%O" (Cons(Int 1, Cons(Int 2, Cons(Int 3, Empty))))
    printfn "%O" (Cons(Int 1, Cons(Int 2, Int 3)))
    printfn "%O" (Cons(Int 1, Int 2))
    printfn "%O" (Vec(Array.create 10 Empty))
    printfn "%O" (Err "Something went wrong")
    printfn "%O" (Func(Compiled(2, ``kl_+``)))
    printfn "%O" (Func(Interpreted(["X"; "Y"], Application(Constant(Sym "+"), [Constant(Sym "X"); Constant(Sym "Y")]))))
    printfn "%O" (Func(Interpreted(["X"], Application(Constant(Sym "+"), [Constant(Sym "X"); Constant(Int 1)]))))
    printfn "%O" (Func(Interpreted([], Application(Constant(Sym "get-time"), [Constant(Sym "run")]))))
    printfn "%O" (Func(Partial(Compiled(2, ``kl_+``), [Int 1])))
    printfn "%O" console
