module Kl.Tests.``Print Debug Info``

open NUnit.Framework
open Kl.Extensions
open Kl
open Kl.Values
open Kl.Analysis
open Kl.Builtins
open Kl.Startup
open Assertions

[<Test>]
let ``print platform information``() =
    let globals = baseGlobals()
    for pair in globals do
        Option.iter (printfn "%s = %O" pair.Key) (getValueOption pair.Value)

[<Test>]
let ``demo ToString() for values``() =
    let pars = parse (newGlobals(), Set.empty)
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
    printfn "%O" (Func(Interpreted(Map.empty, ["X"; "Y"], pars <| toCons [Sym "+"; Sym "X"; Sym "Y"])))
    printfn "%O" (Func(Interpreted(Map.empty, ["X"], pars <| toCons [Sym "+"; Int 1; Sym "X"])))
    printfn "%O" (Func(Interpreted(Map.empty, [], pars <| toCons [Sym "get-time"; Sym "run"])))
    printfn "%O" (Func(Partial(Compiled(2, ``kl_+``), [Int 1])))
    printfn "%O" console
