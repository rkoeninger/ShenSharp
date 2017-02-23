namespace Kl.Tests

open NUnit.Framework
open Kl.Extensions
open Kl
open Kl.Values
open Kl.Analysis
open Kl.Builtins
open Kl.Startup
open Assertions

[<TestFixture>]
type ``Print Debug Info``() =

    [<Test>]
    member this.``print platform information``() =
        let globals = baseGlobals()
        for pair in globals.Symbols do
            printfn "%s = %O" pair.Key pair.Value

    [<Test>]
    member this.``demo ToString() for values``() =
        let pars = parse (newGlobals(), Set.empty)
        printfn "%O" (Int 12)
        printfn "%O" (Num 34.12m)
        printfn "%O" (Sym "hello")
        printfn "%O" (Str "hello")
        printfn "%O" Empty
        printfn "%O" (Cons(Int 1, Empty))
        printfn "%O" (Cons(Int 1, Cons(Int 2, Cons(Int 3, Empty))))
        printfn "%O" (Vec(Array.create 10 Empty))
        printfn "%O" (Err "Something went wrong")
        printfn "%O" (Func(Defun("+", 2, CompiledDefun ``kl_+``)))
        printfn "%O" (Func(Defun("add", 2, InterpretedDefun(["X"; "Y"], pars <| toCons [Sym "+"; Sym "X"; Sym "Y"]))))
        printfn "%O" (Func(Lambda(InterpretedLambda(Map.empty, "X", pars <| toCons [Sym "+"; Int 1; Sym "X"]))))
        printfn "%O" (Func(Freeze(InterpretedFreeze(Map.empty, pars <| toCons [Sym "get-time"; Sym "run"]))))
        printfn "%O" (Func(Partial(Defun("+", 2, CompiledDefun ``kl_+``), [Int 1])))
        printfn "%O" console
