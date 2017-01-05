namespace Kl.Tests

open NUnit.Framework
open Kl.Extensions
open Kl
open Kl.Values
open Kl.Builtins
open Kl.Startup
open TestCommon

[<TestFixture>]
type PrintInfo() =

    // Prints environment info so we can confirm it on CI boxes

    [<Test>]
    member this.``print platform information``() =
        let env = baseEnv()
        for pair in env.Globals.Symbols do
            printfn "%s = %A" pair.Key pair.Value

    [<Test>]
    member this.``demo ToString() for values``() =
        printfn "%A" (Bool true)
        printfn "%A" (Bool false)
        printfn "%A" (Int 12)
        printfn "%A" (Dec 34.12m)
        printfn "%A" (Sym "hello")
        printfn "%A" (Str "hello")
        printfn "%A" Empty
        printfn "%A" (Cons(Int 1, Empty))
        printfn "%A" (Cons(Int 1, Cons(Int 2, Cons(Int 3, Empty))))
        printfn "%A" (Vec(Array.create 10 Empty))
        printfn "%A" (Err "Something went wrong")
        printfn "%A" (Func(Native("+", 2, klAdd)))
        printfn "%A" (Func(Defun("add", ["X"; "Y"], toCons [Sym "+"; Sym "X"; Sym "Y"])))
        printfn "%A" (Func(Lambda("X", Map.empty, toCons [Sym "+"; Int 1; Sym "X"])))
        printfn "%A" (Func(Freeze(Map.empty, Empty)))
        printfn "%A" (Func(Partial(Native("+", 2, klAdd), [Int 1])))
        printfn "%A" stinput
        printfn "%A" stoutput
