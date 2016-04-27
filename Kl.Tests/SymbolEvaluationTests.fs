namespace Kl.Tests

open NUnit.Framework
open Kl
open Kl.Startup
open TestCommon

[<TestFixture>]
type SymbolEvaluationTests() =

    [<Test>]
    member this.DefunAndResolutionOfDefinedFunctions() =
        let env = Values.newEnv()
        runInEnv env "(defun not (X) (if X false true))" |> ignore
        runInEnv env "(defun xor (L R) (or (and L (not R)) (and (not L) R)))" |> ignore
        assertTrue(runInEnv env "(xor true false)")

    [<Test>]
    member this.SymbolResolution() =
        let klIsSymbol _ args =
            match args with
            | [Sym _] -> Values.truev
            | [_] -> Values.falsev
            | _ -> Values.err "symbol? only takes 1 argument"

        let klId _ args =
            match args with
            | [x] -> x
            | _ -> Values.err "id only takes 1 argument"

        let env = baseEnv()
        env.Globals.Functions.["symbol?"] <- Values.primitivev "symbol?" 1 klIsSymbol
        env.Globals.Functions.["id"] <- Values.primitivev "id" 1 klId
        assertTrue(runInEnv env "(symbol? run)")
        assertTrue(runInEnv env "(= run (id run))")

    [<Test>]
    member this.``result of interning a string is equal to symbol with name that is equal to that string``() =
        assertEq (Sym "hi") (runIt "(intern \"hi\")")
        
    [<Test>]
    member this.``idle symbols``() =
        assertEq
            (Cons(Sym "A", Cons(Sym "-->", Cons(Sym "boolean", Empty))))
            (runIt "(cons A (cons --> (cons boolean ())))")
