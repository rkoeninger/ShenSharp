namespace Kl.Tests

open NUnit.Framework
open Kl.Extensions
open Kl
open Kl.Reader
open Kl.Evaluator
open Kl.Builtins
open Kl.Startup
open System
open TestCommon

[<TestFixture>]
type PrintInfo() =

    // Prints environment info so we can confirm it on CI boxes

    [<Test>]
    member this.``print platform information``() =
        let env = baseEnv()
        for pair in env.Globals.Symbols do
            printfn "%s = %s" pair.Key (Values.toStr pair.Value)
