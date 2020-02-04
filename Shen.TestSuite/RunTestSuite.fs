module Shen.TestSuite

open System
open Kl
open Kl.Values
open Shen.Runtime
open ShenSharp.Shared

let testFolder = fromRoot ["packages"; KernelFolderName; "tests"]

let runTestSuite () =
    let globals = newRuntime ()
    define globals "y-or-n?" (Compiled(0, fun _ _ -> Environment.Exit 1; Empty))
    Environment.CurrentDirectory <- testFolder
    assign globals "*home-directory*" <| Str testFolder
    load globals "README.shen" |> ignore
    load globals "tests.shen" |> ignore

[<EntryPoint>]
let main _ = separateThread16MB runTestSuite
