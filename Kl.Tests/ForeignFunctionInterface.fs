module Kl.Tests.``Foreign Function Interface``

open Kl
open NUnit.Framework
open Assertions

[<Test>]
let ``error message for method not found properly formatted``() =
    try run
         """(let L (clr.new System.Collections.Generic.List<System.String> ())
            (let X (clr.invoke L Add (cons (clr.int 0) ()))
                 L))""" |> ignore
    with e -> printfn "%s" e.Message
