module Kl.Tests.``Foreign Function Interface``

open System.Collections.Generic
open Kl
open Kl.Values
open NUnit.Framework
open Assertions

[<Test>]
let ``demo error message for method not found properly formatted``() =
    try run
         """(let L (clr.new System.Collections.Generic.List<System.String> ())
            (let X (clr.invoke L Add (cons (clr.int 0) ()))
                 L))""" |> ignore
    with e -> printfn "%s" e.Message

[<Test>]
let ``CLR type parameter names can be referred to by primitive alias``() =
    let l = run "(clr.new System.Collections.Generic.List<string> ())" |> asObj
    Assert.IsInstanceOf<List<string>> l
