module Kl.Tests.``Foreign Function Interface``

open System.Collections.Generic
open Kl
open Kl.Values
open NUnit.Framework
open Assertions

[<Test>]
let ``demo error message for method not found``() =
    try run "(let L (clr.new System.Collections.Generic.List<System.String> ())
             (let X (clr.invoke L Add (cons (clr.int 0) ()))
                  L))" |> ignore
    with e -> printfn "%s" e.Message

    try run "(let D (clr.new System.Collections.Generic.Dictionary<string,System.Collections.Generic.List<int>> ())
             (let L (clr.new System.Collections.Generic.List<string> ())
             (let X (clr.invoke D Add (cons (clr.string \"one\") (cons L ())))
                  D)))" |> ignore
    with e -> printfn "%s" e.Message

    try run "(let D (clr.get-static System.DateTime Now)
             (let X (clr.invoke D ToJson ())
                  X))" |> ignore
    with e -> printfn "%s" e.Message

[<Test>]
let ``CLR type parameter names can be referred to by primitive alias``() =
    let l = run "(clr.new System.Collections.Generic.List<string> ())" |> asObj
    Assert.IsInstanceOf<List<string>> l

[<Test>]
let ``CLR typenames can be aliased``() =
    let l = runAll "(clr.alias List System.Collections.Generic.List)
                    (clr.new List<string> ())" |> asObj
    Assert.IsInstanceOf<List<string>> l
