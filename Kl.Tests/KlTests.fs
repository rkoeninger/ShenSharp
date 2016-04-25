namespace Kl.Tests

open NUnit.Framework
open FParsec
open Kl
open Kl.Tokenizer
open Kl.Parser
open Kl.Evaluator
open Kl.Builtins
open System.Reflection
open System
open System.IO

[<TestFixture>]
type KlTests() =

    let runInEnv env = tokenize >> rootParse >> rootEval env.Globals
    let runIt = runInEnv (baseEnv ())
    let isIntR = function | Ok (IntValue _) -> true | _ -> false
    let isDecimalR = function | Ok (DecimalValue _) -> true | _ -> false
    let rError = function
        | Ok (ErrorValue e) -> e
        | _ -> failwith "not an Error"
    let rInt = function
        | Ok (IntValue n) -> n
        | _ -> failwith "not an int"
    let rString = function
        | Ok (StringValue s) -> s
        | _ -> failwith "not a String"
    let rVector = function
        | Ok (VectorValue s) -> s
        | _ -> failwith "not a Vector"
    let rUncaught = function
        | Err s -> s
        | _ -> failwith "not an Error"
    let isUncaught = function
        | Err _ -> true
        | _ -> false
    let arrayEqual (xs : 'a[]) (ys : 'a[]) =
        xs.Length = ys.Length && Array.forall2 (=) xs ys
    let rFunc = function
        | Ok (FunctionValue f) -> f
        | _ -> failwith "not a Function"
    let isThunk = function
        | Done _ -> true
        | _ -> false
    let rBool = function
        | Ok (BoolValue b) -> b
        | _ -> failwith "not a Bool"
    let trueV = BoolValue true
    let falseV = BoolValue false
    let trueR = BoolValue true |> Ok
    let falseR = BoolValue false |> Ok
    let trueW = trueR |> Done
    let falseW = falseR |> Done
    let eApp1 f arg1 = AppExpr(Head, f, [arg1])
    let symApp1 sym arg1 = AppExpr (Head, SymbolExpr sym, [arg1])
    let symApp2 sym arg1 arg2 = AppExpr (Head, SymbolExpr sym, [arg1; arg2])
    let symApp3 sym arg1 arg2 arg3 = AppExpr (Head, SymbolExpr sym, [arg1; arg2; arg3])
    let intE = IntExpr
    let intV = IntValue
    let intR = intV >> Ok
    let func n f = new Function("", n, [], f)
    let funcV n f = FunctionValue <| func n f
    let strV = StringValue
    let strR = StringValue >> Ok
    let str x = x.ToString()

    [<Test>]
    member this.TokenizerTest() =
        let tryit = tokenize >> ignore

        tryit "abc"
        tryit "123"
        tryit "()"
        tryit "(a)"
        tryit "(a b)"
        tryit "((a b) c (d e f) ((a) ()) c (d f))"
        tryit "(a     b)"
        tryit "(a   \" b c \"  d)"
        let all = tokenizeAll "\"long copyright string\n\rwith line breaks\r\n\"\r\n\r\n(defun form () (stuff 0))\r\n\r\n(defun form2 (a b) (+ a b))"
        Assert.AreEqual(3, all.Length)

    [<Test>]
    member this.LazyBooleanOperations() =
        let consToArray = function
            | ConsValue _ as cons ->
                let generator = function | ConsValue (head, tail) -> Some(head, tail)
                                         | EmptyValue -> None
                                         | _ -> invalidArgs ()
                cons |> Seq.unfold generator |> Seq.toArray
            | _ -> [||]
        let lazinessTest syntax expectedBool expectedResults =
            let env = baseEnv ()
            runInEnv env "(defun do (X Y) Y)" |> ignore
            env.Globals.Symbols.["results"] <- EmptyValue
            match runInEnv env syntax with
            | Ok (BoolValue b) ->
                Assert.IsTrue((b && expectedBool) || not(b || expectedBool))
                let results = env.Globals.Symbols.["results"] |> consToArray
                Assert.IsTrue(arrayEqual expectedResults results)
            | _ -> Assert.Fail()

        lazinessTest "(and true true)" true [||]
        lazinessTest "(and false (do (set results (cons 1 (value results))) false))" false [||]
        lazinessTest "(and true (do (set results (cons 1 (value results))) false))" false [|intV 1|]
        lazinessTest "(and true (do (set results (cons 1 (value results))) true))" true [|intV 1|]
        lazinessTest "(or false true)" true [||]
        lazinessTest "(or false (do (set results (cons 1 (value results))) false))" false [|intV 1|]
        lazinessTest "(or true (do (set results (cons 1 (value results))) false))" true [||]

    [<Test>]
    member this.DefunAndResolutionOfDefinedFunctions() =
        let env = Values.newEnv()
        let klNot _ args =
            match args with
            | [BoolValue b] -> not b |> BoolValue |> Ok |> Done
            | _ -> failwith "must be bool"
        env.Globals.Functions.["not"] <- func 1 klNot
        runInEnv env "(defun xor (L R) (or (and L (not R)) (and (not L) R)))" |> ignore
        Assert.AreEqual(trueR, runInEnv env "(xor true false)")

    [<Test>]
    member this.SymbolResolution() =
        let env = Values.newEnv()
        let klIsSymbol _ args =
            match args with
            | [SymbolValue _] -> trueW
            | _ -> falseW
        env.Globals.Functions.["symbol?"] <- func 1 klIsSymbol
        Assert.AreEqual(trueR, runInEnv env "(symbol? run)")
        let klId _ args =
            match args with
            | [x] -> Ok x |> Done
            | _ -> failwith "must be 1 arg"
        env.Globals.Functions.["id"] <- func 1 klId
        Assert.AreEqual(trueR, runInEnv env "(symbol? (id run))")

    [<Test>]
    member this.``result of interning a string is equal to symbol with name that is equal to that string``() =
        Assert.AreEqual(Ok (SymbolValue "hi"), runIt "(intern \"hi\")")

    [<Test>]
    member this.PartialApplicationForBuiltins() =
        let env = baseEnv()
        Assert.AreEqual(intR 3, eval env (symApp2 "+" (intE 1) (intE 2)))
        runInEnv env "(defun add4 (A B C D) (+ A (+ B (+ C D))))" |> ignore
        Assert.AreEqual(intR 10, runInEnv env "(let X (add4 1) (let Y (X 2 3) (Y 4)))")

    [<Test>]
    member this.Builtins() =
        Assert.AreEqual(intR 3, runIt "(let X (+ 1) (X 2))")
        Assert.AreEqual(intR 2, runIt "(let X (- 4) (X 2))")
    
    [<Test>]
    member this.``adding two integers gives integer``() =
        Assert.IsTrue(isIntR <| runIt "(+ 5 3)")

    [<Test>]
    member this.``adding decimal and integer gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(+ 1.1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(+ 11 -2.4)")
    
    [<Test>]
    member this.``adding two decimals gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(+ 1.1 2.4)")

    [<Test>]
    member this.``subtracting two integers gives integer``() =
        Assert.IsTrue(isIntR <| runIt "(- 1 2)")

    [<Test>]
    member this.``subtracting integer from decimal gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(- 1.1 2)")

    [<Test>]
    member this.``subtracting decimal from integer gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(- 11 -2.4)")

    [<Test>]
    member this.``subtracting two decimals gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(- 1.1 2.4)")

    [<Test>]
    member this.``multiplying two integers gives integer``() =
        Assert.IsTrue(isIntR <| runIt "(* 1 2)")

    [<Test>]
    member this.``multiplying integer and decimal gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(* 1.1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(* 11 -2.4)")

    [<Test>]
    member this.``multiplying two decimals gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(* 1.1 2.4)")

    [<Test>]
    member this.``dividing any combination of two decimals/integers gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(/ 1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 2 1)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 1.1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 11 -2.4)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 1.1 2.4)")

    [<Test>]
    member this.StringFunctions() =
        Assert.AreEqual(strR "Hello, World!", runIt "(cn \"Hello, \" \"World!\")")
        Assert.AreEqual(strR "Hello", runIt "(cn (n->string (string->n \"Hello\")) (tlstr \"Hello\"))")
        Assert.AreEqual(strR "1", runIt "(str 1)")
        
    [<Test>]
    member this.Vectors() =
        Assert.IsTrue(arrayEqual Array.empty<Value> (runIt "(absvector 0)" |> rVector))
        Assert.IsTrue(arrayEqual [|SymbolValue "fail!"|] (runIt "(absvector 1)" |> rVector))
        Assert.IsTrue(arrayEqual [|BoolValue true|] (runIt "(address-> (absvector 1) 0 true)" |> rVector))

    [<Test>]
    member this.Conses() =
        Assert.AreEqual(trueR, runIt "(= () (tl (cons 1 ())))")
        Assert.AreEqual(falseR, runIt "(cons? ())")
        Assert.AreEqual(falseR, runIt "(cons? 0)")
        Assert.AreEqual(trueR, runIt "(cons? (cons 0 0))")

    [<Test>]
    member this.EvalFunction() =
        Assert.AreEqual(intR 3, runIt "(eval-kl (cons + (cons 1 (cons 2 ()))))") // (+ 1 2)
        let incR = runIt "(eval-kl (cons lambda (cons X (cons (cons + (cons 1 (cons X ()))) ()))))" // (lambda X (+ 1 X))
        let inc = rFunc incR
        Assert.AreEqual(intR 5, inc.Apply(Values.newGlobals(), [intV 4]) |> go)

    [<Test>]
    member this.SanityChecks() =
        Assert.AreEqual(BoolToken true, BoolToken true)
        Assert.AreEqual(BoolExpr true, BoolExpr true)
        Assert.AreEqual(BoolValue true, BoolValue true) // this was failing when KlValue had a case containing a function type
        Assert.AreEqual(true |> BoolValue |> Ok, true |> BoolValue |> Ok) // this might start failing for the same reason
        Assert.AreEqual(true |> BoolValue |> Ok |> Done, true |> BoolValue |> Ok |> Done)

    [<Test>]
    member this.``deep-running tail-recursive function does not stack overflow``() =
        let env = baseEnv ()
        runInEnv env "(defun fill (Vec Start Stop Val) (if (= Stop Start) (address-> Vec Start Val) (fill (address-> Vec Start Val) (+ 1 Start) Stop Val)))" |> ignore
        let x = runInEnv env "(fill (absvector 20000) 0 19999 0)"
        ()
    
    [<Test>]
    member this.``deep-running mutually-recursive functions do not stack overflow``() =
        let env = baseEnv ()
        runInEnv env "(defun odd? (X) (if (= 1 X) true (even? (- X 1))))" |> ignore
        runInEnv env "(defun even? (X) (if (= 1 X) false (odd? (- X 1))))" |> ignore
        Assert.AreEqual(Ok (BoolValue false), runInEnv env "(odd? 20000)")

    [<Test>]
    member this.HeadTailPositionsParsed() =
        let e = "(defun ! (acc n) (if (= 0 n) acc (! (* n acc) (- n 1))))" |> tokenize |> rootParse
        let e0 = DefunExpr ("!",
                            ["acc"; "n"],
                            IfExpr (AppExpr (Head,
                                             SymbolExpr "=",
                                             [intE 0; SymbolExpr "n"]),
                                    SymbolExpr "acc",
                                    AppExpr (Tail,
                                             SymbolExpr "!",
                                             [symApp2 "*" (SymbolExpr "n") (SymbolExpr "acc")
                                              symApp2 "-" (SymbolExpr "n") (intE 1)])))
        Assert.AreEqual(e0, e)

    [<Test>]
    member this.``string index out of bounds should cause uncaught error``() =
        Assert.IsTrue(runIt "(pos \"\" 0)" |> isUncaught)
        Assert.IsTrue(runIt "(pos \"hello\" 5)" |> isUncaught)

    [<Test>]
    member this.``vector index out of bounds should cause uncaught error``() =
        Assert.IsTrue(runIt "(<-address (absvector 0) 0)" |> isUncaught)

    [<Test>]
    member this.``simple-error should cause uncaught error``() =
        Assert.AreEqual("whoops", "(simple-error \"whoops\")" |> runIt |> rUncaught)

    [<Test>]
    member this.``simple-error should be caught by trap-error``() =
        Assert.AreEqual(Ok EmptyValue, runIt "(trap-error (simple-error \"whoops\") (lambda E ()))")

    [<Test>]
    member this.``trap-error should prevent uncaught error from propogating``() =
        Assert.AreEqual(Ok EmptyValue, runIt "(trap-error (pos \"\" 0) (lambda E ()))")

    [<Test>]
    member this.``trap-error should eval and apply second expression if eval of first results in uncaught error``() =
        let env = baseEnv ()
        runInEnv env "(defun do (X Y) Y)" |> ignore
        Assert.IsTrue(runInEnv env "(trap-error (do (pos \"\" 0) false) (lambda E true))" |> rBool)

    [<Test>]
    member this.``error message should be preserved when error is caught and handled``() =
        Assert.AreEqual(strR "hi", runIt "(trap-error (simple-error \"hi\") (lambda E (error-to-string E)))")

    [<Test>]
    member this.PrintStuff() =
        "(get-time unix)" |> runIt |> rInt |> printfn "Unix time: %i"
        "(get-time run)" |> runIt |> rInt |> printfn "Run time: %i"
        "(str (cons 1 (cons 2 (cons 3 ()))))" |> runIt |> rString |> printfn "Cons: %s"
        "(str (address-> (address-> (address-> (absvector 3) 0 1) 1 2) 2 3))" |> runIt |> rString |> printfn "Vector: %s"
        "(trap-error (simple-error \"whoops\") (lambda E E))" |> runIt |> rError |> printfn "Error: %s"
        "(str (trap-error (simple-error \"whoops\") (lambda Ex Ex)))" |> runIt |> rString |> printfn "Error-string: %s"
