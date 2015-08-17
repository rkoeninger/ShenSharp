namespace Kl

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open KlTokenizer
open KlParser
open KlEvaluator
open KlBuiltins

[<TestClass>]
type Tests() =

    let runInEnv env = tokenize >> parse Head >> eval env
    let runIt = runInEnv (baseEnv ())
    let isIntR = function | ValueResult (IntValue _) -> true | _ -> false
    let isDecimalR = function | ValueResult (DecimalValue _) -> true | _ -> false
    let rError = function
        | ValueResult (ErrorValue e) -> e
        | _ -> failwith "not an Error"
    let rInt = function
        | ValueResult (IntValue n) -> n
        | _ -> failwith "not an int"
    let rString = function
        | ValueResult (StringValue s) -> s
        | _ -> failwith "not a String"
    let rVector = function
        | ValueResult (VectorValue s) -> s
        | _ -> failwith "not a Vector"
    let rUncaught = function
        | ErrorResult s -> s
        | _ -> failwith "not an Error"
    let isUncaught = function
        | ErrorResult _ -> true
        | _ -> false
    let arrayEqual (xs : 'a[]) (ys : 'a[]) =
        xs.Length = ys.Length && Array.forall2 (=) xs ys
    let rFunc = function
        | ValueResult (FunctionValue f) -> f
        | _ -> failwith "not a Function"
    let isThunk = function
        | Completed _ -> true
        | _ -> false
    let rBool = function
        | ValueResult (BoolValue b) -> b
        | _ -> failwith "not a Bool"
    let trueV = BoolValue true
    let falseV = BoolValue false
    let trueR = BoolValue true |> ValueResult
    let falseR = BoolValue false |> ValueResult
    let trueW = trueR |> Completed
    let falseW = falseR |> Completed
    let eApp1 f arg1 = AppExpr(Head, f, [arg1])
    let symApp1 sym arg1 = AppExpr (Head, SymbolExpr sym, [arg1])
    let symApp2 sym arg1 arg2 = AppExpr (Head, SymbolExpr sym, [arg1; arg2])
    let symApp3 sym arg1 arg2 arg3 = AppExpr (Head, SymbolExpr sym, [arg1; arg2; arg3])
    let intE = IntExpr
    let intV = IntValue
    let intR = intV >> ValueResult
    let funcV n f = FunctionValue <| new Function(n, f)
    let strV = StringValue
    let strR = StringValue >> ValueResult
    let str x = x.ToString()

    [<TestMethod>]
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

    [<TestMethod>]
    member this.ParserTest() =
        let tryit syntax expr = 
            match run pKlToken syntax with
                | Success(result, _, _)   -> Assert.AreEqual(expr, parse Head result)
                | Failure(errorMsg, _, _) -> Assert.Fail(errorMsg)

        tryit "2" (intE 2)
        tryit "()" EmptyExpr
        tryit "(add 1 2)" (symApp2 "add" (intE 1) (intE 2))

    [<TestMethod>]
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
            env.SymbolDefinitions.["results"] <- EmptyValue
            match runInEnv env syntax with
            | ValueResult (BoolValue b) ->
                Assert.IsTrue((b && expectedBool) || not(b || expectedBool))
                let results = env.SymbolDefinitions.["results"] |> consToArray
                Assert.IsTrue(arrayEqual expectedResults results)
            | _ -> Assert.Fail()

        lazinessTest "(and true true)" true [||]
        lazinessTest "(and false (do (set results (cons 1 (value results))) false))" false [||]
        lazinessTest "(and true (do (set results (cons 1 (value results))) false))" false [|intV 1|]
        lazinessTest "(and true (do (set results (cons 1 (value results))) true))" true [|intV 1|]
        lazinessTest "(or false true)" true [||]
        lazinessTest "(or false (do (set results (cons 1 (value results))) false))" false [|intV 1|]
        lazinessTest "(or true (do (set results (cons 1 (value results))) false))" true [||]

    [<TestMethod>]
    member this.DefunAndResolutionOfDefinedFunctions() =
        let env = emptyEnv ()
        env.FunctionDefinitions.["not"] <- funcV 1 (function | [BoolValue b] -> not b |> BoolValue |> ValueResult |> Completed
                                                             | _             -> failwith "must be bool")
        runInEnv env "(defun xor (l r) (or (and l (not r)) (and (not l) r)))" |> ignore
        Assert.AreEqual(trueR, runInEnv env "(xor true false)")

    [<TestMethod>]
    member this.SymbolResolution() =
        let env = emptyEnv ()
        env.FunctionDefinitions.["symbol?"] <- funcV 1 (function | [SymbolValue _] -> trueW
                                                                 | _               -> falseW)
        Assert.AreEqual(trueR, runInEnv env "(symbol? run)")
        env.FunctionDefinitions.["id"] <- funcV 1 (function | [x] -> ValueResult x |> Completed
                                                            | _   -> failwith "must be 1 arg")
        Assert.AreEqual(trueR, runInEnv env "(symbol? (id run))")

    [<TestMethod>]
    member this.``result of interning a string is equal to symbol with name that is equal to that string``() =
        Assert.AreEqual(ValueResult (SymbolValue "hi"), runIt "(intern \"hi\")")

    [<TestMethod>]
    member this.PartialApplicationForBuiltins() =
        let env = baseEnv ()
        Assert.AreEqual(intR 3, eval env (symApp2 "+" (intE 1) (intE 2)))
        Assert.AreEqual(intR 3, eval env (eApp1 (symApp1 "+" (intE 1)) (intE 2)))
        runInEnv env "(defun add4 (a b c d) (+ a (+ b (+ c d))))" |> ignore
        Assert.AreEqual(intR 10, runInEnv env "(((add4 1) 2 3) 4)")

    [<TestMethod>]
    member this.Builtins() =
        Assert.AreEqual(intR 3, runIt "((+ 1) 2)")
        Assert.AreEqual(intR 2, runIt "((- 4) 2)")
    
    [<TestMethod>]
    member this.``adding two integers gives integer``() =
        Assert.IsTrue(isIntR <| runIt "(+ 5 3)")

    [<TestMethod>]
    member this.``adding decimal and integer gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(+ 1.1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(+ 11 -2.4)")
    
    [<TestMethod>]
    member this.``adding two decimals gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(+ 1.1 2.4)")

    [<TestMethod>]
    member this.``subtracting two integers gives integer``() =
        Assert.IsTrue(isIntR <| runIt "(- 1 2)")

    [<TestMethod>]
    member this.``subtracting integer from decimal gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(- 1.1 2)")

    [<TestMethod>]
    member this.``subtracting decimal from integer gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(- 11 -2.4)")

    [<TestMethod>]
    member this.``subtracting two decimals gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(- 1.1 2.4)")

    [<TestMethod>]
    member this.``multiplying two integers gives integer``() =
        Assert.IsTrue(isIntR <| runIt "(* 1 2)")

    [<TestMethod>]
    member this.``multiplying integer and decimal gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(* 1.1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(* 11 -2.4)")

    [<TestMethod>]
    member this.``multiplying two decimals gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(* 1.1 2.4)")

    [<TestMethod>]
    member this.``dividing any combination of two decimals/integers gives decimal``() =
        Assert.IsTrue(isDecimalR <| runIt "(/ 1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 2 1)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 1.1 2)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 11 -2.4)")
        Assert.IsTrue(isDecimalR <| runIt "(/ 1.1 2.4)")

    [<TestMethod>]
    member this.StringFunctions() =
        Assert.AreEqual(strR "Hello, World!", runIt "(cn \"Hello, \" \"World!\")")
        Assert.AreEqual(strR "Hello", runIt "(cn (n->string (string->n \"Hello\")) (tlstr \"Hello\"))")
        Assert.AreEqual(strR "1", runIt "(str 1)")
        
    [<TestMethod>]
    member this.Vectors() =
        Assert.IsTrue(arrayEqual Array.empty<KlValue> (runIt "(absvector 0)" |> rVector))
        Assert.IsTrue(arrayEqual [|EmptyValue|] (runIt "(absvector 1)" |> rVector))
        Assert.IsTrue(arrayEqual [|BoolValue true|] (runIt "(address-> (absvector 1) 0 true)" |> rVector))

    [<TestMethod>]
    member this.Conses() =
        Assert.AreEqual(trueR, runIt "(= () (tl (cons 1 ())))")
        Assert.AreEqual(falseR, runIt "(cons? ())")
        Assert.AreEqual(falseR, runIt "(cons? 0)")
        Assert.AreEqual(trueR, runIt "(cons? (cons 0 0))")

    [<TestMethod>]
    member this.EvalFunction() =
        Assert.AreEqual(intR 3, runIt "(eval-kl (cons + (cons 1 (cons 2 ()))))") // (+ 1 2)
        let inc = (runIt >> rFunc) "(eval-kl (cons lambda (cons X (cons (cons + (cons 1 (cons X ()))) ()))))" // (lambda X (+ 1 X))
        Assert.AreEqual(intR 5, (inc.Apply[intV 4]) |> go)

    [<TestMethod>]
    member this.SanityChecks() =
        Assert.AreEqual(BoolToken true, BoolToken true)
        Assert.AreEqual(BoolExpr true, BoolExpr true)
        Assert.AreEqual(BoolValue true, BoolValue true) // this was failing when KlValue had a case containing a function type
        Assert.AreEqual(true |> BoolValue |> ValueResult, true |> BoolValue |> ValueResult) // this might start failing for the same reason
        Assert.AreEqual(true |> BoolValue |> ValueResult |> Completed, true |> BoolValue |> ValueResult |> Completed)

    [<TestMethod>]
    member this.``deep-running tail-recursive function does not stack overflow``() =
        let env = baseEnv ()
        runInEnv env "(defun fill (vec start stop val) (if (= stop start) (address-> vec start val) (fill (address-> vec start val) (+ 1 start) stop val)))" |> ignore
        let x = runInEnv env "(fill (absvector 20000) 0 19999 0)"
        ()
    
    [<TestMethod>]
    member this.``deep-running mutually-recursive functions do not stack overflow``() =
        let env = baseEnv ()
        runInEnv env "(defun add (x y) (if (= 0 x) y (add2 (- x 1) (+ y 1))))" |> ignore
        runInEnv env "(defun add2 (x y) (if (= 0 x) y (add (- x 1) (+ y 1))))" |> ignore
        Assert.AreEqual(intR 40000, runInEnv env "(add 20000 20000)")

    [<TestMethod>]
    member this.HeadTailPositionsParsed() =
        let e = "(defun ! (acc n) (if (= 0 n) acc (! (* n acc) (- n 1))))" |> tokenize |> parse Head
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

    [<TestMethod>]
    member this.``string index out of bounds should cause uncaught error``() =
        Assert.IsTrue(runIt "(pos \"\" 0)" |> isUncaught)
        Assert.IsTrue(runIt "(pos \"hello\" 5)" |> isUncaught)

    [<TestMethod>]
    member this.``vector index out of bounds should cause uncaught error``() =
        Assert.IsTrue(runIt "(<-address (absvector 0) 0)" |> isUncaught)

    [<TestMethod>]
    member this.``simple-error should cause uncaught error``() =
        Assert.AreEqual("whoops", "(simple-error \"whoops\")" |> runIt |> rUncaught)

    [<TestMethod>]
    member this.``simple-error should be caught by trap-error``() =
        Assert.AreEqual(ValueResult EmptyValue, runIt "(trap-error (simple-error \"whoops\") (lambda E ()))")

    [<TestMethod>]
    member this.``trap-error should prevent uncaught error from propogating``() =
        Assert.AreEqual(ValueResult EmptyValue, runIt "(trap-error (pos \"\" 0) (lambda E ()))")

    [<TestMethod>]
    member this.``trap-error should eval and apply second expression if eval of first results in uncaught error``() =
        let env = baseEnv ()
        runInEnv env "(defun do (X Y) Y)" |> ignore
        Assert.IsTrue(runInEnv env "(trap-error (do (pos \"\" 0) false) (lambda E true))" |> rBool)

    [<TestMethod>]
    member this.``error message should be preserved when error is caught and handled``() =
        Assert.AreEqual(strR "hi", runIt "(trap-error (simple-error \"hi\") (lambda E (error-to-string E)))")

    [<TestMethod>]
    member this.PrintStuff() =
        "(get-time unix)" |> runIt |> rInt |> printfn "Unix time: %i"
        "(get-time run)" |> runIt |> rInt |> printfn "Run time: %i"
        "(str (cons 1 (cons 2 (cons 3 ()))))" |> runIt |> rString |> printfn "Cons: %s"
        "(str (address-> (address-> (address-> (absvector 3) 0 1) 1 2) 2 3))" |> runIt |> rString |> printfn "Vector: %s"
        "(trap-error (simple-error \"whoops\") (lambda E E))" |> runIt |> rError |> printfn "Error: %s"
        "(str (trap-error (simple-error \"whoops\") (lambda Ex Ex)))" |> runIt |> rString |> printfn "Error-string: %s"

    [<TestMethod>]
    member this.CompilerPrintStuff() =
        let p = KlTokenizer.tokenize >> KlParser.parse Head >> KlCompiler.compile >> str >> printfn "%s"
        p "1"
        p "true"
        p "\"abc\""
        p "abc"
        p "(lambda E 0)"
        p "(let X 0 0)"
        p "(and true true)"
