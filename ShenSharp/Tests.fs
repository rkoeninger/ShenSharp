namespace ShenPOF

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec

[<TestClass>]
type Tests() =

    let runit = KlTokenizer.tokenize >> KlParser.parse >> KlEvaluator.eval KlBuiltins.baseContext
    let getError = function
        | ValueResult (ErrorValue e) -> e
        | _ -> raise (new System.Exception("not an Error"))
    let getNumber = function
        | ValueResult (NumberValue n) -> n
        | _ -> raise (new System.Exception("not a Number"))
    let getString = function
        | ValueResult (StringValue s) -> s
        | _ -> raise (new System.Exception("not a String"))
    let getUncaught = function
        | ErrorResult (Uncaught s) -> s
        | _ -> raise (new System.Exception("not an Uncaught"))

    [<TestMethod>]
    member this.TokenizerTest() =
        let tryit s = 
            match run KlTokenizer.pKlToken s with
                | Success(result, _, _)   -> ()
                | Failure(errorMsg, _, _) -> Assert.Fail(errorMsg)

        tryit "abc"
        tryit "123"
        tryit "()"
        tryit "(a)"
        tryit "(a b)"
        tryit "((a b) c (d e f) ((a) ()) c (d f))"
        tryit "(a     b)"

        // extra space tests
        // these aren't working
        // parse fails when there is extra space between
        // open paren and first child
        // or last child and close paren
        //tryit "(    )"
        //tryit "(   a)"
        //tryit "(a  )"
        //tryit "(   a   b  c   )"

    [<TestMethod>]
    member this.ParserTest() =
        let tryit s e = 
            match run KlTokenizer.pKlToken s with
                | Success(result, _, _)   -> Assert.AreEqual(e, KlParser.parse result)
                | Failure(errorMsg, _, _) -> Assert.Fail(errorMsg)

        tryit "2" (NumberExpr 2.0)
        tryit "()" EmptyExpr
        tryit "(add 1 2)" (AppExpr (SymbolExpr "add", [NumberExpr 1.0; NumberExpr 2.0]))

    [<TestMethod>]
    member this.EvaluatorTest() =
        let tryit s e = 
            match run KlTokenizer.pKlToken s with
                | Success(result, _, _)   -> Assert.AreEqual(ValueResult e, result |> KlParser.parse |> KlEvaluator.eval (new Context()))
                | Failure(errorMsg, _, _) -> Assert.Fail(errorMsg)

        // some basic expressions using special forms and literals only
        tryit "()" EmptyValue
        tryit "true" (BoolValue true)
        tryit "2" (NumberValue 2.0)
        tryit "(and true true)" (BoolValue true)
        tryit "(and true false)" (BoolValue false)
        tryit "(and false true)" (BoolValue false)
        tryit "(and false false)" (BoolValue false)
        tryit "(or true true)" (BoolValue true)
        tryit "(or true false)" (BoolValue true)
        tryit "(or false true)" (BoolValue true)
        tryit "(or false false)" (BoolValue false)

        // testing defun and resolution of defined functions
        let c = new Context()
        let tpeval s = s |> KlTokenizer.tokenize |> KlParser.parse |> KlEvaluator.eval c

        c.Add("not", FunctionValue (new Function(1, function
                                                    | [BoolValue b] -> not b |> BoolValue |> ValueResult
                                                    | _ -> raise <| new System.Exception("must be bool"))))
        tpeval "(defun xor (l r) (or (and l (not r)) (and (not l) r)))" |> ignore
        Assert.AreEqual(BoolValue true |> ValueResult, tpeval "(xor true false)")

        // testing symbol resolution
        let c2 = new Context()
        let tpeval2 s = s |> KlTokenizer.tokenize |> KlParser.parse |> KlEvaluator.eval c2
        let symbolP = new Function(1, function
                                      | [SymbolValue _] -> BoolValue true |> ValueResult
                                      | _ -> BoolValue false |> ValueResult)
        c2.Add("symbol?", FunctionValue symbolP)
        Assert.AreEqual(BoolValue true |> ValueResult, tpeval2 "(symbol? run)")
        c2.Add("id", FunctionValue (new Function(1, function | [x] -> ValueResult x; | _ -> raise <| new System.Exception("must be 1 arg"))))
        Assert.AreEqual(BoolValue true |> ValueResult, tpeval2 "(symbol? (id run))")

        // function has partial application built-in; this is not what will be typical
        // partial application needs to be automatic for all functions
        let rec add = new Function(2, function
                                  | [NumberValue x] -> FunctionValue (new Function(1, function
                                                                            | [NumberValue y] -> NumberValue (x + y) |> ValueResult
                                                                            | _ -> raise <| new System.Exception("must be two numbers"))) |> ValueResult
                                  | [NumberValue x; NumberValue y] -> NumberValue (x + y) |> ValueResult
                                  | _ -> raise <| new System.Exception("must be two numbers"))

        let context = new Context()
        context.Add("+", FunctionValue add)

        Assert.AreEqual(
            NumberValue 3.0 |> ValueResult,
            KlEvaluator.eval context
                             (AppExpr (SymbolExpr "+",
                                      [(NumberExpr 1.0); (NumberExpr 2.0)])))
        Assert.AreEqual(
            NumberValue 3.0 |> ValueResult,
            KlEvaluator.eval context
                             (AppExpr (AppExpr (SymbolExpr "+", [NumberExpr 1.0]),
                                               [NumberExpr 2.0])))

        // testing application and partial application for built-in functions
        let context2 = KlBuiltins.baseContext
        Assert.AreEqual(
            NumberValue 3.0 |> ValueResult,
            KlEvaluator.eval context2
                             (AppExpr (SymbolExpr "+",
                                      [(NumberExpr 1.0); (NumberExpr 2.0)])))
        Assert.AreEqual(
            NumberValue 3.0 |> ValueResult,
            KlEvaluator.eval context2
                             (AppExpr (AppExpr (SymbolExpr "+", [NumberExpr 1.0]),
                                               [NumberExpr 2.0])))

    [<TestMethod>]
    member this.Builtins() =
        Assert.AreEqual(NumberValue 3.0 |> ValueResult, runit "((+ 1) 2)")
        Assert.AreEqual(NumberValue 2.0 |> ValueResult, runit "((- 4) 2)")
        Assert.AreEqual(BoolValue false |> ValueResult, runit "(cons? ())")
        Assert.AreEqual(BoolValue false |> ValueResult, runit "(cons? 0)")
        Assert.AreEqual(BoolValue true |> ValueResult, runit "(cons? (cons 0 0))")
    
    [<TestMethod>]
    member this.StringFunctions() =
        Assert.AreEqual("Hello, World!", runit "(cn \"Hello, \" \"World!\")" |> getString)
        Assert.AreEqual("Hello", runit "(cn (n->string (string->n \"Hello\")) (strtl \"Hello\"))" |> getString)
        Assert.AreEqual("1", runit "(str 1)" |> getString)

    [<TestMethod>]
    member this.EvalFunction() =
        Assert.AreEqual(3, runit "(eval-kl (cons + (cons 1 (cons 2 ()))))" |> getNumber) // the atom `/` is evaluating to a function instead of a symbol

    [<TestMethod>]
    member this.SanityChecks() =
        Assert.AreEqual(BoolToken true, BoolToken true)
        Assert.AreEqual(BoolExpr true, BoolExpr true)
        Assert.AreEqual(BoolValue true, BoolValue true) // this was failing when KlValue had a case containing a function type
        Assert.AreEqual(BoolValue true |> ValueResult, BoolValue true |> ValueResult) // this might start failing for the same reason

    [<TestMethod>]
    member this.SimpleError() =
        let s = runit "(simple-error \"whoops\")" |> getUncaught
        Assert.AreEqual("whoops", s)

    [<TestMethod>]
    member this.PrintStuff() =
        let u = runit "(get-time unix)" |> getNumber
        printf "Unix time: %f" u
        printf "\r\n"
        let r = runit "(get-time run)" |> getNumber
        printf "Run time: %f" r
        printf "\r\n"
        let s = runit "(str (cons 1 (cons 2 (cons 3 ()))))" |> getString
        printf "Cons: %s" s
        printf "\r\n"
        let s = runit "(str (address-> (address-> (address-> (absvector 3) 0 1) 1 2) 2 3))" |> getString
        printf "Vector: %s" s
        printf "\r\n"
        let s = runit "(trap-error (simple-error \"whoops\") (lambda E E))" |> getError
        printf "Error: %s" s
        printf "\r\n"
        let s = runit "(str (trap-error (simple-error \"whoops\") (lambda Ex Ex)))" |> getString
        printf "Error-string: %s" s
        printf "\r\n"
