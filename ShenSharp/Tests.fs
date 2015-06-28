namespace ShenPOF

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec

[<TestClass>]
type Tests() =

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
                | Success(result, _, _)   -> Assert.AreEqual(e, result |> KlParser.parse |> KlEvaluator.eval (new Context()))
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
                                                    | [BoolValue b] -> BoolValue (not b)
                                                    | _ -> raise <| new System.Exception("must be bool"))))
        tpeval "(defun xor (l r) (or (and l (not r)) (and (not l) r)))" |> ignore
        Assert.AreEqual(BoolValue true, tpeval "(xor true false)")

        // testing symbol resolution
        let c2 = new Context()
        let tpeval2 s = s |> KlTokenizer.tokenize |> KlParser.parse |> KlEvaluator.eval c2
        let symbolP = new Function(1, function
                                      | [SymbolValue _] -> BoolValue true
                                      | _ -> BoolValue false)
        c2.Add("symbol?", FunctionValue symbolP)
        Assert.AreEqual(BoolValue true, tpeval2 "(symbol? run)")
        c2.Add("id", FunctionValue (new Function(1, function | [x] -> x; | _ -> raise <| new System.Exception("must be 1 arg"))))
        Assert.AreEqual(BoolValue true, tpeval2 "(symbol? (id run))")

        // function has partial application built-in; this is not what will be typical
        // partial application needs to be automatic for all functions
        let rec add = new Function(2, function
                                  | [NumberValue x] -> FunctionValue (new Function(1, function
                                                                            | [NumberValue y] -> NumberValue (x + y)
                                                                            | _ -> raise <| new System.Exception("must be two numbers")))
                                  | [NumberValue x; NumberValue y] -> NumberValue (x + y)
                                  | _ -> raise <| new System.Exception("must be two numbers"))

        let context = new Context()
        context.Add("+", FunctionValue add)

        Assert.AreEqual(
            NumberValue 3.0,
            KlEvaluator.eval context
                             (AppExpr (SymbolExpr "+",
                                      [(NumberExpr 1.0); (NumberExpr 2.0)])))
        Assert.AreEqual(
            NumberValue 3.0,
            KlEvaluator.eval context
                             (AppExpr (AppExpr (SymbolExpr "+", [NumberExpr 1.0]),
                                               [NumberExpr 2.0])))

        // testing application and partial application for built-in functions
        let context2 = KlBuiltins.baseContext
        Assert.AreEqual(
            NumberValue 3.0,
            KlEvaluator.eval context2
                             (AppExpr (SymbolExpr "+",
                                      [(NumberExpr 1.0); (NumberExpr 2.0)])))
        Assert.AreEqual(
            NumberValue 3.0,
            KlEvaluator.eval context2
                             (AppExpr (AppExpr (SymbolExpr "+", [NumberExpr 1.0]),
                                               [NumberExpr 2.0])))

    [<TestMethod>]
    member this.Builtins() =
        let run = KlTokenizer.tokenize >> KlParser.parse >> KlEvaluator.eval KlBuiltins.baseContext
        Assert.AreEqual(NumberValue 3.0, run "((+ 1) 2)")
        Assert.AreEqual(NumberValue 2.0, run "((- 4) 2)")
        Assert.AreEqual(BoolValue false, run "(cons? ())")
        Assert.AreEqual(BoolValue false, run "(cons? 0)")
        Assert.AreEqual(BoolValue true, run "(cons? (cons 0 0))")
    
    [<TestMethod>]
    member this.SanityChecks() =
        Assert.AreEqual(BoolToken true, BoolToken true)
        Assert.AreEqual(BoolExpr true, BoolExpr true)
        Assert.AreEqual(BoolValue true, BoolValue true) // this was failing when KlValue had a case containing a function type
