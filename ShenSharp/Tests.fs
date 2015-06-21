namespace ShenPOF

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec

[<TestClass>]
type Tests() =

    [<TestMethod>]
    member this.TokenizerTest() =
        let tryit s = 
            match run KlTokenizer.pKlToken s with
                | Success(result, _, _)   -> printfn "Good"
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
                | Success(result, _, _)   -> Assert.AreEqual(e, result |> KlParser.parse |> KlEvaluator.eval (Map.empty))
                | Failure(errorMsg, _, _) -> Assert.Fail(errorMsg)

        tryit "()" EmptyValue
        tryit "true" (BoolValue true)
        tryit "2" (NumberValue 2.0)

        let rec add = new Function(2, function
                                  | [NumberValue x] -> FunctionValue (new Function(1, function
                                                                            | [NumberValue y] -> add.Apply([NumberValue x; NumberValue y])
                                                                            | _ -> raise <| new System.Exception("must be two numbers")))
                                  | [NumberValue x; NumberValue y] -> NumberValue (x + y)
                                  | _ -> raise <| new System.Exception("must be two numbers"))
        Assert.AreEqual(
            NumberValue 3.0,
            KlEvaluator.eval (Map.ofList [("+", FunctionValue add)])
                             (AppExpr (SymbolExpr "+",
                                      [(NumberExpr 1.0); (NumberExpr 2.0)])))
        Assert.AreEqual(
            NumberValue 3.0,
            KlEvaluator.eval (Map.ofList [("+", FunctionValue add)])
                             (AppExpr (AppExpr (SymbolExpr "+", [NumberExpr 1.0]), [NumberExpr 2.0])))

    [<TestMethod>]
    member this.BasicStuff() =
        Assert.AreEqual(BoolToken true, BoolToken true)
        Assert.AreEqual(BoolExpr true, BoolExpr true)
        Assert.AreEqual(BoolValue true, BoolValue true)
