using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;

namespace Shen.NET.UnitTests
{
    [TestClass]
    public class ShenTests
    {
        [TestMethod]
        public void LexerBasicSyntax()
        {
            Assert.IsInstanceOfType(Syntax.Read("1234"), typeof(Atom));
            Assert.IsInstanceOfType(Syntax.Read(" \n 1234"), typeof(Atom));
            Assert.AreEqual(2, Syntax.Read(" \n 1234").Location.Line);
            Assert.IsInstanceOfType(Syntax.Read(" 1234 \r"), typeof(Atom));
            Assert.AreEqual(1, Syntax.Read(" 1234 \r").Location.Line);
            Assert.AreEqual("abc", ((Atom) Syntax.Read("   \t\r  abc  \r\n ")).Literal);
        }

        [TestMethod]
        public void LexerNoisySyntax()
        {
            var g = Syntax.Read(" ( \n abfgj \r\t 345g4g \t\n (sdfv wergwv3) \"ef dfv\nsdf\" (234\t54   3   (  ))   \n\n ) ");
            Assert.AreEqual("abfgj", g.As<Combo>().Tokens[0].As<Atom>().Literal);
            Assert.AreEqual("345g4g", g.As<Combo>().Tokens[1].As<Atom>().Literal);
            Assert.AreEqual("sdfv", g.As<Combo>().Tokens[2].As<Combo>().Tokens[0].As<Atom>().Literal);
            Assert.AreEqual("wergwv3", g.As<Combo>().Tokens[2].As<Combo>().Tokens[1].As<Atom>().Literal);
            Assert.AreEqual("\"ef dfv\nsdf\"", g.As<Combo>().Tokens[3].As<Atom>().Literal);
            Assert.AreEqual("234", g.As<Combo>().Tokens[4].As<Combo>().Tokens[0].As<Atom>().Literal);
            Assert.AreEqual("54", g.As<Combo>().Tokens[4].As<Combo>().Tokens[1].As<Atom>().Literal);
            Assert.AreEqual("3", g.As<Combo>().Tokens[4].As<Combo>().Tokens[2].As<Atom>().Literal);
            Assert.AreEqual(0, g.As<Combo>().Tokens[4].As<Combo>().Tokens[3].As<Combo>().Tokens.Count);
        }

        [TestMethod]
        public void ParserCleanSyntax()
        {
            var e = Syntax.Read("(defun shen.add-macro (V1807) (set *macros* (adjoin V1807 (value *macros*))))");
            var f = e.Parse(Position.Tail);
            Assert.IsInstanceOfType(f, typeof(DefunExpr));
            var g = f.As<DefunExpr>();
            Assert.AreEqual("shen.add-macro", g.Name.Id);
            Assert.AreEqual(1, g.Params.Count);
            Assert.AreEqual("V1807", g.Params[0].Id);
            Assert.IsInstanceOfType(g.Body, typeof(AppExpr));
            var h = g.Body.As<AppExpr>();
            Assert.IsInstanceOfType(h.Functor, typeof(SymbolExpr));
            Assert.AreEqual("set", h.Functor.As<SymbolExpr>().Id);
            Assert.AreEqual(2, h.Args.Count);
            Assert.AreEqual("*macros*", h.Args[0].As<SymbolExpr>().Id);
            Assert.IsInstanceOfType(h.Args[1], typeof(AppExpr));
        }

        [TestMethod]
        public void Apps()
        {
            Assert.AreEqual(3m, Run("(+ 1 2)"));
            Assert.AreEqual(18.5m, Run("(- (* 4 5) (/ 3 2))"));
        }

        [TestMethod]
        public void PartialApps()
        {
            Assert.IsInstanceOfType(Run("(+ 1)"), typeof(IFunction));
            Assert.AreEqual(3m, Run("((+ 1) 2)"));
        }

        [TestMethod]
        public void Lambdas()
        {
            var k = Run("(lambda z (tlstr z))");
            Assert.IsInstanceOfType(k, typeof(IFunction));
            Assert.AreEqual("ello", k.As<IFunction>().Apply(new object[] {"hello"}, Position.Head));
        }

        [TestMethod]
        public void Lets()
        {
            Assert.AreEqual(-3m, Run("(let x 5 (- 2 x))"));
            Assert.IsTrue(Run("(let x \"abc\" (= x \"abc\"))").As<Boolean>());
        }

        [TestMethod]
        public void DefunPartialApps()
        {
            var j = Builtins.Install(Context.NewGlobal());
            Run("(defun somefunc (x y z) (+ (* x y) (/ z y)))", j);
            var somefunc = j.Intern("somefunc").Value;
            var somefunc2 = somefunc.As<IFunction>().Apply(new object[] {5m, -2m}, Position.Head);
            Assert.IsInstanceOfType(somefunc2, typeof(IFunction));
            Assert.AreEqual(-60m, somefunc2.As<IFunction>().Apply(new object[] {100m}, Position.Head));
        }

        [TestMethod]
        public void FreezeSetInterns()
        {
            var n = Builtins.Install(Context.NewGlobal());
            n.Intern("abc").Value = 0m;
            var m = Run("(freeze (set abc (+ 1 (value abc))))", n);
            Assert.AreEqual(1m, m.As<IFunction>().Apply(new object[0], Position.Head));
            Assert.AreEqual(2m, m.As<IFunction>().Apply(new object[0], Position.Head));
            Assert.AreEqual(3m, m.As<IFunction>().Apply(new object[0], Position.Head));
            n.Intern("do-it").Value = m;
            Assert.AreEqual(4m, Run("(do-it)", n));
            Assert.AreEqual(5m, Run("(do-it)", n));
        }

        [TestMethod]
        public void IfAndOrConds()
        {
            Assert.AreEqual(1m, Run("(let x true (if x 1 2))"));
            Assert.AreEqual(true, Run("(or true (set abc 0))"));
            Assert.AreEqual(false, Run("(and false (set def 0))"));
            Assert.AreEqual(3m, Run("(cond ((and true false) 1) ((= 0 1) 2) ((or false true) 3) ((blowup 0) 4))"));
        }

        [TestMethod]
        public void Vectors()
        {
            var x = Run("(address-> (address-> (address-> (absvector 3) 0 1) 1 2) 2 3)");
            var y = Run("(address-> (address-> (address-> (absvector 3) 0 1) 1 2) 2 3)");
            Assert.AreEqual(x, y);
            Assert.IsTrue(Run("(= (address-> (absvector 3) 0 1) (address-> (absvector 3) 0 1))").As<Boolean>());
        }

        [TestMethod]
        public void Types()
        {
            Assert.AreEqual(typeof(String), Run("(type \"a\")"));
            Assert.AreEqual(typeof(Boolean), Run("(type true)"));
            Assert.AreEqual(typeof(Decimal), Run("(type 5)"));
            Assert.AreEqual(typeof(Defun), Run("(type (freeze (+ 1 2)))"));
            Assert.AreEqual(typeof(Defun), Run("(type (lambda x 5))"));
        }

        [TestMethod]
        public void Errors()
        {
            Try.Expect<DivideByZeroException>(() => Run("(/ 3 0)"));
            Assert.AreEqual("Attempted to divide by zero.", Run("(trap-error (/ 3 0) (lambda e (error-to-string e)))"));
            Assert.AreEqual("Attempted to divide by zero.", Run("(trap-error (/ 3 0) error-to-string)"));
            Try.Expect<IndexOutOfRangeException>(() => Run("(<-address (absvector 3) 6)"));
        }

        [TestMethod]
        public void Conses()
        {
            var list = Run("(cons 1 (cons 2 (cons 3 (cons 4 ()))))").As<Cons>().ToList();
            Assert.AreEqual(4, list.Count);
            foreach (var pair in list.Zip(new[] { 1m, 2m, 3m, 4m }, Tuple.Create))
                Assert.AreEqual(pair.Item1, pair.Item2);
            Assert.AreEqual(Cons.EmptyList, Run("(cons () ())").As<Cons>().Single());
            Assert.AreEqual(new Cons(new Cons(3m, new Cons(4m, Cons.EmptyList)), new Cons(2m, new Cons(1m, Cons.EmptyList))),
                Run("(cons (cons 3 (cons 4 ())) (cons 2 (cons 1 ())))"));
        }

        [TestMethod]
        public void EvalKl()
        {
            Assert.AreEqual(5.5m, Run("(eval-kl (cons / (cons 11 (cons 2 ()))))"));
        }

        [TestMethod]
        public void Symbols()
        {
            Assert.IsTrue(Run("(= abc (intern \"abc\"))").As<Boolean>());
            Assert.IsFalse(Run("(= (intern \"def\") ())").As<Boolean>());
        }

        [TestMethod]
        public void Scoping()
        {
            var context = Builtins.Install(Context.NewGlobal());
            Run("(set abc (freeze (= x 5)))", context);
            Assert.IsFalse(Run("(let x 5 (abc))", context).As<Boolean>());
            Assert.IsInstanceOfType(Run("abc", context), typeof(Symbol));
            Assert.IsInstanceOfType(Run("abc", context).As<Symbol>().Value, typeof(IFunction));
            Assert.IsInstanceOfType(Run("(abc)", context), typeof(Boolean));
        }

        [TestMethod]
        public void Recursion()
        {
            var context = Builtins.Install(Context.NewGlobal());
            Run("(defun ! (n acc) (if (= 0 n) acc (! (- n 1) (* acc n))))", context);
            Assert.AreEqual(120m, Run("(! 5 1)", context));
            Assert.AreEqual(120m, Run(@"((let Y
                (lambda f ((lambda r (r r)) (lambda g (f (lambda x ((g g) x))))))
                (Y (lambda f (lambda x (if (< x 2) 1 (* x (f (- x 1))))))))
                5)"));
        }

        [TestMethod]
        public void TailAnnotations()
        {
            var a = Syntax.Read("(lambda x (lambda y (+ x y)))").Parse(Position.Tail);
            Assert.IsTrue(a.IsTail);
            Assert.IsTrue(a.As<LambdaExpr>().Body.IsTail);
            Assert.IsTrue(a.As<LambdaExpr>().Body.As<LambdaExpr>().Body.IsTail);

            var b = Syntax.Read("(and *abc* *xyz*)").Parse(Position.Tail);
            Assert.IsTrue(b.IsTail);
            Assert.IsFalse(b.As<AndExpr>().First.IsTail);
            Assert.IsFalse(b.As<AndExpr>().Second.IsTail);
        }

        [TestMethod]
        public void TailCallOptimization()
        {
            var context = Builtins.Install(Context.NewGlobal());
            Run("(defun ! (n acc) (if (= 0 n) acc (! (- n 1) (* acc n))))", context);
            var a = Syntax.Read("(! 2 1)").Parse(Position.Tail).Eval(context, Position.Tail);
            var b = a.As<Thunk>().Eval();
            var c = b.As<Thunk>().Eval();
            c.As<Thunk>().Eval();
        }

        private static object Run(string code, Context context = null)
        {
            return Syntax.Read(code).Parse(Position.Tail).Eval(context ?? Builtins.Install(Context.NewGlobal()), Position.Head);
        }
    }
}
