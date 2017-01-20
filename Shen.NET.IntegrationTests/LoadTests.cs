using System;
using System.IO;
using System.Linq;

namespace Shen.NET.IntegrationTests
{
    class LoadTests
    {
        static void Main()
        {
            try
            {
                var tests = new LoadTests();
                Console.Write("StreamFile... ");
                tests.StreamFile();
                Console.WriteLine("OK.");
                Console.Write("LoadKl... ");
                tests.LoadKl();
                Console.WriteLine("Benchmarks...");

                Console.WriteLine("OK.");
            }
            catch (Exception e)
            {
                Console.WriteLine();
                Console.WriteLine(e);
            }

            Console.WriteLine("Press any key to continue...");
            Console.ReadKey();
        }

        public void StreamFile()
        {
            File.WriteAllBytes(Path.Combine(Environment.CurrentDirectory, "testFile.txt"), new byte[] { 45, 255, 123 });
            var c = Builtins.Install(Context.NewGlobal());
            Run("(set fstream (open \"testFile.txt\" in)))", c);
            var s = c.Intern("fstream").Value.As<Stream>();
            Func<Object> rb = () => Run("(read-byte (value fstream))", c);
            Assert.IsTrue(s.CanRead);
            Assert.AreEqual(45m, rb());
            Assert.AreEqual(255m, rb());
            Assert.AreEqual(123m, rb());
            Assert.AreEqual(-1m, rb());
            Run("(close (value fstream))", c);
            Assert.IsFalse(s.CanRead);
        }

        public Context LoadKl()
        {
            var globals = Context.NewGlobal();
            Builtins.Install(globals);

            var klFolder = Path.Combine(Environment.CurrentDirectory, @".\..\..\..\klambda\");
            var klFiles = new[] {
                "toplevel", "core", "sys", "sequent", "yacc", "reader", "prolog",
                "track", "load", "writer", "macros", "declarations", "t-star", "types"
            };

            foreach (var klFile in klFiles)
            {
                var tokens = Syntax.ReadFile(Path.Combine(klFolder, klFile + ".kl"));
                var exprs = tokens.Select(x => x.Parse(Position.Tail));

                foreach (var expr in exprs)
                {
                    Console.WriteLine(expr.Token);
                    expr.Eval(globals, Position.Head);
                }
            }

            return globals;
        }

        private static object Run(string code, Context context = null)
        {
            return Syntax.Read(code).Parse(Position.Tail).Eval(context ?? Builtins.Install(Context.NewGlobal()), Position.Head);
        }
    }

    class Assert
    {
        public static void IsTrue(bool b)
        {
            if (!b)
                throw new Exception("not true");
        }

        public static void IsFalse(bool b)
        {
            if (b)
                throw new Exception("not false");
        }

        public static void AreEqual(Object a, Object b)
        {
            if (!Equals(a, b))
                throw new Exception("not equal");
        }
    }
}
