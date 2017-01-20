using System;
using System.IO;
using System.Linq;
using System.Reflection;

namespace Shen.NET
{
    public static class Builtins
    {
        public static Context Install(Context c, DateTime? startTime = null, Stream sin = null, Stream sout = null)
        {
            sin = sin ?? Console.OpenStandardInput();
            sout = sout ?? Console.OpenStandardOutput();
            var start = startTime ?? DateTime.UtcNow;

            c.Define("*language*", "C# " + Assembly.GetAssembly(typeof(object)).GetName().Version);
            c.Define("*implementation*", "CLR " + Environment.Version);
            c.Define("*release*", "4.5");
            c.Define("*port*", "0.0");
            c.Define("*porters*", "Robert Koeninger");
            c.Define("*version*", "13.2.1");
            c.Define("*stinput*", sin);
            c.Define("*stoutput*", sout);
            c.Define("*home-directory*", Environment.CurrentDirectory);
            c.Define("intern", (String x) => c.Global.Intern(x));
            c.Define("pos", (String str, Decimal n) => str.Substring(n.AsInt(), 1));
            c.Define("tlstr", (String str) => str.Substring(1));
            c.Define("cn", (String x, String y) => x + y);
            c.Define("str", (Object x) => x.ToString());
            c.Define("string?", (Object x) => x is String);
            c.Define("n->string", (Decimal n) => n.AsString());
            c.Define("string->n", (String str) => str.First().AsNumber());
            c.Define("cons", (Object x, Object y) => new Cons(x, y));
            c.Define("hd", (Cons x) => x.Car);
            c.Define("tl", (Cons x) => x.Cdr);
            c.Define("cons?", (Object x) => x is Cons && ! ReferenceEquals(Cons.EmptyList, x));
            c.Define("=", (Object x, Object y) => Equals(x, y));
            c.Define("type", (Object x) => x.GetType());
            c.Define("absvector", (Decimal size) => new Vector(size, c));
            c.Define("<-address", (Vector vec, Decimal n) => vec[n]);
            c.Define("address->", (Vector vec, Decimal n, Object val) =>
                {
                    vec[n] = val;
                    return vec;
                });
            c.Define("absvector?", (Object x) => x is Vector);
            c.Define("set", (Symbol sym, Object val) => sym.Value = val);
            c.Define("value", (Symbol sym) => sym.Value);
            c.Define("simple-error", (String message) => { throw new ApplicationException(message); });
            c.Define("error-to-string", (Error e) => e.Message);
            c.Define("get-time", (Symbol x) =>
                Math.Floor((DateTime.UtcNow - (x.Name == "unix" ? Constants.Epoch : start)).TotalSeconds).As<Decimal>());
            c.Define("write-byte", (Decimal b, Stream stream) =>
                {
                    stream.WriteByte(b.AsByte());
                    stream.Flush();
                    return b;
                });
            c.Define("read-byte", (Stream stream) => (Decimal) stream.ReadByte());
            c.Define("open", (String path, Symbol direction) =>
                {
                    if (!Path.IsPathRooted(path))
                        path = Path.Combine(c.Intern("*home-directory*").Value.ToString(), path);

                    if (direction.Name == "in") return File.OpenRead(path);
                    if (direction.Name == "out") return File.OpenWrite(path);

                    throw new Exception("Invalid stream direction");
                });
            c.Define("close", (Stream stream) =>
                {
                    stream.Close();
                    return Cons.EmptyList;
                });
            c.Define("+", (Decimal x, Decimal y) => x + y);
            c.Define("-", (Decimal x, Decimal y) => x - y);
            c.Define("*", (Decimal x, Decimal y) => x * y);
            c.Define("/", (Decimal x, Decimal y) => x / y);
            c.Define(">", (Decimal x, Decimal y) => x > y);
            c.Define("<", (Decimal x, Decimal y) => x < y);
            c.Define(">=", (Decimal x, Decimal y) => x >= y);
            c.Define("<=", (Decimal x, Decimal y) => x <= y);
            c.Define("number?", (Object x) => x is Decimal);
            c.Define("eval-kl", (Cons kl) => kl.ToToken().Parse(Position.Tail).Eval(c, Position.Head));
            c.Define("function", (Symbol s) => s.As<IFunction>());
            return c;
        }
    }
}
