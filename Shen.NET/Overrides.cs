using System;

namespace Shen.NET
{
    public static class Overrides
    {
        public static Context Install(Context c)
        {
            var trueSymbol = c.Intern("true");
            var falseSymbol = c.Intern("false");
            var tupleSymbol = c.Intern("shen.tuple");
            c.Define("variable?", (Object x) => x is Symbol && Char.IsUpper(x.As<Symbol>().Name[0]));
            c.Define("boolean?", (Object x) => x == trueSymbol || x == falseSymbol);
            c.Define("symbol?", (Object x) => x is Symbol && !(x == trueSymbol || x == falseSymbol));
            c.Define("@p", (Object x, Object y) => new Vector(tupleSymbol, x, y));
            c.Define("hash", (Object obj, Decimal limit) =>
                {
                    var hash = obj.GetHashCode();
                    return hash == 0 ? 1 : hash % limit;
                });
            c.Define("length", (Cons cons) => cons.Length);
            c.Define("shen.fillvector", (Vector vector, Decimal stop, Decimal start, Object fillValue) =>
                vector.Fill(start.AsInt(), stop.AsInt(), fillValue));
            return c;
        }
    }
}
