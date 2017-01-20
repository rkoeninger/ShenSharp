using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Shen.NET
{
    public static class Extensions
    {
        [DebuggerStepThrough]
        public static T As<T>(this Object obj)
        {
            if (! (obj is T))
                throw new Exception("{" + obj + "} is not of type " + typeof(T).Name);

            return (T) obj;
        }
        
        [DebuggerStepThrough]
        public static byte AsByte(this Decimal dec)
        {
            if (dec % 1.0m != 0.0m || dec < 0.0m || dec > 255.0m)
                throw new Exception("Number {" + dec + "} is not a byte");

            return (byte) dec;
        }
        
        [DebuggerStepThrough]
        public static int AsInt(this Decimal dec)
        {
            if (dec % 1.0m != 0.0m)
                throw new Exception("Number {" + dec + "} is not an integer");

            return (int) dec;
        }
        
        [DebuggerStepThrough]
        public static String AsString(this Decimal dec)
        {
            if ((dec % 1.0m != 0.0m) || (dec < 0))
                throw new Exception("Number {" + dec + "} is not a valid code point");

            return ((char) (int) dec) + "";
        }
        
        [DebuggerStepThrough]
        public static Decimal AsNumber(this Char ch)
        {
            return ch;
        }
        
        /// <summary>Returns a sequence of Int32's from start (inclusive) to stop (exclusive).</summary>
        [DebuggerStepThrough]
        public static IEnumerable<int> To(this int start, int stop)
        {
            return Enumerable.Range(start, stop - start);
        }

        [DebuggerStepThrough]
        public static Object[] Concat(this Object[] a1, Object[] a2)
        {
            var z = new Object[a1.Length + a2.Length];
            a1.CopyTo(z, 0);
            a2.CopyTo(z, a1.Length);
            return z;
        }
    }
}
