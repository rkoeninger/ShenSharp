using System;
using System.Collections.Generic;
using System.Linq;

namespace Shen.NET
{
    public class Cons : IEnumerable<Object>
    {
        public static readonly Cons EmptyList;

        static Cons()
        {
            EmptyList = new Cons(null, null);
            EmptyList.Car = EmptyList;
            EmptyList.Cdr = EmptyList;
            EmptyList.Length = 0;
        }

        public Cons(Object car, Object cdr)
        {
            Car = car;
            Cdr = cdr;
            Length = LengthOf(Car) + LengthOf(Cdr);
        }

        private static int LengthOf(Object obj)
        {
            return obj is Cons ? obj.As<Cons>().Length : 1;
        }

        public Object Car { get; private set; }
        public Object Cdr { get; private set; }
        public int Length { get; private set; }

        public override bool Equals(object obj)
        {
            var that = obj as Cons;

            return that != null
                && Equals(Car, that.Car)
                && Equals(Cdr, that.Cdr);
        }

        public override int GetHashCode()
        {
            return (Car == null ? 0 : Car.GetHashCode() * 101) + (Cdr == null ? 0 : Cdr.GetHashCode() * 31);
        }

        public IEnumerator<Object> GetEnumerator()
        {
            if (ReferenceEquals(this, EmptyList))
                yield break;

            yield return Car;

            if (Cdr is Cons)
            {
                foreach (var item in Cdr.As<Cons>())
                    yield return item;
            }
            else
            {
                yield return Cdr;
            }
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public static bool operator ==(Cons a, Cons b)
        {
            return Equals(a, b);
        }

        public static bool operator !=(Cons a, Cons b)
        {
            return !Equals(a, b);
        }

        public override string ToString()
        {
            if (this == EmptyList) return "()";

            return String.Format("({0}, {1})",
                Car == null ? "null" : Car.ToString(),
                Cdr == null ? "null" : Cdr.ToString());
        }

        public Token ToToken()
        {
            return ToToken(this);
        }

        private static Token ToToken(Object obj)
        {
            if (obj is String)
                return new Atom("\"" + obj + "\"", null);
            
            if (obj is Decimal || obj is Boolean || obj is Symbol)
                return new Atom(obj.ToString(), null);
            
            if (obj is Cons)
                return new Combo(obj.As<Cons>().Select(ToToken).ToList(), null);

            throw new Exception("Invalid syntax element in expression construction");
        }
    }
}
