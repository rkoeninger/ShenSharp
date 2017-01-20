using System;
using System.Linq;

namespace Shen.NET
{
    public class Vector
    {
        public Vector(Decimal length, Context context)
        {
            _array = new Object[length.AsInt()];
            Fill(0, _array.Length - 1, context.Intern("fail!"));
        }

        public Vector(params Object[] values)
        {
            _array = values;
        }

        private readonly Object[] _array;

        public Object this[Decimal index]
        {
            get { return _array[index.AsInt()]; }
            set { _array[index.AsInt()] = value; }
        }

        /// <summary>Sets all places between start (inclusive) and stop (inclusive) to fillValue.</summary>
        public Vector Fill(int start, int stop, Object fillValue)
        {
            foreach (var i in start.To(stop + 1))
                _array[i] = fillValue;

            return this;
        }

        public override int GetHashCode()
        {
            // "djb2" hash algorithm
            return _array.Where(item => item != null).Aggregate(5381, (hash, item) => (hash * 33) + item.GetHashCode());
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(obj, this))
                return true;

            if (!(obj is Vector))
                return false;

            var that = (Vector)obj;
            
            return _array.Length == that._array.Length
                && 0.To(_array.Length).All(i => Equals(_array[i], that._array[i]));
        }

        public override string ToString()
        {
            return "[" + String.Join(", ", _array) + "]";
        }

        public static bool operator ==(Vector a, Vector b)
        {
            return Equals(a, b);
        }

        public static bool operator !=(Vector a, Vector b)
        {
            return !Equals(a, b);
        }
    }
}
