using System;

namespace Shen.NET
{
    public class Symbol : IFunction
    {
        public Symbol(String name)
        {
            if (name == null)
                throw new ArgumentNullException("name");

            Name = name;
            Value = Cons.EmptyList;
        }

        public String Name { get; private set; }
        public Object Value { get; set; }

        public override String ToString()
        {
            return Name;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public Object Apply(Object[] args, Position position)
        {
            return ValueAsFunction().Apply(args, position);
        }

        public int Arity { get { return ValueAsFunction().Arity; } }

        private IFunction ValueAsFunction()
        {
            if (! (Value is IFunction))
                throw new ApplicationException("Symbol \"" + Name + "\" does not represent a function");

            return Value.As<IFunction>();
        }
    }
}
