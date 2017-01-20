using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Shen.NET
{
    public class Context
    {
        public static Context NewGlobal()
        {
            var c = new Context();
            c.Parent = c;
            c.Global = c;
            return c;
        }

        private Context()
        {
        }

        private Context(Context parent)
        {
            Parent = parent;
            Global = parent.Global;
        }

        public Context Parent { get; private set; }
        public Context Global { get; private set; }

        /// <summary>Enables the use of trampolines for tail-call optimization.</summary>
        public bool EnableTrampolines
        {
            get { return Global._enableTrampolines; }
            set { Global._enableTrampolines = value; }
        }

        private bool _enableTrampolines = true;

        private readonly ConcurrentDictionary<String, Object> _defs = new ConcurrentDictionary<String, Object>();

        public Symbol Intern(String id)
        {
            return Global._defs.GetOrAdd(id, _ => new Symbol(id)).As<Symbol>();
        }

        private Object InternalLookup(String id)
        {
            if (_defs.ContainsKey(id))
                return _defs[id];

            return this == Parent ? null : Parent.InternalLookup(id);
        }

        public Object Resolve(String id)
        {
            return InternalLookup(id) ?? Intern(id);
        }
        
        public Context With(String id, Object value)
        {
            var newContext = new Context(this);
            newContext._defs[id] = value;
            return newContext;
        }

        public Context With(IEnumerable<Tuple<String, Object>> defs)
        {
            var newContext = new Context(this);

            foreach (var def in defs)
                newContext._defs[def.Item1] = def.Item2;

            return newContext;
        }

        public Context Define(String id, Object value)
        {
            Intern(id).Value = value;
            return this;
        }

        public Context Define<TA>(String id, Func<TA, Object> func)
        {
            Intern(id).Value = Native.Create(func);
            return this;
        }
        
        public Context Define<TA, TB>(String id, Func<TA, TB, Object> func)
        {
            Intern(id).Value = Native.Create(func);
            return this;
        }
        
        public Context Define<TA, TB, TC>(String id, Func<TA, TB, TC, Object> func)
        {
            Intern(id).Value = Native.Create(func);
            return this;
        }
        
        public Context Define<TA, TB, TC, TD>(String id, Func<TA, TB, TC, TD, Object> func)
        {
            Intern(id).Value = Native.Create(func);
            return this;
        }
    }
}
