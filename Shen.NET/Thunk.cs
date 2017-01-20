using System;
using System.Diagnostics;

namespace Shen.NET
{
    public class Thunk
    {
        public Thunk(Func<Object> func)
        {
            _func = func;
        }

        private readonly Func<Object> _func;
        
        [DebuggerStepThrough]
        public Object Eval()
        {
            return _func();
        }
    }
}
