using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

namespace Shen.NET.UnitTests
{
    public static class Try
    {
        public static Exception Expect(Action func)
        {
            return Expect<Exception>(func);
        }

        public static TException Expect<TException>(Action func) where TException : Exception
        {
            try
            {
                func();
                throw new AssertFailedException("Expected " + typeof(TException).Name);
            }
            catch (TException e)
            {
                return e;
            }
        }
    }
}
