using System;

namespace Shen.NET
{
    public class Error
    {
        public Error(String message)
        {
            Message = message;
        }

        public Error(Exception exc)
        {
            Message = exc.Message;
        }

        public String Message { get; private set; }

        public override string ToString()
        {
            return "ERROR: " + Message;
        }
    }
}