using System;
using System.Collections.Generic;
using System.Linq;

namespace Shen.NET
{
    public interface IFunction
    {
        Object Apply(Object[] args, Position position);
        int Arity { get; }
    }

    public class Native : IFunction
    {
        public static Native Create<TA>(Func<TA, Object> func)
        {
            return new Native(args => func(args[0].As<TA>()), 1);
        }
        
        public static Native Create<TA, TB>(Func<TA, TB, Object> func)
        {
            return new Native(args => func(args[0].As<TA>(), args[1].As<TB>()), 2);
        }
        
        public static Native Create<TA, TB, TC>(Func<TA, TB, TC, Object> func)
        {
            return new Native(args => func(args[0].As<TA>(), args[1].As<TB>(), args[2].As<TC>()), 3);
        }
        
        public static Native Create<TA, TB, TC, TD>(Func<TA, TB, TC, TD, Object> func)
        {
            return new Native(args => func(args[0].As<TA>(), args[1].As<TB>(), args[2].As<TC>(), args[3].As<TD>()), 4);
        }

        private Native(Func<Object[], Object> func, int arity)
        {
            _func = func;
            Arity = arity;
        }

        private readonly Func<Object[], Object> _func;
        
        public Object Apply(Object[] args, Position position)
        {
            if (args.Length > Arity)
                throw new ApplicationException(String.Format("Too many arguments, expected {0}, {1} provided", Arity, args.Length));

            if (args.Length < Arity)
                return new Native(remainingArgs => _func(args.Concat(remainingArgs)), Arity - args.Length);

            return _func(args);
        }

        public int Arity { get; private set; }

        public override string ToString()
        {
            return "Function/" + Arity;
        }
    }

    public class Defun : IFunction
    {
        public Defun(DefunExpr expr, Context context) : this(expr.Params, expr.Body, context)
        {
            DefunExpr = expr;
        }

        public Defun(List<SymbolExpr> paramz, Expr body, Context context)
        {
            Body = body;
            Params = paramz;
            Context = context;
            Arity = paramz.Count;
        }

        public DefunExpr DefunExpr { get; private set; } // TODO: this is kept so tail-recursion detection/compilation can be done
        public List<SymbolExpr> Params { get; private set; }
        public Expr Body { get; private set; }
        public Context Context { get; private set; }

        public Object Apply(Object[] args, Position position)
        {
            if (args.Length > Arity)
                throw new ApplicationException(String.Format("Too many arguments, expected {0}, {1} provided", Arity, args.Length));

            if (args.Length < Arity)
            {
                var appliedParams = Params.Take(args.Length).Select(x => x.Id).Zip(args, Tuple.Create);
                var remainingParams = Params.Skip(args.Length).ToList();
                var newContext = Context.With(appliedParams);
                return new Defun(remainingParams, Body, newContext);
            }
            else
            {
                var appliedParams = Params.Select(x => x.Id).Zip(args, Tuple.Create);
                var newContext = Context.With(appliedParams);
                return Body.Eval(newContext, position);
            }
        }

        public int Arity { get; private set; }

        public override string ToString()
        {
            return "Function/" + Arity;
        }
    }
}
