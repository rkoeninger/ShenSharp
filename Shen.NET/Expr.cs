using System;
using System.Collections.Generic;
using System.Linq;

namespace Shen.NET
{
    public abstract class Expr
    {
        protected Expr(Token token, Position position)
        {
            Token = token;
            Position = position;
        }

        public Token Token { get; private set; }
        public Position Position { get; private set; }
        public bool IsTail { get { return Position == Position.Tail; } }

        public Object Eval(Context context, Position position)
        {
            var result = Eval0(context);

            while (position == Position.Head && result is Thunk)
                result = result.As<Thunk>().Eval();

            return result;
        }

        protected abstract Object Eval0(Context context);

        protected Object EvalR(Context context, Expr expr)
        {
            return IsTail && context.EnableTrampolines ? new Thunk(() => expr.Eval(context, Position.Tail)) : expr.Eval(context, Position.Head);
        }

        protected Object EvalF(Context context, IFunction f, Object[] args)
        {
            return IsTail && context.EnableTrampolines ? new Thunk(() => f.Apply(args, Position.Tail)) : f.Apply(args, Position.Head);
        }
    }
    
    public abstract class ConstExpr<T> : Expr
    {
        protected ConstExpr(T val, Token token, Position position) : base(token, position)
        {
            Value = val;
        }

        public T Value { get; private set; }

        protected override Object Eval0(Context context)
        {
            return Value;
        }

    }

    public class BooleanExpr : ConstExpr<Boolean>
    {
        public BooleanExpr(bool val, Token token, Position position) : base(val, token, position)
        {
        }
    }

    public class NumberExpr : ConstExpr<Decimal>
    {
        public NumberExpr(decimal val, Token token, Position position) : base(val, token, position)
        {
        }
    }

    public class StringExpr : ConstExpr<String>
    {
        public StringExpr(String val, Token token, Position position) : base(val, token, position)
        {
        }
    }

    public class EmptyExpr : ConstExpr<Cons>
    {
        public EmptyExpr(Token token, Position position) : base(Cons.EmptyList, token, position)
        {
        }
    }

    public class SymbolExpr : Expr
    {
        public SymbolExpr(String id, Token token, Position position) : base(token, position)
        {
            Id = id;
        }

        public String Id { get; private set; }

        protected override object Eval0(Context context)
        {
            return context.Resolve(Id);
        }
    }

    public class LetExpr : Expr
    {
        public LetExpr(SymbolExpr symbol, Expr def, Expr body, Token token, Position position) : base(token, position)
        {
            Symbol = symbol;
            Definition = def;
            Body = body;
        }

	    public SymbolExpr Symbol { get; private set; }
	    public Expr Definition { get; private set; }
	    public Expr Body { get; private set; }

        protected override object Eval0(Context context)
        {
            var def = Definition.Eval(context, Position.Head);
            var newContext = context.With(Symbol.Id, def);
            return EvalR(newContext, Body);
        }
    }

    public class LambdaExpr : Expr
    {
        public LambdaExpr(SymbolExpr param, Expr body, Token token, Position position) : base(token, position)
        {
            Param = param;
            Body = body;
        }

	    public SymbolExpr Param { get; private set; }
	    public Expr Body { get; private set; }

        protected override object Eval0(Context context)
        {
            return new Defun(new List<SymbolExpr> {Param}, Body, context);
        }
    }

    public class DefunExpr : Expr
    {
        public DefunExpr(SymbolExpr name, List<SymbolExpr> paramz, Expr body, Token token, Position position) : base(token, position)
        {
            Name = name;
            Params = paramz;
            Body = body;
        }

	    public SymbolExpr Name { get; private set; }
	    public List<SymbolExpr> Params { get; private set; }
	    public Expr Body { get; private set; }

        protected override object Eval0(Context context)
        {
            var sym = context.Intern(Name.Id);
            sym.Value = new Defun(this, context);
            return sym;
        }
    }

    public class IfExpr : Expr
    {
        public IfExpr(Expr condition, Expr consequent, Expr alternative, Token token, Position position) : base(token, position)
        {
            Condition = condition;
            Consequent = consequent;
            Alternative = alternative;
        }

        public Expr Condition { get; private set; }
        public Expr Consequent { get; private set; }
        public Expr Alternative { get; private set; }

        protected override object Eval0(Context context)
        {
            var evaluatedCondition = Condition.Eval(context, Position.Head).As<Boolean>();
            return EvalR(context, evaluatedCondition ? Consequent : Alternative);
        }
    }

    public class CondExpr : Expr
    {
        public CondExpr(List<Tuple<Expr, Expr>> clauses, Token token, Position position) : base(token, position)
        {
            Clauses = clauses;
        }

        public List<Tuple<Expr, Expr>> Clauses { get; private set; }

        protected override object Eval0(Context context)
        {
            foreach (var clause in Clauses)
                if (clause.Item1.Eval(context, Position.Head).As<Boolean>())
                    return EvalR(context, clause.Item2);

            throw new ApplicationException("No clause was true in cond expression @ " + Token.Location);
        }
    }
    
    public class AndExpr : Expr
    {
        public AndExpr(Expr first, Expr second, Token token, Position position) : base(token, position)
        {
            First = first;
            Second = second;
        }

        public Expr First { get; private set; }
        public Expr Second { get; private set; }

        protected override object Eval0(Context context)
        {
            var evaluatedLeft = First.Eval(context, Position.Head).As<Boolean>();

            if (!evaluatedLeft)
                return false;

            var evaluatedRight = Second.Eval(context, Position.Head).As<Boolean>();
            return evaluatedRight;
        }
    }
    
    public class OrExpr : Expr
    {
        public OrExpr(Expr first, Expr second, Token token, Position position) : base(token, position)
        {
            First = first;
            Second = second;
        }

        public Expr First { get; private set; }
        public Expr Second { get; private set; }

        protected override object Eval0(Context context)
        {
            var evaluatedLeft = First.Eval(context, Position.Head).As<Boolean>();

            if (evaluatedLeft)
                return true;

            var evaluatedRight = Second.Eval(context, Position.Head).As<Boolean>();
            return evaluatedRight;
        }
    }

    public class FreezeExpr : Expr
    {
        public FreezeExpr(Expr body, Token token, Position position) : base(token, position)
        {
            Body = body;
        }

        public Expr Body { get; private set; }

        protected override object Eval0(Context context)
        {
            return new Defun(new List<SymbolExpr>(), Body, context);
        }
    }

    public class AppExpr : Expr
    {
        public AppExpr(Expr functor, List<Expr> args, Token token, Position position) : base(token, position)
        {
            Functor = functor;
            Args = args;
        }

        public Expr Functor { get; private set; }
	    public List<Expr> Args { get; private set; }
        
        protected override object Eval0(Context context)
        {
            var evaluatedFunctor = Functor.Eval(context, Position.Head).As<IFunction>();
            var evaluatedArgs = Args.Select(x => x.Eval(context, Position.Head)).ToArray();
            return EvalF(context, evaluatedFunctor, evaluatedArgs);
        }
    }

    public class TrapExpr : Expr
    {
        public TrapExpr(Expr body, Expr handler, Token token, Position position) : base(token, position)
        {
            Body = body;
            Handler = handler;
        }

        public Expr Body { get; private set; }
        public Expr Handler { get; private set; }

        protected override object Eval0(Context context)
        {
            try
            {
                return Body.Eval(context, Position.Head);
            }
            catch (Exception exc)
            {
                var evaluatedHandler = Handler.Eval(context, Position.Head).As<IFunction>();
                return EvalF(context, evaluatedHandler, new object[] {new Error(exc)});
            }
        }
    }
}
