using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;

namespace Shen.NET
{
    [DebuggerDisplay("{ToString()}")]
    public class Location
    {
        public Location(String file, int line, int column)
        {
            File = file;
            Line = line;
            Column = column;
        }

        public String File { get; private set; }
        public int Line { get; private set; }
        public int Column { get; private set; }

        public override string ToString()
        {
            return String.Format("Line {0}, Column {1} in {2}", Line, Column, File);
        }
    }

    public abstract class Token
    {
        protected Token(Location location)
        {
            Location = location;
        }

        public Location Location { get; private set; }

        public abstract Expr Parse(Position position);
    }

    [DebuggerDisplay("Atom {ToString()}")]
    public class Atom : Token
    {
        public Atom(String atom, Location location) : base(location)
        {
            Literal = atom;
        }

        public String Literal { get; private set; }

        public override string ToString()
        {
            return Literal;
        }

        private static readonly Regex NumberRegex = new Regex(@"^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$");

        public override Expr Parse(Position postiion)
        {
            if (NumberRegex.IsMatch(Literal))
                return new NumberExpr(Decimal.Parse(Literal), this, postiion);

            if (Literal[0] == '\"') return new StringExpr(Literal.Substring(1, Literal.Length - 2), this, postiion);
            if (Literal == "true")  return new BooleanExpr(true, this, postiion);
            if (Literal == "false") return new BooleanExpr(false, this, postiion);

            return new SymbolExpr(Literal, this, postiion);
        }
    }
    
    [DebuggerDisplay("Combo {ToString()}")]
    public class Combo : Token
    {
        public Combo(List<Token> tokens, Location location) : base(location)
        {
            Tokens = tokens;
        }

        public List<Token> Tokens { get; private set; }

        public override string ToString()
        {
            return "(" + String.Join(" ", Tokens) + ")";
        }

        public override Expr Parse(Position position)
        {
            if (Tokens.Count == 0) return new EmptyExpr(this, position);

            if (Tokens[0] is Atom)
            {
                switch (Tokens[0].As<Atom>().Literal)
                {
                    case "lambda":
                    {
                        CheckArity(this, 3, "lambda");
                        var param = Tokens[1].Parse(Position.Head).As<SymbolExpr>();
                        var body = Tokens[2].Parse(Position.Tail);
                        return new LambdaExpr(param, body, this, position);
                    }
                    case "let":
                    {
                        CheckArity(this, 4, "let");
                        var symbol = Tokens[1].Parse(Position.Head).As<SymbolExpr>();
                        var def = Tokens[2].Parse(Position.Head);
                        var body = Tokens[3].Parse(position);
                        return new LetExpr(symbol, def, body, this, position);
                    }
                    case "defun":
                    {
                        CheckArity(this, 4, "defun");
                        var symbol = Tokens[1].Parse(Position.Head).As<SymbolExpr>();
                        var paramz = Tokens[2].As<Combo>().Tokens.Select(x => x.Parse(Position.Head)).Cast<SymbolExpr>().ToList();
                        var body = Tokens[3].Parse(Position.Tail);
                        return new DefunExpr(symbol, paramz, body, this, position);
                    }
                    case "if":
                    {
                        CheckArity(this, 4, "if");
                        var condition = Tokens[1].Parse(Position.Head);
                        var consequent = Tokens[2].Parse(position);
                        var alternative = Tokens[3].Parse(position);
                        return new IfExpr(condition, consequent, alternative, this, position);
                    }
                    case "cond":
                    {
                        if (Tokens.Count < 2)
                            throw new ParseException(Location, "cond must have at least one clause");

                        return new CondExpr(Tokens.Skip(1).Cast<Combo>().Select(x =>
                        {
                            CheckArity(x, 2, "cond clause");
                            return Tuple.Create(x.Tokens[0].Parse(Position.Head), x.Tokens[1].Parse(position));
                        }).ToList(), this, position);
                    }
                    case "and":
                    {
                        CheckArity(this, 3, "and");
                        return new AndExpr(Tokens[1].Parse(Position.Head), Tokens[2].Parse(Position.Head), this, position);
                    }
                    case "or":
                    {
                        CheckArity(this, 3, "or");
                        return new OrExpr(Tokens[1].Parse(Position.Head), Tokens[2].Parse(Position.Head), this, position);
                    }
                    case "freeze":
                    {
                        CheckArity(this, 2, "freeze");
                        return new FreezeExpr(Tokens[1].Parse(Position.Tail), this, position);
                    }
                    case "trap-error":
                    {
                        CheckArity(this, 3, "trap-error");
                        return new TrapExpr(Tokens[1].Parse(Position.Head), Tokens[2].Parse(position), this, position);
                    }
                }
            }

            var functor = Tokens[0].Parse(Position.Head);
            var args = Tokens.Skip(1).Select(x => x.Parse(Position.Head)).ToList();
            return new AppExpr(functor, args, this, position);
        }

        private static void CheckArity(Token t, int count, String form)
        {
            if (t.As<Combo>().Tokens.Count != count)
                throw new ParseException(t.Location, String.Format("{0} expression should have {1} parts but found {2}", form, count, t.As<Combo>().Tokens.Count));
        }
    }

    public class ParseException : Exception
    {
        public ParseException(Location location, String message) : base(message)
        {
            Location = location;
        }

        public Location Location { get; private set; }
    }
}
