using System;
using System.IO;
using System.Linq;

namespace Shen.NET.REPL
{
    class ReplMain
    {
        static void Main()
        {
            Console.ForegroundColor = ConsoleColor.Gray;
            WriteLine(@"| |/ /\ \       |  __ \|  ____|  __ \| |     
| ' /  \ \      | |__) | |__  | |__) | |     
|  <    > \     |  _  /|  __| |  ___/| |     
| . \  / ^ \    | | \ \| |____| |    | |____ 
|_|\_\/_/ \_\   |_|  \_\______|_|    |______|", ConsoleColor.Magenta);
            ShowHelp();
            var context = Context.NewGlobal();
            Builtins.Install(context);

            for (var input = 0;; input++)
            {
                Console.Write(input.ToString("D2") + "> ");
                var line = (Console.ReadLine() ?? "").Trim();
                var parts = line.Split(new[] {' ', '\t'}, StringSplitOptions.RemoveEmptyEntries);

                if (parts.Length == 0)
                    continue;

                var cmd = parts[0];
                var arg = parts.Length >= 2 ? parts[1] : null;

                switch (cmd)
                {
                    case "quit":
                    case "exit":
                        return;
                    case "help":
                        ShowHelp();
                        break;
                    case "load":
                        if (arg == "all")
                        {
                            foreach (var klFile in KlFiles)
                                Load(klFile, context);
                        }
                        else
                        {
                            Load(arg, context);
                        }
                        break;
                    case "overrides":
                        Overrides.Install(context);
                        break;
                    case "tco":
                        switch (arg)
                        {
                            case "on":
                                context.EnableTrampolines = true;
                                break;
                            case "off":
                                context.EnableTrampolines = false;
                                break;
                            default:
                                WriteLine("Argument to command \"tco\" should be \"on\" or \"off\"", ConsoleColor.Red);
                                break;
                        }
                        break;
                    case "reset":
                        context = Context.NewGlobal();
                        Builtins.Install(context);
                        break;
                    case "source":
                        var val = context.Resolve(arg);

                        if (val is Symbol)
                            val = val.As<Symbol>().Value;

                        if (val is Defun)
                            val = val.As<Defun>().DefunExpr;

                        if (val is Expr)
                        {
                            var expr = val.As<Expr>();
                            WriteLine(expr.Token.Location, ConsoleColor.Cyan);
                            WriteLine(expr.Token, ConsoleColor.White);
                        }
                        else
                        {
                            WriteLine("Not an expression", ConsoleColor.Yellow);
                        }

                        break;
                    case "shen":
                        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

                        foreach (var klFile in KlFiles)
                            Load(klFile, context);

                        Overrides.Install(context);
                        Console.WriteLine();
                        Console.WriteLine("Load time: " + stopwatch.Elapsed);
                        Console.WriteLine();

                        Run("(shen.shen)", context, input);
                        break;
                    default:
                        Run(line, context, input);
                        break;
                }
            }
        }

        static void Run(String line, Context context, int inputCount)
        {
            try
            {
                var token = Syntax.ReadFromRepl(line, inputCount);
                var expr = token.Parse(Position.Tail);
                var result = expr.Eval(context, Position.Head);
                WriteLine(result, result is Error ? ConsoleColor.Yellow : ConsoleColor.Green);
            }
            catch (Exception exc)
            {
                WriteLine(exc.Message, ConsoleColor.Red);
            }
        }

        static readonly String KlFolder = Path.Combine(Environment.CurrentDirectory, @".\..\..\..\klambda\");
        static readonly String[] KlFiles = {
            "toplevel", "core", "sys", "sequent", "yacc", "reader", "prolog",
            "track", "load", "writer", "macros", "declarations", "t-star", "types"
        };

        static void Load(String file, Context context)
        {
            context.Intern("shen-*installing-kl*").Value = true;

            if (! file.EndsWith(".kl")) file = file + ".kl";
            var klFile = Path.GetFullPath(Path.Combine(KlFolder, file));
            var tokens = Syntax.ReadFile(klFile);
            var exprs = tokens.Select(x => x.Parse(Position.Tail));

            foreach (var expr in exprs.Where(x => x is DefunExpr || x is AppExpr))
            {
                WriteLine(TokenToString(expr.Token), ConsoleColor.White);
                expr.Eval(context, Position.Head);
            }

            context.Intern("shen-*installing-kl*").Value = false;
        }

        static string TokenToString(Token t)
        {
            return t is Atom
                ? t.ToString()
                : (t.As<Combo>().Tokens.Count >= 2
                    ? t.As<Combo>().Tokens[0] + " " + t.As<Combo>().Tokens[1]
                    : t.ToString());
        }

        static void ShowHelp()
        {
            var nextColor = Other(ConsoleColor.Magenta, ConsoleColor.Cyan);
            
            WriteLine("exit|quit           = exit repl", nextColor());
            WriteLine("load <name>         = loads kl file", nextColor());
            WriteLine("load all            = loads all kl files in pre-determined order", nextColor());
            WriteLine("overrides           = installs native overrides of kl functions", nextColor());
            WriteLine("tco <on|off>        = toggle use of tail-call optimization", nextColor());
            WriteLine("source <symbol>     = shows source code and location of function", nextColor());
            WriteLine("shen                = loads all kl files and starts the Shen REPL", nextColor());
            WriteLine("(shen.shen)         = starts the Shen REPL", nextColor());
            WriteLine("reset               = clears the global scope and re-installs built-ins", nextColor());
            WriteLine("help                = show this message", nextColor());
        }

        static Func<T> Other<T>(T val1, T val2)
        {
            var currentVal = val1;
            return () =>
            {
                currentVal = Equals(currentVal, val1) ? val2 : val1;
                return currentVal;
            };
        }

        static void WriteLine(Object obj, ConsoleColor color)
        {
            var prevColor = Console.ForegroundColor;
            Console.ForegroundColor = color;
            Console.WriteLine(obj == null ? "null" : obj.ToString());
            Console.ForegroundColor = prevColor;
        }
    }
}
