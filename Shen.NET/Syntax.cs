using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Shen.NET
{
    public class Syntax
    {
        public static Token Read(String str)
        {
            using (var reader = new StringReader(str))
                return new Syntax(reader).ReadToken();
        }

        public static Token ReadFromRepl(String str, int inputNumber)
        {
            using (var reader = new StringReader(str))
                return new Syntax(reader, "repl-" + inputNumber.ToString("D2")).ReadToken();
        }

        public static List<Token> ReadAll(String str)
        {
            using (var reader = new StringReader(str))
                return new Syntax(reader).ReadAllTokens();
        }

        public static List<Token> ReadFile(String path)
        {
            using (var reader = File.OpenText(path))
                return new Syntax(reader, path).ReadAllTokens();
        }

        private readonly String _source;
        private int _line = 1;
        private int _column = 1;
        private readonly TextReader _reader;

        public Syntax(TextReader reader, String source = "Unknown")
        {
            _reader = reader;
            _source = source;
        }

        public List<Token> ReadAllTokens()
        {
            var tokens = new List<Token>();

            for (;;)
            {
                SkipWhiteSpace();

                if (_reader.Peek() == -1)
                    break;

                tokens.Add(ReadToken());
            }

            return tokens;
        }

        public Token ReadToken()
        {
            SkipWhiteSpace();
            var location = CurrentLocation;

            switch (_reader.Peek())
            {
            case -1:
                Fail("Unexpected end of file");
                return null;
            case '(':
                Skip(); // Pull '(' off the reader
                var tokens = new List<Token>();

                for (var token = ReadToken(); ! (token is ComboEnd); token = ReadToken())
                {
                    if (token == null)
                        Fail("Unexpected end of combo");

                    tokens.Add(token);
                }

                return new Combo(tokens, location);
            case ')':
                Skip(); // Pull ')' off the reader
                return new ComboEnd(location);
            case '\"':
                return new Atom(ReadStringLiteral(), location);
            default:
                return new Atom(ReadLiteral(), location);
            }
        }

        private class ComboEnd : Token
        {
            internal ComboEnd(Location location) : base(location)
            {
            }

            public override Expr Parse(Position position)
            {
                throw new Exception("ComboEnd cannot be parsed");
            }
        }
        
        private void Skip()
        {
            _reader.Read();
            _column++;
        }

        private void SkipWhiteSpace()
        {
            for (;;)
            {
                var b = _reader.Peek();

                if (b == -1)
                    break;

                var ch = (char) b;

                if (! Char.IsWhiteSpace(ch))
                    break;

                if (ch != '\r')
                    _column++;

                if (ch == '\n')
                {
                    _line++;
                    _column = 1;
                }

                _reader.Read();
            }
        }

        private String ReadLiteral()
        {
            var builder = new StringBuilder();

            for (;;)
            {
                var b = _reader.Peek();

                if (b == -1)
                    break;

                var ch = (char) b;
                
                if (ch != '\r')
                    _column++;

                if (ch == '\n')
                {
                    _line++;
                    _column = 1;
                }

                if (Char.IsWhiteSpace(ch) || ch == ')' || ch == '(')
                    break;

                builder.Append(ch);
                _reader.Read();
            }

            return builder.ToString();
        }

        private String ReadStringLiteral()
        {
            var builder = new StringBuilder();
            builder.Append((char)_reader.Read()); // pull the opening qoute

            for (;;)
            {
                var b = _reader.Read();

                if (b == -1)
                    Fail("Uncompleted string literal");

                var ch = (char) b;
                
                if (ch != '\r')
                    _column++;

                if (ch == '\n')
                {
                    _line++;
                    _column = 1;
                }

                builder.Append(ch);

                if (ch == '\"')
                    break;
            }

            return builder.ToString();
        }

        private Location CurrentLocation
        {
            get { return new Location(_source, _line, _column); }
        }

        private void Fail(String message)
        {
            throw new LexException(CurrentLocation, message);
        }
    }

    public class LexException : ApplicationException
    {
        public LexException(Location location, String message) : base(message)
        {
            Location = location;
        }

        public Location Location { get; private set; }
    }
}
