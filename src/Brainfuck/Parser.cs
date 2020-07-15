using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Brainfuck
{
    public enum TokenType
    {
        MoveLeft, MoveRight, IncByte, DecByte,
        WriteByte, ReadByte,
        Loop
    }

    public struct Token
    {
        public Token(TokenType type)
        {
            Type = type;
            Offset = 0;
            LoopBody = null;
        }

        public Token(TokenType type, int offset)
        {
            Type = type;
            if (type == TokenType.MoveLeft || type == TokenType.MoveRight || type == TokenType.IncByte || type == TokenType.DecByte)
            {
                Type = type;
                Offset = offset;
            }
            else
            {
                throw new InvalidOperationException("'Offset' is only valid for >, <, +, -");
            }
            LoopBody = null;
        }

        public Token(IEnumerable<Token> loopBody)
        {
            Type = TokenType.Loop;
            Offset = 0;
            LoopBody = loopBody;
        }

        public readonly TokenType Type { get; }

        public readonly int Offset { get; }

        public readonly IEnumerable<Token> LoopBody { get; }

#if DEBUG
        public override string ToString()
        {
            switch (Type)
            {
                case TokenType.MoveLeft:
                    return String.Format("(< {0})", Offset);
                case TokenType.MoveRight:
                    return String.Format("(> {0})", Offset);
                case TokenType.IncByte:
                    return String.Format("(+ {0})", Offset);
                case TokenType.DecByte:
                    return String.Format("(- {0})", Offset);
                case TokenType.ReadByte:
                    return "(getbyte)";
                case TokenType.WriteByte:
                    return "(putbyte)";
                case TokenType.Loop:
                    var sb = new StringBuilder("(loop\n  ");
                    foreach (var token in LoopBody)
                    {
                        sb.Append("  ");
                        sb.Append(token.ToString());
                        sb.Append('\n');
                    }
                    sb.Append(')');
                    return sb.ToString();
                default:
                    throw new InvalidOperationException();
            }
        }
#endif
    }

    public static class Parser
    {
        public static IEnumerable<Token> Run(TextReader reader)
        {
            reader = reader ?? throw new ArgumentNullException(nameof(reader));

            while (reader.Peek() != -1)
            {
                switch (Convert.ToChar(reader.Read()))
                {
                    case '<':
                        yield return ReadMoveLeft(reader);
                        continue;
                    case '>':
                        yield return ReadMoveRight(reader);
                        continue;
                    case '+':
                        yield return ReadIncByte(reader);
                        continue;
                    case '-':
                        yield return ReadDecByte(reader);
                        continue;
                    case ',':
                        yield return new Token(TokenType.ReadByte);
                        continue;
                    case '.':
                        yield return new Token(TokenType.WriteByte);
                        continue;
                    case '[':
                        yield return TryReadLoopBody(reader);
                        continue;
                    case ']':
                        throw new BrainfuckParsingException("Unexpected ']'");
                    default:
                        continue;
                }
            }

            yield break;
        }

        static int CollectUntilDifferent(TextReader reader, char targetChar)
        {
            var offset = 1;

            while (reader.Peek() != -1)
            {
                switch (Convert.ToChar(reader.Peek()))
                {
                    case Char c when c == targetChar:
                        offset += 1;
                        reader.Read();
                        continue;
                    default:
                        return offset;
                }
            }

            return offset;
        }

        static Token ReadMoveLeft(TextReader reader) => new Token(TokenType.MoveLeft, CollectUntilDifferent(reader, '<'));

        static Token ReadMoveRight(TextReader reader) => new Token(TokenType.MoveRight, CollectUntilDifferent(reader, '>'));

        static Token ReadIncByte(TextReader reader) => new Token(TokenType.IncByte, CollectUntilDifferent(reader, '+'));

        static Token ReadDecByte(TextReader reader) => new Token(TokenType.DecByte, CollectUntilDifferent(reader, '-'));

        static Token TryReadLoopBody(TextReader reader)
        {
            // '[' already consumed
            var loopBody = new List<Token>();

            while (reader.Peek() != -1)
            {
                switch (Convert.ToChar(reader.Read()))
                {
                    case '<':
                        loopBody.Add(ReadMoveLeft(reader));
                        continue;
                    case '>':
                        loopBody.Add(ReadMoveRight(reader));
                        continue;
                    case '+':
                        loopBody.Add(ReadIncByte(reader));
                        continue;
                    case '-':
                        loopBody.Add(ReadDecByte(reader));
                        continue;
                    case ',':
                        loopBody.Add(new Token(TokenType.ReadByte));
                        continue;
                    case '.':
                        loopBody.Add(new Token(TokenType.WriteByte));
                        continue;
                    case '[':
                        loopBody.Add(TryReadLoopBody(reader));
                        continue;
                    case ']':
                        return new Token(loopBody);
                }
            }

            throw new BrainfuckParsingException("Unexpected EOF, unmatched ']' during parsing.");
        }
    }

    public class BrainfuckParsingException : Exception
    {
        public BrainfuckParsingException(string message) : base(message)
        {

        }
    }
}