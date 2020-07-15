using System;
using System.IO;

namespace Brainfuck
{
    class Program
    {
        static void Main(string[] args)
        {
            args = args ?? throw new ArgumentNullException(nameof(args));

            if (args.Length != 2)
            {
                Console.WriteLine("Wrong number of arguments.\n Usage: '--file /path/to/code'");
                return;
            }

            var fileFullPath = Path.GetFullPath(args[1]);
            if (!File.Exists(fileFullPath))
            {
                Console.WriteLine("File not exist!");
                return;
            }

            try
            {
                using (var reader = new StreamReader(fileFullPath))
                {
                    Compiler.Run(reader);
                }
            }
            catch (BrainfuckParsingException e)
            {
                Console.WriteLine(e.Message);
                return;
            }
        }
    }
}
