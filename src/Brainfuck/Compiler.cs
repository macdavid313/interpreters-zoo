using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using ILGen = System.Reflection.Emit.ILGenerator;

namespace Brainfuck
{
    public static class Compiler
    {
        static readonly int tapeSize = 30_000; // https://en.wikipedia.org/wiki/Brainfuck#Language_design
        static readonly MethodAttributes methodAttributes = MethodAttributes.Private | MethodAttributes.HideBySig | MethodAttributes.Static;
        static readonly BindingFlags bindingFlags = BindingFlags.NonPublic | BindingFlags.Static;
        static readonly MethodInfo writeCharMethod = typeof(Console).GetMethod("Write", new Type[] { typeof(char) });
        static readonly MethodInfo writeStringMethod = typeof(Console).GetMethod("Write", new Type[] { typeof(string) });
        static readonly string inputCharPrompt = "\nPlease input a (single) character -> ";
        static readonly MethodInfo readKeyMethod = typeof(Console).GetMethod("ReadKey", Array.Empty<Type>());
        static readonly MethodInfo getKeyCharMethod = typeof(ConsoleKeyInfo).GetProperty("KeyChar").GetMethod;

        public static void Run(TextReader reader)
        {
            reader = reader ?? throw new ArgumentNullException(nameof(reader));

            var asm = AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("Brainfuck"), AssemblyBuilderAccess.Run);
            var module = asm.DefineDynamicModule("BfModule");
            var type = module.DefineType("BfProgram");
            var method = type.DefineMethod("Main", methodAttributes, typeof(void), Array.Empty<Type>());
            var ilGen = method.GetILGenerator();

            LocalBuilder tape;
            LocalBuilder ptr;
            EmitTapeAlloc(ilGen, out tape, out ptr);

            foreach (var token in Parser.Run(reader))
            {
                EmitToken(token, ilGen, tape, ptr);
            }

            ilGen.Emit(OpCodes.Ret);

#if DEBUG
            Console.WriteLine("--------------- All Code Emitted Successfully. ---------------\n");
#endif            

            type.CreateType()
                .GetMethod("Main", bindingFlags)
                .Invoke(null, Array.Empty<object>());
        }

        static void EmitTapeAlloc(ILGen ilGen, out LocalBuilder tape, out LocalBuilder ptr)
        {
            tape = ilGen.DeclareLocal(typeof(byte[]));
            ptr = ilGen.DeclareLocal(typeof(int));

            ilGen.Emit(OpCodes.Ldc_I4, tapeSize);
            ilGen.Emit(OpCodes.Newarr, typeof(byte));
            ilGen.Emit(OpCodes.Stloc, tape);
            ilGen.Emit(OpCodes.Ldc_I4_0);
            ilGen.Emit(OpCodes.Stloc, ptr);
        }

        static void EmitToken(Token token, ILGen ilGen, LocalBuilder tape, LocalBuilder ptr)
        {
#if DEBUG
            Console.WriteLine($"--------------- Emiiting {token.Type.ToString()} ---------------");
#endif
            switch (token.Type)
            {
                case TokenType.MoveLeft:
                    ilGen.Emit(OpCodes.Ldloc, ptr);
                    ilGen.Emit(OpCodes.Ldc_I4, token.Offset);
                    ilGen.Emit(OpCodes.Sub);
                    ilGen.Emit(OpCodes.Stloc, ptr);
                    EmitCheckBounds();
                    return;
                case TokenType.MoveRight:
                    ilGen.Emit(OpCodes.Ldc_I4, token.Offset);
                    ilGen.Emit(OpCodes.Ldloc, ptr);
                    ilGen.Emit(OpCodes.Add);
                    ilGen.Emit(OpCodes.Stloc, ptr);
                    EmitCheckBounds();
                    return;
                case TokenType.IncByte:
                    var tmp = ilGen.DeclareLocal(typeof(byte));
                    EmitPushCurrentBytetoStack();
                    ilGen.Emit(OpCodes.Stloc, tmp);
                    ilGen.Emit(OpCodes.Ldloc, tape);
                    ilGen.Emit(OpCodes.Ldloc, ptr);
                    ilGen.Emit(OpCodes.Ldloc, tmp);
                    ilGen.Emit(OpCodes.Ldc_I4, token.Offset);
                    ilGen.Emit(OpCodes.Add_Ovf_Un);
                    ilGen.Emit(OpCodes.Stelem, typeof(byte));
                    return;
                case TokenType.DecByte:
                    tmp = ilGen.DeclareLocal(typeof(byte));
                    EmitPushCurrentBytetoStack();
                    ilGen.Emit(OpCodes.Stloc, tmp);
                    ilGen.Emit(OpCodes.Ldloc, tape);
                    ilGen.Emit(OpCodes.Ldloc, ptr);
                    ilGen.Emit(OpCodes.Ldloc, tmp);
                    ilGen.Emit(OpCodes.Ldc_I4, token.Offset);
                    ilGen.Emit(OpCodes.Sub_Ovf);
                    ilGen.Emit(OpCodes.Stelem, typeof(byte));
                    return;
                case TokenType.ReadByte:
                    var key = ilGen.DeclareLocal(typeof(ConsoleKeyInfo));
                    ilGen.Emit(OpCodes.Ldstr, inputCharPrompt);
                    ilGen.Emit(OpCodes.Call, writeStringMethod);
                    ilGen.Emit(OpCodes.Call, readKeyMethod);
                    ilGen.Emit(OpCodes.Stloc, key);
                    ilGen.EmitWriteLine("");

                    ilGen.Emit(OpCodes.Ldloc, tape);
                    ilGen.Emit(OpCodes.Ldloc, ptr);
                    ilGen.Emit(OpCodes.Ldloca, key);
                    ilGen.Emit(OpCodes.Call, getKeyCharMethod);
                    ilGen.Emit(OpCodes.Conv_U1);
                    ilGen.Emit(OpCodes.Stelem, typeof(byte));
                    return;
                case TokenType.WriteByte:
                    EmitPushCurrentBytetoStack();
                    ilGen.Emit(OpCodes.Conv_U1);
                    ilGen.Emit(OpCodes.Call, writeCharMethod);
                    return;
                case TokenType.Loop:
                    var loopStart = ilGen.DefineLabel();
                    var loopEnd = ilGen.DefineLabel();
                    ilGen.Emit(OpCodes.Br, loopEnd);
                    ilGen.MarkLabel(loopStart);

                    foreach (var loopBodyToken in token.LoopBody)
                    {
                        EmitToken(loopBodyToken, ilGen, tape, ptr);
                    }

                    ilGen.MarkLabel(loopEnd);
                    EmitPushCurrentBytetoStack();
                    ilGen.Emit(OpCodes.Brtrue, loopStart);
                    return;
            }

            void EmitPushCurrentBytetoStack()
            {
                ilGen.Emit(OpCodes.Ldloc, tape);
                ilGen.Emit(OpCodes.Ldloc, ptr);
                ilGen.Emit(OpCodes.Ldelem, typeof(byte));
            }

            void EmitCheckBounds()
            {
                var quitProgramLabel = ilGen.DefineLabel();
                var continueProgramLabel = ilGen.DefineLabel();
                ilGen.Emit(OpCodes.Ldloc, ptr);
                ilGen.Emit(OpCodes.Ldc_I4_0);
                ilGen.Emit(OpCodes.Blt, quitProgramLabel);
                ilGen.Emit(OpCodes.Ldloc, ptr);
                ilGen.Emit(OpCodes.Ldc_I4, tapeSize);
                ilGen.Emit(OpCodes.Bgt, quitProgramLabel);
                ilGen.Emit(OpCodes.Br, continueProgramLabel);

                ilGen.MarkLabel(quitProgramLabel);
                ilGen.EmitWriteLine("Abort: Tape index is out of bound!");
                ilGen.Emit(OpCodes.Ret);

                ilGen.MarkLabel(continueProgramLabel);
            }
        }
    }
}