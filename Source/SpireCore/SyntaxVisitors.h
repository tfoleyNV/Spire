#ifndef RASTER_RENDERER_SYNTAX_PRINTER_H
#define RASTER_RENDERER_SYNTAX_PRINTER_H

#include "Diagnostics.h"
#include "Syntax.h"
#include "IL.h"
#include "CompiledProgram.h"

namespace Spire
{
    namespace Compiler
    {
        class CompileOptions;
        class ShaderCompiler;
        class ShaderLinkInfo;
        class ShaderSymbol;

#if 0
        class ICodeGenerator : public SyntaxVisitor
        {
        public:
            ICodeGenerator(DiagnosticSink * perr)
                : SyntaxVisitor(perr)
            {}
            virtual void ProcessFunction(FunctionSyntaxNode * func) = 0;
            virtual void ProcessStruct(StructSyntaxNode * st) = 0;
            virtual void ProcessGlobalVar(VarDeclBase * var) = 0;
        };
#endif

        SyntaxVisitor * CreateSemanticsVisitor(DiagnosticSink * err, CompileOptions const& options);

#if 0
        ICodeGenerator * CreateCodeGenerator(CompileResult & result);
        SyntaxVisitor * CreateILCodeGenerator(DiagnosticSink * err, ILProgram * program, CompileOptions * options);
#endif
    }
}

#endif