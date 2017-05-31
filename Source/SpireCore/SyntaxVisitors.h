#ifndef RASTER_RENDERER_SYNTAX_PRINTER_H
#define RASTER_RENDERER_SYNTAX_PRINTER_H

#include "Diagnostics.h"
#include "Syntax.h"
#include "CompiledProgram.h"

namespace Spire
{
    namespace Compiler
    {
        class CompileOptions;
        class ShaderCompiler;
        class ShaderLinkInfo;
        class ShaderSymbol;

        SyntaxVisitor * CreateSemanticsVisitor(DiagnosticSink * err, CompileOptions const& options);
    }
}

#endif