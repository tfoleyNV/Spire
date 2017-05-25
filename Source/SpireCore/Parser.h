#ifndef RASTER_RENDERER_PARSER_H
#define RASTER_RENDERER_PARSER_H

#include "Lexer.h"
#include "ShaderCompiler.h"
#include "Syntax.h"

namespace Spire
{
    namespace Compiler
    {
        // Parse a source file into an existing translation unit
        void parseSourceFile(
            ProgramSyntaxNode*  translationUnitSyntax,
            CompileOptions&     options,
            TokenSpan const&    tokens,
            DiagnosticSink*     sink,
            String const&       fileName,
            RefPtr<Scope> const&outerScope);
    }
}

#endif