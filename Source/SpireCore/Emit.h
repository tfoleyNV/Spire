// Emit.h
#ifndef SPIRE_EMIT_H_INCLUDED
#define SPIRE_EMIT_H_INCLUDED

#include "../CoreLib/Basic.h"

#include "ShaderCompiler.h"

namespace Spire
{
    namespace Compiler
    {
        using namespace CoreLib::Basic;

        class ProgramSyntaxNode;
        class ProgramLayout;

        String emitProgram(
            ProgramSyntaxNode*  program,
            ProgramLayout*      programLayout,
            CodeGenTarget       target);
    }
}
#endif
