// spire-support.cpp
#include "spire-support.h"

#include <stdio.h>

namespace renderer_test {

struct SpireShaderCompilerWrapper : public ShaderCompiler
{
    ShaderCompiler*     innerCompiler;
    SpireCompileTarget  target;

    virtual ShaderProgram* compileProgram(ShaderCompileRequest const& request) override
    {
        SpireSession* spireSession = spCreateSession(NULL);
        SpireCompileRequest* spireRequest = spCreateCompileRequest(spireSession);

        spSetCodeGenTarget(spireRequest, target);

        int translationUnitIndex = spAddTranslationUnit(spireRequest, SPIRE_SOURCE_LANGUAGE_SPIRE, nullptr);

        spAddTranslationUnitSourceString(spireRequest, translationUnitIndex, request.sourcePath, request.sourceText);

        spAddTranslationUnitEntryPoint(spireRequest, translationUnitIndex, request.vertexShader.name,   spFindProfile(spireSession, request.vertexShader.profile));
        spAddTranslationUnitEntryPoint(spireRequest, translationUnitIndex, request.fragmentShader.name, spFindProfile(spireSession, request.fragmentShader.profile));

        int compileErr = spCompile(spireRequest);
        if(auto diagnostics = spGetDiagnosticOutput(spireRequest))
        {
            // TODO(tfoley): re-enable when I get a logging solution in place
//            OutputDebugStringA(diagnostics);
            fprintf(stderr, "%s", diagnostics);
        }
        if(compileErr)
        {
            return nullptr;
        }

        char const* translatedCode = spGetTranslationUnitSource(spireRequest, translationUnitIndex);

        ShaderCompileRequest innerRequest = request;
        innerRequest.sourceText = translatedCode;

        auto result = innerCompiler->compileProgram(innerRequest);

        // We clean up the Spire compilation context and result *after*
        // we have run the downstream compiler, because Spire
        // owns the memory allocation for the generated text, and will
        // free it when we destroy the compilation result.
        spDestroyCompileRequest(spireRequest);
        spDestroySession(spireSession);

        return result;
    }
};

ShaderCompiler* createSpireShaderCompiler(ShaderCompiler* innerCompiler, SpireCompileTarget target)
{
    auto result = new SpireShaderCompilerWrapper();
    result->innerCompiler = innerCompiler;
    result->target = target;

    return result;

}


} // renderer_test

//
// In order to actually use Spire in our application, we need to link in its
// implementation. The easiest way to accomplish this is by directly inlcuding
// the (concatenated) Spire source code into our app.
//

#include <SpireAllSource.h>
