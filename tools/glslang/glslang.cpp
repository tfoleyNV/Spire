// glslang.cpp
#include "glslang.h"


#include "StandAlone/ResourceLimits.h"
#include "StandAlone/Worklist.h"
#include "glslang/Include/ShHandle.h"
#include "glslang/Include/revision.h"
#include "glslang/Public/ShaderLang.h"
#include "SPIRV/GlslangToSpv.h"
#include "SPIRV/GLSL.std.450.h"
#include "SPIRV/doc.h"
#include "SPIRV/disassemble.h"

#include "../../Spire.h"

#if 0
#include <cstring>
#include <cstdlib>
#include <cctype>
#include <cmath>
#include <array>
#include <memory>
#include <thread>
#endif

#ifdef _WIN32
#include <Windows.h>
#endif

#include <sstream>

// This is a wrapper to allow us to run the `glslang` compiler
// in a controlled fashion.

#define UNLIMITED 9999

static TBuiltInResource gResources =
{
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, UNLIMITED, 
    UNLIMITED, UNLIMITED, UNLIMITED,

    { true, true, true, true, true, true, true, true, true, }
};

static void dump(
    std::string const&      text,
    glslang_OutputFunc      outputFunc,
    void*                   outputUserData,
    FILE*                   fallbackStream)
{
    if( outputFunc )
    {
        outputFunc(text.c_str(), outputUserData);
    }
    else
    {
        fprintf(fallbackStream, "%s", text.c_str());

        // also output it for debug purposes
        OutputDebugStringA(text.c_str());
    }
}

static void dumpDiagnostics(
    glslang_CompileRequest* request,
    std::string const&      log)
{
    dump(log, request->diagnosticFunc, request->diagnosticUserData, stderr);
}

extern "C"
_declspec(dllexport)
int glslang_compile(glslang_CompileRequest* request)
{
    glslang::InitializeProcess();

    EShLanguage glslangStage;
    switch( request->spireStage )
    {
#define CASE(SP, GL) case SPIRE_STAGE_##SP: glslangStage = EShLang##GL; break
    CASE(VERTEX,    Vertex);
    CASE(FRAGMENT,  Fragment);
    CASE(GEOMETRY,  Geometry);
    CASE(HULL,      TessControl);
    CASE(DOMAIN,    TessEvaluation);
    CASE(COMPUTE,   Compute);

#undef CASE

    default:
        return 1;
    }

    // TODO: compute glslang stage to use

    glslang::TShader* shader = new glslang::TShader(glslangStage);
    auto shaderPtr = std::unique_ptr<glslang::TShader>(shader);

    glslang::TProgram* program = new glslang::TProgram();
    auto programPtr = std::unique_ptr<glslang::TProgram>(program);

    int sourceTextLength = (int) strlen(request->sourceText);

    shader->setPreamble("#extension GL_GOOGLE_cpp_style_line_directive : require\n");

    shader->setStringsWithLengthsAndNames(
        &request->sourceText,
        &sourceTextLength,
        &request->sourcePath,
        1);

    // Note: this seems required to get past a bug where
    // glslang complains about a declaration of `out gl_PerVertex`
    // that it (seemingly) *should* allow according to the GLSL-for-Vulkan
    // extension.
    shader->setAutoMapLocations(true);

    // Let's auto-map the bindings too, just because we can
    shader->setAutoMapBindings(true);

    EShMessages messages = EShMessages(EShMsgSpvRules | EShMsgVulkanRules);

    if( !shader->parse(&gResources, 110, false, messages) )
    {
        dumpDiagnostics(request, shader->getInfoLog());
        return 1;
    }

    program->addShader(shader);

    if( !program->link(messages) )
    {
        dumpDiagnostics(request, program->getInfoLog());
        return 1;
    }

    if( !program->mapIO() )
    {
        dumpDiagnostics(request, program->getInfoLog());
        return 1;
    }

    for(int stage = 0; stage < EShLangCount; ++stage)
    {
        auto stageIntermediate = program->getIntermediate((EShLanguage)stage);
        if(!stageIntermediate)
            continue;

        std::vector<unsigned int> spirv;
        std::string warningsErrors;
        spv::SpvBuildLogger logger;
        glslang::GlslangToSpv(*stageIntermediate, spirv, &logger);

        dumpDiagnostics(request, logger.getAllMessages());

        std::stringstream spirvAsmStream;

        spv::Disassemble(spirvAsmStream, spirv);

        dump(spirvAsmStream.str(), request->outputFunc, request->outputUserData, stdout);
    }


    glslang::FinalizeProcess();

    return 0;
}
