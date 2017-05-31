#include "../../Spire.h"

#include "../CoreLib/LibIO.h"
#include "../CoreLib/Tokenizer.h"
#include "../SpireCore/StdInclude.h"
#include "../SpireCore/Parser.h"
#include "../SpireCore/Preprocessor.h"
#include "../SpireCore/Reflection.h"
#include "../SpireCore/TypeLayout.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef NOMINMAX
#endif

using namespace CoreLib::Basic;
using namespace CoreLib::IO;
using namespace CoreLib::Text;
using namespace Spire::Compiler;

namespace SpireLib
{
    static void stdlibDiagnosticCallback(
        char const* message,
        void*       userData)
    {
        fputs(message, stderr);
        fflush(stderr);
#ifdef WIN32
        OutputDebugStringA(message);
#endif
    }

    class Session
    {
    public:
        bool useCache = false;
        CoreLib::String cacheDir;

        RefPtr<ShaderCompiler> compiler;

        RefPtr<Scope>   spireLanguageScope;
        RefPtr<Scope>   hlslLanguageScope;
        RefPtr<Scope>   glslLanguageScope;

        List<RefPtr<ProgramSyntaxNode>> loadedModuleCode;


        Session(bool /*pUseCache*/, CoreLib::String /*pCacheDir*/)
        {
            compiler = CreateShaderCompiler();

            // Create scopes for various language builtins.
            //
            // TODO: load these on-demand to avoid parsing
            // stdlib code for languages the user won't use.

            spireLanguageScope = new Scope();

            hlslLanguageScope = new Scope();
            hlslLanguageScope->parent = spireLanguageScope;

            glslLanguageScope = new Scope();
            glslLanguageScope->parent = spireLanguageScope;

            addBuiltinSource(spireLanguageScope, "stdlib", SpireStdLib::GetCode());
            addBuiltinSource(glslLanguageScope, "glsl", getGLSLLibraryCode());
        }

        ~Session()
        {
            // We need to clean up the strings for the standard library
            // code that we might have allocated and loaded into static
            // variables (TODO: don't use `static` variables for this stuff)

            SpireStdLib::Finalize();

            // Ditto for our type represnetation stuff

            ExpressionType::Finalize();
        }

        CompileUnit createPredefUnit()
        {
            CompileUnit translationUnit;


            RefPtr<ProgramSyntaxNode> translationUnitSyntax = new ProgramSyntaxNode();

            TranslationUnitOptions translationUnitOptions;
            translationUnit.options = translationUnitOptions;
            translationUnit.SyntaxNode = translationUnitSyntax;

            return translationUnit;
        }

        void addBuiltinSource(
            RefPtr<Scope> const&    scope,
            String const&           path,
            String const&           source);
    };

    struct CompileRequest
    {
        // Pointer to parent session
        Session* mSession;

        // Input options
        CompileOptions Options;

        // Output stuff
        DiagnosticSink mSink;
        String mDiagnosticOutput;

        RefPtr<CollectionOfTranslationUnits> mCollectionOfTranslationUnits;

        RefPtr<ProgramLayout> mReflectionData;

        List<String> mTranslationUnitSources;

        List<String> mDependencyFilePaths;

        CompileRequest(Session* session)
            : mSession(session)
        {}

        ~CompileRequest()
        {}

        struct IncludeHandlerImpl : IncludeHandler
        {
            CompileRequest* request;

            List<String> searchDirs;

            virtual bool TryToFindIncludeFile(
                CoreLib::String const& pathToInclude,
                CoreLib::String const& pathIncludedFrom,
                CoreLib::String* outFoundPath,
                CoreLib::String* outFoundSource) override
            {
                String path = Path::Combine(Path::GetDirectoryName(pathIncludedFrom), pathToInclude);
                if (File::Exists(path))
                {
                    *outFoundPath = path;
                    *outFoundSource = File::ReadAllText(path);

                    request->mDependencyFilePaths.Add(path);

                    return true;
                }

                for (auto & dir : searchDirs)
                {
                    path = Path::Combine(dir, pathToInclude);
                    if (File::Exists(path))
                    {
                        *outFoundPath = path;
                        *outFoundSource = File::ReadAllText(path);

                        request->mDependencyFilePaths.Add(path);

                        return true;
                    }
                }
                return false;
            }
        };


        CompileUnit parseTranslationUnit(
            Spire::Compiler::CompileResult& result,
            TranslationUnitOptions const&   translationUnitOptions)
        {
            auto& options = Options;

            IncludeHandlerImpl includeHandler;
            includeHandler.request = this;

            CompileUnit translationUnit;

            RefPtr<Scope> languageScope;
            switch( translationUnitOptions.sourceLanguage )
            {
            case SourceLanguage::HLSL:
                languageScope = mSession->hlslLanguageScope;
                break;

            case SourceLanguage::GLSL:
                languageScope = mSession->glslLanguageScope;
                break;

            case SourceLanguage::Spire:
            default:
                languageScope = mSession->spireLanguageScope;
                break;
            }


            auto& preprocesorDefinitions = options.PreprocessorDefinitions;

            RefPtr<ProgramSyntaxNode> translationUnitSyntax = new ProgramSyntaxNode();

            for( auto sourceFile : translationUnitOptions.sourceFiles )
            {
                auto sourceFilePath = sourceFile->path;

                auto searchDirs = options.SearchDirectories;
                searchDirs.Reverse();
                searchDirs.Add(Path::GetDirectoryName(sourceFilePath));
                searchDirs.Reverse();
                includeHandler.searchDirs = searchDirs;

                String source = sourceFile->content;

                auto tokens = preprocessSource(
                    source,
                    sourceFilePath,
                    result.GetErrorWriter(),
                    &includeHandler,
                    preprocesorDefinitions,
                    translationUnitSyntax.Ptr());

                parseSourceFile(
                    translationUnitSyntax.Ptr(),
                    options,
                    tokens,
                    result.GetErrorWriter(),
                    sourceFilePath,
                    languageScope);
            }

            translationUnit.options = translationUnitOptions;
            translationUnit.SyntaxNode = translationUnitSyntax;

            return translationUnit;
        }

        int executeCompilerDriverActions(
            Spire::Compiler::CompileResult& result)
        {
            // If we are being asked to do pass-through, then we need to do that here...
            if (Options.passThrough != PassThroughMode::None)
            {
                for( auto& translationUnitOptions : Options.translationUnits )
                {
                    switch( translationUnitOptions.sourceLanguage )
                    {
                    // We can pass-through code written in a native shading language
                    case SourceLanguage::GLSL:
                    case SourceLanguage::HLSL:
                        break;

                    // All other translation units need to be skipped
                    default:
                        continue;
                    }

                    auto sourceFile = translationUnitOptions.sourceFiles[0];
                    auto sourceFilePath = sourceFile->path;
                    String source = sourceFile->content;

                    mSession->compiler->PassThrough(
                        source,
                        sourceFilePath,
                        Options,
                        translationUnitOptions);
                }
                return 0;
            }

            // TODO: load the stdlib

            mCollectionOfTranslationUnits = new CollectionOfTranslationUnits();

            // Parse everything from the input files requested
            //
            // TODO: this may trigger the loading and/or compilation of additional modules.
            for( auto& translationUnitOptions : Options.translationUnits )
            {
                auto translationUnit = parseTranslationUnit(result, translationUnitOptions);
                mCollectionOfTranslationUnits->translationUnits.Add(translationUnit);
            }
            if( result.GetErrorCount() != 0 )
                return 1;

            // Now perform semantic checks, emit output, etc.
            mSession->compiler->Compile(
                result, mCollectionOfTranslationUnits.Ptr(), Options);
            if(result.GetErrorCount() != 0)
                return 1;

            mReflectionData = mCollectionOfTranslationUnits->layout;

            return 0;
        }

        // Act as expected of the API-based compiler
        int executeAPIActions()
        {
            Spire::Compiler::CompileResult result;
            result.mSink = &mSink;

            int err = executeCompilerDriverActions(result);

            mDiagnosticOutput = mSink.outputBuffer.ProduceString();

            if(mSink.GetErrorCount() != 0)
                return mSink.GetErrorCount();

            // Copy over the per-translation-unit results
            int translationUnitCount = result.translationUnits.Count();
            for( int tt = 0; tt < translationUnitCount; ++tt )
            {
                auto source = result.translationUnits[tt].outputSource;
                mTranslationUnitSources.Add(source);
            }

            return err;
        }

        int addTranslationUnit(SourceLanguage language, String const& name)
        {
            int result = Options.translationUnits.Count();

            TranslationUnitOptions translationUnit;
            translationUnit.sourceLanguage = SourceLanguage(language);

            Options.translationUnits.Add(translationUnit);

            return result;
        }

        void addTranslationUnitSourceString(
            int             translationUnitIndex,
            String const&   path,
            String const&   source)
        {
            RefPtr<SourceFile> sourceFile = new SourceFile();
            sourceFile->path = path;
            sourceFile->content = source;

            Options.translationUnits[translationUnitIndex].sourceFiles.Add(sourceFile);
        }

        void addTranslationUnitSourceFile(
            int             translationUnitIndex,
            String const&   path)
        {
            String source;
            try
            {
                source = File::ReadAllText(path);
            }
            catch( ... )
            {
                // Emit a diagnostic!
                mSink.diagnose(
                    CodePosition(0,0,0,path),
                    Diagnostics::cannotOpenFile,
                    path);
                return;
            }

            addTranslationUnitSourceString(
                translationUnitIndex,
                path,
                source);

            mDependencyFilePaths.Add(path);
        }

        int addTranslationUnitEntryPoint(
            int                     translationUnitIndex,
            String const&           name,
            Profile                 profile)
        {
            EntryPointOption entryPoint;
            entryPoint.name = name;
            entryPoint.profile = profile;

            // TODO: realistically want this to be global across all TUs...
            int result = Options.translationUnits[translationUnitIndex].entryPoints.Count();

            Options.translationUnits[translationUnitIndex].entryPoints.Add(entryPoint);
            return result;
        }
    };

    void Session::addBuiltinSource(
        RefPtr<Scope> const&    scope,
        String const&           path,
        String const&           source)
    {
        CompileRequest compileRequest(this);

        auto translationUnitIndex = compileRequest.addTranslationUnit(SourceLanguage::Spire, path);

        compileRequest.addTranslationUnitSourceString(
            translationUnitIndex,
            path,
            source);

        int err = compileRequest.executeAPIActions();
        if(err)
        {
            fprintf(stderr, "%s", compileRequest.mDiagnosticOutput.Buffer());

#ifdef _WIN32
            OutputDebugStringA(compileRequest.mDiagnosticOutput.Buffer());
#endif

            assert(!"error in stdlib");
        }

        // Extract the AST for the code we just parsed
        auto syntax = compileRequest.mCollectionOfTranslationUnits->translationUnits[translationUnitIndex].SyntaxNode;

        // HACK(tfoley): mark all declarations in the "stdlib" so
        // that we can detect them later (e.g., so we don't emit them)
        for (auto m : syntax->Members)
        {
            auto fromStdLibModifier = new FromStdLibModifier();

            fromStdLibModifier->next = m->modifiers.first;
            m->modifiers.first = fromStdLibModifier;
        }

        // Add the resulting code to the appropriate scope
        if( !scope->containerDecl )
        {
            // We are the first chunk of code to be loaded for this scope
            scope->containerDecl = syntax.Ptr();
        }
        else
        {
            // We need to create a new scope to link into the whole thing
            auto subScope = new Scope();
            subScope->containerDecl = syntax.Ptr();
            subScope->nextSibling = scope->nextSibling;
            scope->nextSibling = subScope;
        }

        // We need to retain this AST so that we can use it in other code
        // (Note that the `Scope` type does not retain the AST it points to)
        loadedModuleCode.Add(syntax);
    }
}

using namespace SpireLib;

// implementation of C interface

#define SESSION(x) reinterpret_cast<SpireLib::Session *>(x)
#define REQ(x) reinterpret_cast<SpireLib::CompileRequest*>(x)

SPIRE_API SpireSession* spCreateSession(const char * cacheDir)
{
    return reinterpret_cast<SpireSession *>(new SpireLib::Session((cacheDir ? true : false), cacheDir));
}

SPIRE_API void spDestroySession(
    SpireSession*   session)
{
    if(!session) return;
    delete SESSION(session);
}

SPIRE_API void spAddBuiltins(
    SpireSession*   session,
    char const*     sourcePath,
    char const*     sourceString)
{
    auto s = SESSION(session);
    s->addBuiltinSource(

        // TODO(tfoley): Add ability to directly new builtins to the approriate scope
        s->spireLanguageScope,

        sourcePath,
        sourceString);
}


SPIRE_API SpireCompileRequest* spCreateCompileRequest(
    SpireSession* session)
{
    auto s = SESSION(session);
    auto req = new SpireLib::CompileRequest(s);
    return reinterpret_cast<SpireCompileRequest*>(req);
}

/*!
@brief Destroy a compile request.
*/
SPIRE_API void spDestroyCompileRequest(
    SpireCompileRequest*    request)
{
    if(!request) return;
    auto req = REQ(request);
    delete req;
}

SPIRE_API void spSetCompileFlags(
    SpireCompileRequest*    request,
    SpireCompileFlags       flags)
{
    REQ(request)->Options.flags = flags;
}

SPIRE_API void spSetCodeGenTarget(
        SpireCompileRequest*    request,
        int target)
{
    REQ(request)->Options.Target = (CodeGenTarget)target;
}

SPIRE_API void spSetPassThrough(
    SpireCompileRequest*    request,
    SpirePassThrough        passThrough)
{
    REQ(request)->Options.passThrough = PassThroughMode(passThrough);
}

SPIRE_API void spSetDiagnosticCallback(
    SpireCompileRequest*    request,
    SpireDiagnosticCallback callback,
    void const*             userData)
{
    if(!request) return;
    auto req = REQ(request);

    req->mSink.callback = callback;
    req->mSink.callbackUserData = (void*) userData;
}

SPIRE_API void spAddSearchPath(
        SpireCompileRequest*    request,
        const char*             searchDir)
{
    REQ(request)->Options.SearchDirectories.Add(searchDir);
}

SPIRE_API void spAddPreprocessorDefine(
    SpireCompileRequest*    request,
    const char*             key,
    const char*             value)
{
    REQ(request)->Options.PreprocessorDefinitions[key] = value;
}

SPIRE_API char const* spGetDiagnosticOutput(
    SpireCompileRequest*    request)
{
    if(!request) return 0;
    auto req = REQ(request);
    return req->mDiagnosticOutput.begin();
}

// New-fangled compilation API

SPIRE_API int spAddTranslationUnit(
    SpireCompileRequest*    request,
    SpireSourceLanguage     language,
    char const*             name)
{
    auto req = REQ(request);

    return req->addTranslationUnit(
        SourceLanguage(language),
        name ? name : "");
}

SPIRE_API void spAddTranslationUnitSourceFile(
    SpireCompileRequest*    request,
    int                     translationUnitIndex,
    char const*             path)
{
    if(!request) return;
    auto req = REQ(request);
    if(!path) return;
    if(translationUnitIndex < 0) return;
    if(translationUnitIndex >= req->Options.translationUnits.Count()) return;

    req->addTranslationUnitSourceFile(
        translationUnitIndex,
        path);
}

// Add a source string to the given translation unit
SPIRE_API void spAddTranslationUnitSourceString(
    SpireCompileRequest*    request,
    int                     translationUnitIndex,
    char const*             path,
    char const*             source)
{
    if(!request) return;
    auto req = REQ(request);
    if(!source) return;
    if(translationUnitIndex < 0) return;
    if(translationUnitIndex >= req->Options.translationUnits.Count()) return;

    if(!path) path = "";

    req->addTranslationUnitSourceString(
        translationUnitIndex,
        path,
        source);

}

SPIRE_API SpireProfileID spFindProfile(
    SpireSession*   session,
    char const*     name)
{
    return Profile::LookUp(name).raw;
}

SPIRE_API int spAddTranslationUnitEntryPoint(
    SpireCompileRequest*    request,
    int                     translationUnitIndex,
    char const*             name,
    SpireProfileID          profile)
{
    if(!request) return -1;
    auto req = REQ(request);
    if(!name) return -1;
    if(translationUnitIndex < 0) return -1;
    if(translationUnitIndex >= req->Options.translationUnits.Count()) return -1;


    return req->addTranslationUnitEntryPoint(
        translationUnitIndex,
        name,
        Profile(Profile::RawVal(profile)));
}


// Compile in a context that already has its translation units specified
SPIRE_API int spCompile(
    SpireCompileRequest*    request)
{
    auto req = REQ(request);

    int anyErrors = req->executeAPIActions();
    return anyErrors;
}

SPIRE_API int
spGetDependencyFileCount(
    SpireCompileRequest*    request)
{
    if(!request) return 0;
    auto req = REQ(request);
    return req->mDependencyFilePaths.Count();
}

/** Get the path to a file this compilation dependend on.
*/
SPIRE_API char const*
spGetDependencyFilePath(
    SpireCompileRequest*    request,
    int                     index)
{
    if(!request) return 0;
    auto req = REQ(request);
    return req->mDependencyFilePaths[index].begin();
}


// Get the output code associated with a specific translation unit
SPIRE_API char const* spGetTranslationUnitSource(
    SpireCompileRequest*    request,
    int                     translationUnitIndex)
{
    auto req = REQ(request);
    return req->mTranslationUnitSources[translationUnitIndex].begin();
}

// Reflection API

SPIRE_API SpireReflection* spGetReflection(
    SpireCompileRequest*    request)
{
    if( !request ) return 0;

    auto req = REQ(request);
    return (SpireReflection*) req->mReflectionData.Ptr();
}


// ... rest of reflection API implementation is in `Reflection.cpp`
