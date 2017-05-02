#ifndef RASTER_SHADER_COMPILER_H
#define RASTER_SHADER_COMPILER_H

#include "../CoreLib/Basic.h"
#include "Diagnostics.h"
#include "CompiledProgram.h"
#include "Syntax.h"
#include "CodeGenBackend.h"

#include "../../Spire.h"

namespace Spire
{
    namespace Compiler
    {
        class ILConstOperand;
        struct IncludeHandler;

        enum class CompilerMode
        {
            ProduceLibrary,
            ProduceShader,
            GenerateChoice
        };

        enum class Language
        {
            Unknown,
#define LANGUAGE(TAG, NAME) TAG,
#include "ProfileDefs.h"
        };

        enum class ProfileFamily
        {
            Unknown,
#define PROFILE_FAMILY(TAG) TAG,
#include "ProfileDefs.h"
        };

        enum class ProfileVersion
        {
            Unknown,
#define PROFILE_VERSION(TAG, FAMILY) TAG,
#include "ProfileDefs.h"
        };

        enum class Stage
        {
            Unknown,
#define PROFILE_STAGE(TAG, NAME) TAG,
#include "ProfileDefs.h"
        };

        struct Profile
        {
            typedef uint32_t RawVal;
            enum : RawVal
            {
            Unknown,

#define PROFILE(TAG, NAME, STAGE, VERSION) TAG = (uint32_t(Stage::STAGE) << 16) | uint32_t(ProfileVersion::VERSION),
#include "ProfileDefs.h"
            };

            Profile() {}
            Profile(RawVal raw)
                : raw(raw)
            {}

            Stage GetStage() const { return Stage((uint32_t(raw) >> 16) & 0xFFFF); }
            ProfileVersion GetVersion() const { return ProfileVersion(uint32_t(raw) & 0xFFFF); }

            static Profile LookUp(char const* name);

            RawVal raw = Unknown;
        };

        enum class StageTarget
        {
            Unknown,
            VertexShader,
            HullShader,
            DomainShader,
            GeometryShader,
            FragmentShader,
            ComputeShader,
        };

        enum class CodeGenTarget
        {
            Unknown             = SPIRE_TARGET_UNKNOWN,
            GLSL                = SPIRE_GLSL,
            GLSL_Vulkan         = SPIRE_GLSL_VULKAN,
            GLSL_Vulkan_OneDesc = SPIRE_GLSL_VULKAN_ONE_DESC,
            HLSL                = SPIRE_HLSL,
            SPIRV               = SPIRE_SPIRV,
            DXBytecode          = SPIRE_DXBC,
            DXBytecodeAssembly  = SPIRE_DXBC_ASM,
            ReflectionJSON, // TODO: Add to `Spire.h`
        };

        // Describes an entry point that we've been requested to compile
        struct EntryPointOption
        {
            String name;
            Profile profile;
        };

        enum class PassThroughMode
        {
            None,	// don't pass through: use Spire compiler
            HLSL,	// pass through HLSL to `D3DCompile` API
//			GLSL,	// pass through GLSL to `glslang` library
        };

        // Flavors of translation unit
        enum class TranslationUnitFlavor
        {
            Invalid, // Invalid: should not occur

            SpireCode, // One or more Spire files to be compiled together into a PACKAGE

            ImportedSpireCode, // A separate PACKAGE of Spire code that has been imported

            ForeignShaderCode, // A translation unit of HLSL, GLSL, or other foreign shader code
        };

        // Options for a single translation unit being requested by the user
        class TranslationUnitOptions
        {
        public:
            TranslationUnitFlavor flavor = TranslationUnitFlavor::Invalid;

            // All entry points we've been asked to compile for this translation unit
            List<EntryPointOption> entryPoints;

            // The source file(s) that will be compiled to form this translation unit
            List<String> sourceFilePaths;
        };

        class CompileOptions
        {
        public:
            CompilerMode Mode = CompilerMode::ProduceShader;
            CodeGenTarget Target = CodeGenTarget::Unknown;
            StageTarget stage = StageTarget::Unknown;
            EnumerableDictionary<String, String> BackendArguments;

            String SymbolToCompile;
            String outputName;
            List<String> TemplateShaderArguments;
            List<String> SearchDirectories;
            Dictionary<String, String> PreprocessorDefinitions;

            List<TranslationUnitOptions> translationUnits;

            // the code generation profile we've been asked to use
            Profile profile;

            // should we just pass the input to another compiler?
            PassThroughMode passThrough = PassThroughMode::None;

            // Flags supplied through the API
            SpireCompileFlags flags = 0;
        };

        // This is the representation of a given translation unit
        class CompileUnit
        {
        public:
            TranslationUnitOptions      options;
            RefPtr<ProgramSyntaxNode>   SyntaxNode;
        };

        // TODO: pick an appropriate name for this...
        class CollectionOfTranslationUnits : public RefObject
        {
        public:
            List<CompileUnit> translationUnits;
        };

        class CompilationContext
        {
        public:
            RefPtr<ILProgram> Program;
        };

        class ShaderCompiler : public CoreLib::Basic::Object
        {
        public:
            virtual CompileUnit Parse(
                CompileOptions& options,
                CompileResult & result,
                String source,
                String fileName,
                IncludeHandler* includeHandler,
                Dictionary<String,String> const& preprocessorDefinitions,
                CompileUnit predefUnit) = 0;
            virtual void Compile(CompileResult & result, CompilationContext & context, List<CompileUnit> & units, const CompileOptions & options) = 0;
            void Compile(CompileResult & result, List<CompileUnit> & units, const CompileOptions & options)
            {
                CompilationContext context;
                Compile(result, context, units, options);
            }

            virtual void PassThrough(
                CompileResult &			result, 
                String const&			sourceText,
                String const&			sourcePath,
                const CompileOptions &	options,
                TranslationUnitOptions const& translationUnitOptions) = 0;

        };

        ShaderCompiler * CreateShaderCompiler();
    }
}

#endif