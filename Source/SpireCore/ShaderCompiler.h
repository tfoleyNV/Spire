#ifndef RASTER_SHADER_COMPILER_H
#define RASTER_SHADER_COMPILER_H

#include "../CoreLib/Basic.h"
#include "Diagnostics.h"
#include "CompiledProgram.h"
#include "Syntax.h"
#include "CodeGenBackend.h"

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
			Unknown,

			GLSL, GLSL_Vulkan, GLSL_Vulkan_OneDesc, HLSL, SPIRV,

			DXBytecode,
			DXBytecodeAssembly,
		};

		// Describes an entry point that we've been requested to compile
		struct EntryPointOption
		{
			String name;
			CodeGenTarget target = CodeGenTarget::Unknown;
			StageTarget stage = StageTarget::Unknown;
		};

		class CompileOptions
		{
		public:
			CompilerMode Mode = CompilerMode::ProduceShader;
			CodeGenTarget Target = CodeGenTarget::Unknown;
			StageTarget stage = StageTarget::Unknown;
			EnumerableDictionary<String, String> BackendArguments;
			String ScheduleSource, ScheduleFileName;
			String SymbolToCompile;
			List<String> TemplateShaderArguments;
			List<String> SearchDirectories;
            Dictionary<String, String> PreprocessorDefinitions;

			// All entry points we've been asked to compile
			List<EntryPointOption> entryPoints;
		};

		class CompileUnit
		{
		public:
			RefPtr<ProgramSyntaxNode> SyntaxNode;
		};

		class CompilationContext
		{
		public:
			SymbolTable Symbols;
			EnumerableDictionary<String, RefPtr<ShaderClosure>> ShaderClosures;
			RefPtr<ILProgram> Program;
		};

		class ShaderCompiler : public CoreLib::Basic::Object
		{
		public:
			virtual CompileUnit Parse(
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
		};

		ShaderCompiler * CreateShaderCompiler();
	}
}

#endif