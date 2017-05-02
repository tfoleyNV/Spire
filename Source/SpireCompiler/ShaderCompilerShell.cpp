#include "CoreLib/LibIO.h"
#include "SpireLib.h"

using namespace CoreLib::Basic;
using namespace CoreLib::IO;
using namespace Spire::Compiler;

// Try to read an argument for a command-line option.
wchar_t const* tryReadCommandLineArgumentRaw(wchar_t const* option, wchar_t***ioCursor, wchar_t**end)
{
    wchar_t**& cursor = *ioCursor;
    if (cursor == end)
    {
        fprintf(stderr, "expected an argument for command-line option '%S'", option);
        exit(1);
    }
    else
    {
        return *cursor++;
    }
}

String tryReadCommandLineArgument(wchar_t const* option, wchar_t***ioCursor, wchar_t**end)
{
    return String::FromWString(tryReadCommandLineArgumentRaw(option, ioCursor, end));
}

Profile TranslateProfileName(char const* name)
{
#define PROFILE(TAG, NAME, STAGE, VERSION)	if(strcmp(name, #NAME) == 0) return Profile::TAG;
#define PROFILE_ALIAS(TAG, NAME)			if(strcmp(name, #NAME) == 0) return Profile::TAG;
#include "SpireCore/ProfileDefs.h"

    return Profile::Unknown;
}

struct Options
{
    String outputDir;
    CompileOptions options;

    Options()
    {
        options.Target = CodeGenTarget::HLSL;
        options.outputName = "out";
    }
};

struct OptionsParser : Options
{
    struct RawEntryPoint : EntryPointOption
    {
        int translationUnitIndex = -1;
    };

    // Collect entry point names, so that we can associate them
    // with entry points later...
    List<RawEntryPoint> rawEntryPoints;

    // The translation unit we are currently working on (or `-1` if none so far)
    int currentTranslationUnitIndex = -1;

    // The number of input files that have been specified
    int inputPathCount = 0;

    // If we already have a translation unit for Spire code, then this will give its index.
    // If not, it will be `-1`.
    int spireTranslationUnit = -1;

    void addInputSpirePath(
        String const& path)
    {
        // All of the input .spire files will be grouped into a single logical translation unit,
        // which we create lazily when the first .spire file is encountered.
        if( spireTranslationUnit == -1 )
        {
            spireTranslationUnit = options.translationUnits.Count();

            TranslationUnitOptions translationUnit;
            translationUnit.flavor = TranslationUnitFlavor::SpireCode;
            options.translationUnits.Add(translationUnit);
        }
        options.translationUnits[spireTranslationUnit].sourceFilePaths.Add(path);

        // Set the translation unit to be used by subsequent entry points
        currentTranslationUnitIndex = spireTranslationUnit;
    }

    void addInputForeignShaderPath(
        String const& path)
    {
        // Set the translation unit to be used by subsequent entry points
        currentTranslationUnitIndex = options.translationUnits.Count();

        // Each foreign shader file is assumed to form its own translation unit, since
        // the existing shading langauges we need to support are all C-derived.
        TranslationUnitOptions translationUnit;
        translationUnit.flavor = TranslationUnitFlavor::ForeignShaderCode;
        translationUnit.sourceFilePaths.Add(path);
        options.translationUnits.Add(translationUnit);
    }

    void addInputPath(
        wchar_t const*  inPath)
    {
        inputPathCount++;

        // look at the extension on the file name to determine
        // how we should handle it.
        String path = String::FromWString(inPath);

        if( path.EndsWith(".spire") )
        {
            // Plain old spire code
            addInputSpirePath(path);
        }
        else if( path.EndsWith(".hlsl") )
        {
            // HLSL for rewriting
            addInputForeignShaderPath(path);
        }
        else
        {
            fprintf(stderr, "error: can't deduce language for input file '%S'\n", inPath);
            exit(1);
        }
    }

    void parse(
        int             argc,
        wchar_t**       argv)
    {
        wchar_t** argCursor = &argv[1];
        wchar_t** argEnd = &argv[argc];
        while (argCursor != argEnd)
        {
            wchar_t const* arg = *argCursor++;
            if (arg[0] == '-')
            {
                String argStr = String::FromWString(arg);

                // The argument looks like an option, so try to parse it.
                if (argStr == "-outdir")
                    outputDir = tryReadCommandLineArgument(arg, &argCursor, argEnd);
                if (argStr == "-out")
                    options.outputName = tryReadCommandLineArgument(arg, &argCursor, argEnd);
                else if (argStr == "-symbo")
                    options.SymbolToCompile = tryReadCommandLineArgument(arg, &argCursor, argEnd);
                else if (argStr == "-no-checking")
                    options.flags |= SPIRE_COMPILE_FLAG_NO_CHECKING;
                else if (argStr == "-backend" || argStr == "-target")
                {
                    String name = tryReadCommandLineArgument(arg, &argCursor, argEnd);
                    if (name == "glsl")
                    {
                        options.Target = CodeGenTarget::GLSL;
                    }
                    else if (name == "glsl_vk")
                    {
                        options.Target = CodeGenTarget::GLSL_Vulkan;
                    }
                    else if (name == "glsl_vk_onedesc")
                    {
                        options.Target = CodeGenTarget::GLSL_Vulkan_OneDesc;
                    }
                    else if (name == "hlsl")
                    {
                        options.Target = CodeGenTarget::HLSL;
                    }
                    else if (name == "spriv")
                    {
                        options.Target = CodeGenTarget::SPIRV;
                    }
                    else if (name == "dxbc")
                    {
                        options.Target = CodeGenTarget::DXBytecode;
                    }
                    else if (name == "dxbc-assembly")
                    {
                        options.Target = CodeGenTarget::DXBytecodeAssembly;
                    }
                    else if (name == "reflection-json")
                    {
                        options.Target = CodeGenTarget::ReflectionJSON;
                    }
                    else
                    {
                        fprintf(stderr, "unknown code generation target '%S'\n", name.ToWString());
                        exit(1);
                    }
                }
                // A "profile" specifies both a specific target stage and a general level
                // of capability required by the program.
                else if (argStr == "-profile")
                {
                    String name = tryReadCommandLineArgument(arg, &argCursor, argEnd);

                    Profile profile = TranslateProfileName(name.begin());
                    if( profile.raw == Profile::Unknown )
                    {
                        fprintf(stderr, "unknown profile '%s'\n", name.begin());
                    }
                    else
                    {
                        options.profile = profile;
                    }
                }
                else if (argStr == "-entry")
                {
                    String name = tryReadCommandLineArgument(arg, &argCursor, argEnd);

                    RawEntryPoint entry;
                    entry.name = name;
                    entry.translationUnitIndex = currentTranslationUnitIndex;

                    // TODO(tfoley): Allow user to fold a specification of a profile into the entry-point name,
                    // for the case where they might be compiling multiple entry points in one invocation...
                    //
                    // For now, just use the last profile set on the command-line to specify this

                    entry.profile = options.profile;

                    rawEntryPoints.Add(entry);
                }
                else if (argStr == "-stage")
                {
                    String name = tryReadCommandLineArgument(arg, &argCursor, argEnd);
                    StageTarget stage = StageTarget::Unknown;
                    if (name == "vertex") { stage = StageTarget::VertexShader; }
                    else if (name == "fragment") { stage = StageTarget::FragmentShader; }
                    else if (name == "hull") { stage = StageTarget::HullShader; }
                    else if (name == "domain") { stage = StageTarget::DomainShader; }
                    else if (name == "compute") { stage = StageTarget::ComputeShader; }
                    else
                    {
                        fprintf(stderr, "unknown stage '%S'\n", name.ToWString());
                    }
                    options.stage = stage;
                }
                else if (argStr == "-pass-through")
                {
                    String name = tryReadCommandLineArgument(arg, &argCursor, argEnd);
                    PassThroughMode passThrough = PassThroughMode::None;
                    if (name == "fxc") { passThrough = PassThroughMode::HLSL; }
                    else
                    {
                        fprintf(stderr, "unknown pass-through target '%S'\n", name.ToWString());
                        exit(1);
                    }
                    options.passThrough = passThrough;
                }
                else if (argStr == "-genchoice")
                    options.Mode = CompilerMode::GenerateChoice;
                else if (argStr[1] == 'D')
                {
                    // The value to be defined might be part of the same option, as in:
                    //     -DFOO
                    // or it might come separately, as in:
                    //     -D FOO
                    wchar_t const* defineStr = arg + 2;
                    if (defineStr[0] == 0)
                    {
                        // Need to read another argument from the command line
                        defineStr = tryReadCommandLineArgumentRaw(arg, &argCursor, argEnd);
                    }
                    // The string that sets up the define can have an `=` between
                    // the name to be defined and its value, so we search for one.
                    wchar_t const* eqPos = nullptr;
                    for(wchar_t const* dd = defineStr; *dd; ++dd)
                    {
                        if (*dd == '=')
                        {
                            eqPos = dd;
                            break;
                        }
                    }

                    // Now set the preprocessor define
                    //
                    if (eqPos)
                    {
                        // If we found an `=`, we split the string...
                        options.PreprocessorDefinitions[String::FromWString(defineStr, eqPos)] = String::FromWString(eqPos+1);
                    }
                    else
                    {
                        // If there was no `=`, then just #define it to an empty string
                        options.PreprocessorDefinitions[String::FromWString(defineStr)] = String();
                    }
                }
                else if (argStr[1] == 'I')
                {
                    // The value to be defined might be part of the same option, as in:
                    //     -IFOO
                    // or it might come separately, as in:
                    //     -I FOO
                    // (see handling of `-D` above)
                    wchar_t const* includeDirStr = arg + 2;
                    if (includeDirStr[0] == 0)
                    {
                        // Need to read another argument from the command line
                        includeDirStr = tryReadCommandLineArgumentRaw(arg, &argCursor, argEnd);
                    }

                    options.SearchDirectories.Add(String::FromWString(includeDirStr));
                }
                else if (argStr == "--")
                {
                    // The `--` option causes us to stop trying to parse options,
                    // and treat the rest of the command line as input file names:
                    while (argCursor != argEnd)
                    {
                        addInputPath(*argCursor++);
                    }
                    break;
                }
                else
                {
                    fprintf(stderr, "unknown command-line option '%S'\n", argStr.ToWString());
                    // TODO: print a usage message
                    exit(1);
                }
            }
            else
            {
                addInputPath(arg);
            }
        }

        if (inputPathCount == 0)
        {
            fprintf(stderr, "error: no input file specified\n");
            exit(1);
        }
        else if (inputPathCount > 1)
        {
            fprintf(stderr, "error: multiple input files specified\n");
            exit(1);
        }

        // No point in moving forward if there is nothing to compile
        if( options.translationUnits.Count() == 0 )
        {
            fprintf(stderr, "error: no compilation requested\n");
            exit(1);
        }

        // For any entry points that were given without an explicit profile, we can now apply
        // the profile that was given to them.
        if( rawEntryPoints.Count() != 0 )
        {
            if( options.profile.raw == Profile::Unknown )
            {
                fprintf(stderr, "error: no profile specified; use the '-profile <profile name>' option");
                exit(1);
            }
            // TODO: issue an error if we have multiple `-profile` options *and*
            // there are entry points that didn't get a profile.
            else
            {
                for( auto& e : rawEntryPoints )
                {
                    if( e.profile.raw == Profile::Unknown )
                    {
                        e.profile = options.profile;
                    }
                }
            }
        }

        // Next, we want to make sure that entry points get attached to the appropriate translation
        // unit that will provide them.
        {
            bool anyEntryPointWithoutTranslationUnit = false;
            for( auto& entryPoint : rawEntryPoints )
            {
                // Skip entry points that are already associated with a translation unit...
                if( entryPoint.translationUnitIndex != -1 )
                    continue;

                anyEntryPointWithoutTranslationUnit = true;
                entryPoint.translationUnitIndex = 0;
            }

            if( anyEntryPointWithoutTranslationUnit && options.translationUnits.Count() != 1 )
            {
                fprintf(stderr, "error: when using multiple translation units, entry points must be specified after their translation unit file(s)");
                exit(1);
            }

            // Now place all those entry points where they belong
            for( auto& entryPoint : rawEntryPoints )
            {
                options.translationUnits[entryPoint.translationUnitIndex].entryPoints.Add(entryPoint);
            }
        }

        // Automatically derive an output directory based on the first file specified.
        //
        // TODO: require manual specification if there are multiple input files, in different directories
        String fileName = options.translationUnits[0].sourceFilePaths[0];
        if (outputDir.Length() == 0)
        {
            outputDir = Path::GetDirectoryName(fileName);
        }

    }

};


Options parseOptions(
    int         argc,
    wchar_t**   argv)
{
    OptionsParser parser;
    parser.parse(argc, argv);
    return parser;
}

int wmain(int argc, wchar_t* argv[])
{
    // Parse any command-line options
    Options options = parseOptions(argc, argv);

    // Invoke the compiler

    try
    {
        int result = SpireLib::executeCompilerDriverActions(options.options);
        if( result != 0 )
        {
            exit(-1);
        }

    }
    catch (Exception & e)
    {
        printf("internal compiler error: %S\n", e.Message.ToWString());
        return 1;
    }

#if 0
        int returnValue = -1;
    {


        auto sourceDir = Path::GetDirectoryName(fileName);
        CompileResult result;
        try
        {
            auto files = SpireLib::CompileShaderSourceFromFile(result, fileName, options);
            for (auto & f : files)
            {
                try
                {
                    f.SaveToFile(Path::Combine(outputDir, f.MetaData.ShaderName + ".cse"));
                }
                catch (Exception &)
                {
                    result.GetErrorWriter()->diagnose(CodePosition(0, 0, 0, ""), Diagnostics::cannotWriteOutputFile, Path::Combine(outputDir, f.MetaData.ShaderName + ".cse"));
                }
            }
        }
        catch (Exception & e)
        {
            printf("internal compiler error: %S\n", e.Message.ToWString());
        }
        result.PrintDiagnostics();
        if (result.GetErrorCount() == 0)
            returnValue = 0;
    }
#endif

#ifdef _MSC_VER
    _CrtDumpMemoryLeaks();
#endif
    return 0;
}