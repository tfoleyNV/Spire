#ifndef SPIRE_H
#define SPIRE_H

#ifdef _MSC_VER
#ifdef SPIRE_COMPILING_DLL
#define SPIRE_API __declspec(dllexport)
#else
#ifdef SPIRE_DYNAMIC
#define SPIRE_API __declspec(dllimport)
#else
#define SPIRE_API
#endif
#endif
#else
#define SPIRE_API
#endif

#ifdef __cplusplus  
extern "C"
{
#endif
    /*!
    @mainpage Introduction
    Spire is a shading language and compiler framework that facilitates modular shader authoring and rapid exploration of
    shader optimization choices (such as frequency reduction and algorithmic approximation) afforded by modern real-time
    graphics engines. The current implementation of the Spire compiler can generate either GLSL or SPIR-V output for use
    with OpenGL and Vulkan based engines.

    Paper: http://graphics.cs.cmu.edu/projects/spire/


    API Reference: Spire.h

    @file Spire.h
    */

    /*!
    @brief Severity of a diagnostic generated by the compiler.
    Values come from the enum below, with higher values representing more severe
    conditions, and all values >= SPIRE_SEVERITY_ERROR indicating compilation
    failure.
    */
    typedef int SpireSeverity;
    enum
    {
        SPIRE_SEVERITY_NOTE = 0,    /**< An informative message. */
        SPIRE_SEVERITY_WARNING,     /**< A warning, which indicates a possible proble. */
        SPIRE_SEVERITY_ERROR,       /**< An error, indicating that compilation failed. */
        SPIRE_SEVERITY_FATAL,       /**< An unrecoverable error, which forced compilation to abort. */
        SPIRE_SEVERITY_INTERNAL,    /**< An internal error, indicating a logic error in the compiler. */
    };

    typedef int SpireBindableResourceType;
    enum
    {
        SPIRE_NON_BINDABLE = 0,
        SPIRE_TEXTURE,
        SPIRE_SAMPLER,
        SPIRE_UNIFORM_BUFFER,
        SPIRE_STORAGE_BUFFER,
    };

    typedef int SpireCompileTarget;
    enum
    {
        SPIRE_TARGET_UNKNOWN,
        SPIRE_GLSL,
        SPIRE_GLSL_VULKAN,
        SPIRE_GLSL_VULKAN_ONE_DESC,
        SPIRE_HLSL,
        SPIRE_SPIRV,
        SPIRE_DXBC,
        SPIRE_DXBC_ASM,
        SPIRE_REFLECTION_JSON,
    };

    typedef int SpirePassThrough;
    enum
    {
        SPIRE_PASS_THROUGH_NONE,
        SPIRE_PASS_THROUGH_FXC,
        SPIRE_PASS_THROUGH_DXC,
        SPIRE_PASS_THROUGH_GLSLANG,
    };

    /*!
    Flags to control compilation behavior.
    */
    typedef unsigned int SpireCompileFlags;
    enum
    {
        SPIRE_COMPILE_FLAG_NO_CHECKING = 1 << 0, /**< Disable semantic checking as much as possible. */
    };

    typedef int SpireSourceLanguage;
    enum
    {
        SPIRE_SOURCE_LANGUAGE_UNKNOWN,
        SPIRE_SOURCE_LANGUAGE_SPIRE,
        SPIRE_SOURCE_LANGUAGE_HLSL,
        SPIRE_SOURCE_LANGUAGE_GLSL,
    };

    typedef unsigned int SpireProfileID;
    enum
    {
        SPIRE_PROFILE_UNKNOWN,
    };

//#define SPIRE_LAYOUT_UNIFORM 0
//#define SPIRE_LAYOUT_PACKED 1
//#define SPIRE_LAYOUT_STORAGE 2

#define SPIRE_ERROR_INSUFFICIENT_BUFFER -1
#define SPIRE_ERROR_INVALID_PARAMETER -2

    /*!
    @brief An instance of the Spire library.
    */
    typedef struct SpireSession SpireSession;

    /*!
    @bref A request for one or more compilation actions to be performed.
    */
    typedef struct SpireCompileRequest SpireCompileRequest;

    /*!
    @brief Initialize an instance of the Spire library.
    @param cacheDir The directory used to store cached compilation results. Pass NULL to disable caching.
    */
    SPIRE_API SpireSession* spCreateSession(const char * cacheDir);

    /*!
    @brief Clean up after an instance of the Spire library.
    */
    SPIRE_API void spDestroySession(
        SpireSession*   session);


    /*!
    @brief Add new builtin declarations to be used in subsequent compiles.
    */
    SPIRE_API void spAddBuiltins(
        SpireSession*   session,
        char const*     sourcePath,
        char const*     sourceString);

    /*!
    @brief Create a compile request.
    */
    SPIRE_API SpireCompileRequest* spCreateCompileRequest(
        SpireSession* session);

    /*!
    @brief Destroy a compile request.
    */
    SPIRE_API void spDestroyCompileRequest(
        SpireCompileRequest*    request);


    /*!
    @brief Set flags to be used for compilation.
    */
    SPIRE_API void spSetCompileFlags(
        SpireCompileRequest*    request,
        SpireCompileFlags       flags);

    /*!
    @brief Sets the target for code generation.
    @param ctx The compilation context.
    @param target The code generation target. Possible values are:
    - SPIRE_GLSL. Generates GLSL code.
    - SPIRE_HLSL. Generates HLSL code.
    - SPIRE_SPIRV. Generates SPIR-V code.
    */
    SPIRE_API void spSetCodeGenTarget(
        SpireCompileRequest*    request,
        int target);

    SPIRE_API void spSetPassThrough(
        SpireCompileRequest*    request,
        SpirePassThrough        passThrough);

    typedef void(*SpireDiagnosticCallback)(
        char const* message,
        void*       userData);

    SPIRE_API void spSetDiagnosticCallback(
        SpireCompileRequest*    request,
        SpireDiagnosticCallback callback,
        void const*             userData);

    /*!
    @brief Add a path in which source files are being search. When the programmer specifies @code using <file_name> @endcode in code, the compiler searches the file
    in all search pathes in order.
    @param ctx The compilation context.
    @param searchDir The additional search directory.
    */
    SPIRE_API void spAddSearchPath(
        SpireCompileRequest*    request,
        const char*             searchDir);

    /*!
    @brief Add a macro definition to be used during preprocessing.
    @param key The name of the macro to define.
    @param value The value of the macro to define.
    */
    SPIRE_API void spAddPreprocessorDefine(
        SpireCompileRequest*    request,
        const char*             key,
        const char*             value);


    /** Add a distinct translation unit to the compilation request

    `name` is optional.
    Returns the zero-based index of the translation unit created.
    */
    SPIRE_API int spAddTranslationUnit(
        SpireCompileRequest*    request,
        SpireSourceLanguage     language,
        char const*             name);

    /** Add a source file to the given translation unit
    */
    SPIRE_API void spAddTranslationUnitSourceFile(
        SpireCompileRequest*    request,
        int                     translationUnitIndex,
        char const*             path);

    /** Add a source string to the given translation unit

    The `path` will be used in any diagnostic output.
    */
    SPIRE_API void spAddTranslationUnitSourceString(
        SpireCompileRequest*    request,
        int                     translationUnitIndex,
        char const*             path,
        char const*             source);

    /** Look up a compilation profile by name.

    For example, one could look up the string `"ps_5_0"` to find the corresponding target ID.
    */
    SPIRE_API SpireProfileID spFindProfile(
        SpireSession*   session,
        char const*     name);

    /** Add an entry point in a particular translation unit
    */
    SPIRE_API int spAddTranslationUnitEntryPoint(
        SpireCompileRequest*    request,
        int                     translationUnitIndex,
        char const*             name,
        SpireProfileID          profile);

    /** Execute the compilation request.

    Returns zero on success, non-zero on failure.
    */
    SPIRE_API int spCompile(
        SpireCompileRequest*    request);


    /** Get any diagnostic messages reported by the compiler.
    */
    SPIRE_API char const* spGetDiagnosticOutput(
        SpireCompileRequest*    request);

    /** Get the number of files that this compilation depended on.
    
    This includes both the explicit source files, as well as any
    additional files that were transitively referenced (e.g., via
    a `#include` directive).
    */
    SPIRE_API int
    spGetDependencyFileCount(
        SpireCompileRequest*    request);

    /** Get the path to a file this compilation dependend on.
    */
    SPIRE_API char const*
    spGetDependencyFilePath(
        SpireCompileRequest*    request,
        int                     index);


    /** Get the output code associated with a specific translation unit

    The lifetime of the output pointer is the same as `request`.
    */
    SPIRE_API char const* spGetTranslationUnitSource(
        SpireCompileRequest*    request,
        int                     translationUnitIndex);


    /* Note(tfoley): working on new reflection interface...
    */

    typedef struct SpireReflection SpireReflection;

    typedef struct SpireReflectionType SpireReflectionType;
    typedef struct SpireReflectionVariable SpireReflectionVariable;

    // get reflection data from a compilation request
    SPIRE_API SpireReflection* spGetReflection(
        SpireCompileRequest*    request);

    // type reflection

    typedef unsigned int SpireTypeKind;
    enum
    {
        SPIRE_TYPE_KIND_NONE,
        SPIRE_TYPE_KIND_STRUCT,
        SPIRE_TYPE_KIND_ARRAY,
        SPIRE_TYPE_KIND_MATRIX,
        SPIRE_TYPE_KIND_VECTOR,
        SPIRE_TYPE_KIND_SCALAR,
        SPIRE_TYPE_KIND_CONSTANT_BUFFER,
        SPIRE_TYPE_KIND_RESOURCE,
        SPIRE_TYPE_KIND_SAMPLER_STATE,
    
        SPIRE_TYPE_KIND_COUNT,
    };

    typedef unsigned int SpireScalarType;
    enum
    {
        SPIRE_SCALAR_TYPE_NONE,
        SPIRE_SCALAR_TYPE_VOID,
        SPIRE_SCALAR_TYPE_BOOL,
        SPIRE_SCALAR_TYPE_INT32,
        SPIRE_SCALAR_TYPE_UINT32,
        SPIRE_SCALAR_TYPE_INT64,
        SPIRE_SCALAR_TYPE_UINT64,
        SPIRE_SCALAR_TYPE_FLOAT16,
        SPIRE_SCALAR_TYPE_FLOAT32,
        SPIRE_SCALAR_TYPE_FLOAT64,
    };

    typedef unsigned int SpireResourceShape;
    enum
    {
        SPIRE_RESOURCE_BASE_SHAPE_MASK      = 0x0F,

        SPIRE_RESOURCE_NONE                 = 0x00,

        SPIRE_TEXTURE_1D                    = 0x01,
        SPIRE_TEXTURE_2D                    = 0x02,
        SPIRE_TEXTURE_3D                    = 0x03,
        SPIRE_TEXTURE_CUBE                  = 0x04,
        SPIRE_TEXTURE_BUFFER                = 0x05,

        SPIRE_STRUCTURED_BUFFER             = 0x06,
        SPIRE_BYTE_ADDRESS_BUFFER           = 0x07,
        SPIRE_RESOURCE_UNKNOWN              = 0x08,

        SPIRE_RESOURCE_EXT_SHAPE_MASK       = 0xF0,
        SPIRE_TEXTURE_ARRAY_FLAG            = 0x40,
        SPIRE_TEXTURE_MULTISAMPLE_FLAG      = 0x80,

        SPIRE_TEXTURE_1D_ARRAY              = SPIRE_TEXTURE_1D   | SPIRE_TEXTURE_ARRAY_FLAG,
        SPIRE_TEXTURE_2D_ARRAY              = SPIRE_TEXTURE_2D   | SPIRE_TEXTURE_ARRAY_FLAG,
        SPIRE_TEXTURE_CUBE_ARRAY            = SPIRE_TEXTURE_CUBE | SPIRE_TEXTURE_ARRAY_FLAG,

        SPIRE_TEXTURE_2D_MULTISAMPLE        = SPIRE_TEXTURE_2D | SPIRE_TEXTURE_MULTISAMPLE_FLAG,
        SPIRE_TEXTURE_2D_MULTISAMPLE_ARRAY  = SPIRE_TEXTURE_2D | SPIRE_TEXTURE_MULTISAMPLE_FLAG | SPIRE_TEXTURE_ARRAY_FLAG,
    };

    typedef unsigned int SpireResourceAccess;
    enum
    {
        SPIRE_RESOURCE_ACCESS_NONE,
        SPIRE_RESOURCE_ACCESS_READ,
        SPIRE_RESOURCE_ACCESS_READ_WRITE,
        SPIRE_RESOURCE_ACCESS_RASTER_ORDERED,
        SPIRE_RESOURCE_ACCESS_APPEND,
        SPIRE_RESOURCE_ACCESS_CONSUME,
    };

    typedef unsigned int SpireParameterCategory;
    enum
    {
        SPIRE_PARAMETER_CATEGORY_NONE,
        SPIRE_PARAMETER_CATEGORY_CONSTANT_BUFFER,
        SPIRE_PARAMETER_CATEGORY_SHADER_RESOURCE,
        SPIRE_PARAMETER_CATEGORY_UNORDERED_ACCESS,
        SPIRE_PARAMETER_CATEGORY_VERTEX_INPUT,
        SPIRE_PARAMETER_CATEGORY_FRAGMENT_OUTPUT,
        SPIRE_PARAMETER_CATEGORY_SAMPLER_STATE,
        SPIRE_PARAMETER_CATEGORY_UNIFORM,
        SPIRE_PARAMETER_CATEGORY_MIXED,
    };



    SPIRE_API SpireTypeKind spReflectionType_GetKind(SpireReflectionType* type);
    SPIRE_API unsigned int spReflectionType_GetFieldCount(SpireReflectionType* type);
    SPIRE_API SpireReflectionVariable* spReflectionType_GetFieldByIndex(SpireReflectionType* type, unsigned index);

    SPIRE_API size_t spReflectionType_GetElementCount(SpireReflectionType* type);
    SPIRE_API size_t spReflectionType_GetElementStride(SpireReflectionType* type, SpireParameterCategory category);
    SPIRE_API SpireReflectionType* spReflectionType_GetElementType(SpireReflectionType* type);

    SPIRE_API unsigned int spReflectionType_GetRowCount(SpireReflectionType* type);
    SPIRE_API unsigned int spReflectionType_GetColumnCount(SpireReflectionType* type);
    SPIRE_API SpireScalarType spReflectionType_GetScalarType(SpireReflectionType* type);

    SPIRE_API SpireResourceShape spReflectionType_GetResourceShape(SpireReflectionType* type);
    SPIRE_API SpireResourceAccess spReflectionType_GetResourceAccess(SpireReflectionType* type);
    SPIRE_API SpireReflectionType* spReflectionType_GetResourceResultType(SpireReflectionType* type);

    SPIRE_API SpireParameterCategory spReflectionType_GetParameterCategory(SpireReflectionType* type);

    // type layout reflection

    SPIRE_API size_t spReflectionType_GetSize(SpireReflectionType* type, SpireParameterCategory category);


    // variable reflection

    SPIRE_API char const* spReflectionVariable_GetName(SpireReflectionVariable* var);
    SPIRE_API SpireReflectionType* spReflectionVariable_GetType(SpireReflectionVariable* var);

    // variable layout reflection

    SPIRE_API size_t spReflectionVariable_GetOffset(SpireReflectionVariable* var, SpireParameterCategory category);

    // shader parameter reflection

    typedef struct SpireReflectionParameter SpireReflectionParameter;

    SPIRE_API SpireParameterCategory spReflectionParameter_GetCategory(SpireReflectionParameter* parameter);
    SPIRE_API char const* spReflectionParameter_GetName(SpireReflectionParameter* parameter);
    SPIRE_API unsigned spReflectionParameter_GetBindingIndex(SpireReflectionParameter* parameter);
    SPIRE_API unsigned spReflectionParameter_GetBindingSpace(SpireReflectionParameter* parameter);

    // constant buffers
    SPIRE_API SpireReflectionType* spReflectionParameter_GetBufferType(SpireReflectionParameter* parameter);

    // whole shader

    SPIRE_API size_t spReflection_GetReflectionDataSize(SpireReflection* reflection);

    SPIRE_API unsigned spReflection_GetParameterCount(SpireReflection* reflection);
    SPIRE_API SpireReflectionParameter* spReflection_GetParameterByIndex(SpireReflection* reflection, unsigned index);

#ifdef __cplusplus  
}

/* Helper interfaces for C++ users */
namespace spire
{
#define SPIRE_SAFE_BOOL(expr) \
    operator bool() const { return expr; }

    struct BufferReflection;
    struct TypeLayoutReflection;
    struct TypeReflection;
    struct VariableLayoutReflection;
    struct VariableReflection;

    struct ReflectionBase
    {};

    struct TypeReflection : ReflectionBase
    {
        enum class Kind
        {
            None    = SPIRE_TYPE_KIND_NONE,
            Struct  = SPIRE_TYPE_KIND_STRUCT,
            Array   = SPIRE_TYPE_KIND_ARRAY,
            Matrix  = SPIRE_TYPE_KIND_MATRIX,
            Vector  = SPIRE_TYPE_KIND_VECTOR,
            Scalar  = SPIRE_TYPE_KIND_SCALAR,
            ConstantBuffer = SPIRE_TYPE_KIND_CONSTANT_BUFFER,
            Resource = SPIRE_TYPE_KIND_RESOURCE,
            SamplerState = SPIRE_TYPE_KIND_SAMPLER_STATE,
        };

        enum class ScalarType
        {
            None    = SPIRE_SCALAR_TYPE_NONE,
            Void    = SPIRE_SCALAR_TYPE_VOID,
            Bool    = SPIRE_SCALAR_TYPE_BOOL,
            Int32   = SPIRE_SCALAR_TYPE_INT32,
            UInt32  = SPIRE_SCALAR_TYPE_UINT32,
            Int64   = SPIRE_SCALAR_TYPE_INT64,
            UInt64  = SPIRE_SCALAR_TYPE_UINT64,
            Float16 = SPIRE_SCALAR_TYPE_FLOAT16,
            Float32 = SPIRE_SCALAR_TYPE_FLOAT32,
            Float64 = SPIRE_SCALAR_TYPE_FLOAT64,
        };

        Kind getKind()
        {
            return (Kind) spReflectionType_GetKind((SpireReflectionType*) this);
        }

        // only useful if `getKind() == Kind::Struct`
        unsigned int getFieldCount()
        {
            return spReflectionType_GetFieldCount((SpireReflectionType*) this);
        }

        VariableReflection* getFieldByIndex(unsigned int index)
        {
            return (VariableReflection*) spReflectionType_GetFieldByIndex((SpireReflectionType*) this, index);
        }

        bool isArray() { return getKind() == TypeReflection::Kind::Array; }

        TypeReflection* unwrapArray()
        {
            TypeReflection* type = this;
            while( type->isArray() )
            {
                type = type->getElementType();
            }
            return type;
        }

        // only useful if `getKind() == Kind::Array`
        size_t getElementCount()
        {
            return spReflectionType_GetElementCount((SpireReflectionType*) this);
        }

        size_t getTotalArrayElementCount()
        {
            if(!isArray()) return 0;
            size_t result = 1;
            TypeReflection* type = this;
            for(;;)
            {
                if(!type->isArray())
                    return result;

                result *= type->getElementCount();
                type = type->getElementType();
            }
        }

        size_t getElementStride(SpireParameterCategory category)
        {
            return spReflectionType_GetElementStride((SpireReflectionType*) this, category);
        }

        TypeReflection* getElementType()
        {
            return (TypeReflection*) spReflectionType_GetElementType((SpireReflectionType*) this);
        }

        unsigned getRowCount()
        {
            return spReflectionType_GetRowCount((SpireReflectionType*) this);
        }

        unsigned getColumnCount()
        {
            return spReflectionType_GetColumnCount((SpireReflectionType*) this);
        }

        ScalarType getScalarType()
        {
            return (ScalarType) spReflectionType_GetScalarType((SpireReflectionType*) this);
        }

        TypeReflection* getResourceResultType()
        {
            return (TypeReflection*) spReflectionType_GetResourceResultType((SpireReflectionType*) this);
        }

        SpireResourceShape getResourceShape()
        {
            return spReflectionType_GetResourceShape((SpireReflectionType*) this);
        }

        SpireResourceAccess getResourceAccess()
        {
            return spReflectionType_GetResourceAccess((SpireReflectionType*) this);
        }
    };

    struct TypeLayoutReflection : TypeReflection
    {
        size_t getSize(SpireParameterCategory category = SPIRE_PARAMETER_CATEGORY_UNIFORM)
        {
            return spReflectionType_GetSize((SpireReflectionType*) this, category);
        }

        VariableLayoutReflection* getFieldByIndex(unsigned int index)
        {
            return (VariableLayoutReflection*) spReflectionType_GetFieldByIndex((SpireReflectionType*) this, index);
        }

        TypeLayoutReflection* getElementType()
        {
            return (TypeLayoutReflection*) spReflectionType_GetElementType((SpireReflectionType*) this);
        }

        // How is this type supposed to be bound?
        SpireParameterCategory getParameterCategory()
        {
            return spReflectionType_GetParameterCategory((SpireReflectionType*) this);
        }
    };

    struct VariableReflection : ReflectionBase
    {
        char const* getName()
        {
            return spReflectionVariable_GetName((SpireReflectionVariable*) this);
        }

        TypeReflection* getType()
        {
            return (TypeReflection*) spReflectionVariable_GetType((SpireReflectionVariable*) this);
        }
    };

    enum ParameterCategory : SpireParameterCategory
    {
        None = SPIRE_PARAMETER_CATEGORY_NONE,
        ConstantBuffer = SPIRE_PARAMETER_CATEGORY_CONSTANT_BUFFER,
        ShaderResource = SPIRE_PARAMETER_CATEGORY_SHADER_RESOURCE,
        UnorderedAccess = SPIRE_PARAMETER_CATEGORY_UNORDERED_ACCESS,
        VertexInput = SPIRE_PARAMETER_CATEGORY_VERTEX_INPUT,
        FragmentOutput = SPIRE_PARAMETER_CATEGORY_FRAGMENT_OUTPUT,
        SamplerState = SPIRE_PARAMETER_CATEGORY_SAMPLER_STATE,
        Uniform = SPIRE_PARAMETER_CATEGORY_UNIFORM,
        Mixed = SPIRE_PARAMETER_CATEGORY_MIXED,
    };

    struct VariableLayoutReflection : VariableReflection
    {
        ParameterCategory getCategory()
        {
            return (ParameterCategory)spReflectionParameter_GetCategory((SpireReflectionParameter*) this);
        }

        size_t getOffset(SpireParameterCategory category = SPIRE_PARAMETER_CATEGORY_UNIFORM)
        {
            return spReflectionVariable_GetOffset((SpireReflectionVariable*) this, category);
        }

        TypeLayoutReflection* getType()
        {
            return (TypeLayoutReflection*) spReflectionVariable_GetType((SpireReflectionVariable*) this);
        }

        unsigned getBindingIndex()
        {
            return spReflectionParameter_GetBindingIndex((SpireReflectionParameter*) this);
        }

        unsigned getBindingSpace()
        {
            return spReflectionParameter_GetBindingSpace((SpireReflectionParameter*) this);
        }

        BufferReflection* asBuffer()
        {
            switch (getCategory())
            {
            case ParameterCategory::ConstantBuffer:
                return (BufferReflection*) this;

            default:
                break;
            }

            switch( getType()->unwrapArray()->getKind() )
            {
            case TypeReflection::Kind::ConstantBuffer:
                return (BufferReflection*) this;

            default:
                break;
            }

            return 0;
        }
    };


    /** A "top-level" shader input parameter */
    struct ParameterReflection : VariableLayoutReflection
    {


#if 0
        unsigned getBindingIndex()
        {
            return spReflectionParameter_GetBindingIndex((SpireReflectionParameter*) this);
        }

        unsigned getBindingSpace()
        {
            return spReflectionParameter_GetBindingSpace((SpireReflectionParameter*) this);
        }

        size_t getOffset(SpireParameterCategory category = SPIRE_PARAMETER_CATEGORY_UNIFORM)
        {
            return spReflectionVariable_GetOffset((SpireReflectionVariable*) this, category);
        }
#endif
    };

    struct BufferReflection : VariableLayoutReflection
    {
        TypeLayoutReflection* getBufferType()
        {
            return (TypeLayoutReflection*) spReflectionParameter_GetBufferType((SpireReflectionParameter*) this);
        }

        size_t getSize()
        {
            return getBufferType()->getSize();
        }

        unsigned getVariableCount()
        {
            return getBufferType()->getFieldCount();
        }

        VariableLayoutReflection* getVariableByIndex(unsigned int index)
        {
            return getBufferType()->getFieldByIndex(index);
        }
    };

    struct ShaderReflection : ReflectionBase
    {
        unsigned getParameterCount()
        {
            return spReflection_GetParameterCount((SpireReflection*) this);
        }

        ParameterReflection* getParameterByIndex(unsigned index)
        {
            return (ParameterReflection*) spReflection_GetParameterByIndex((SpireReflection*) this, index);
        }

        size_t getReflectionDataSize()
        {
            return spReflection_GetReflectionDataSize((SpireReflection*) this);
        }

        static ShaderReflection* get(SpireCompileRequest* request)
        {
            return (ShaderReflection*) spGetReflection(request);
        }
    };
}

#endif  

#endif