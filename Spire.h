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

	enum
	{
		SPIRE_GLSL = 0,
		SPIRE_GLSL_VULKAN,
		SPIRE_GLSL_VULKAN_ONE_DESC,
		SPIRE_HLSL,
		SPIRE_SPIRV
	};

//#define SPIRE_LAYOUT_UNIFORM 0
//#define SPIRE_LAYOUT_PACKED 1
//#define SPIRE_LAYOUT_STORAGE 2

#define SPIRE_ERROR_INSUFFICIENT_BUFFER -1
#define SPIRE_ERROR_INVALID_PARAMETER -2

	/*!
	@brief Represents a compilation context. Created by spCreateCompilationContext().

	Related Functions
	- spCreateCompilationContext()
	- spDestroyCompilationContext()
	- spCreateShaderFromSource()
	- spCreateShaderFromFile()
	- spCompileShader()
	- spSetCodeGenTarget()
	- spAddSearchPath()
	- spSetBackendParameter()
	*/
	struct SpireCompilationContext {};

	/*!
	@brief Represents a shader. A SpireShader can be created by calling spCreateShaderFromSource(), or by loading a module library via spLoadModuleLibrary()

	Related Functions
	- spShaderGetName()
	- spCompileShader()
	- spShaderGetParameterType()
	- spShaderGetParameterName()
	- spShaderGetParameterBinding()
	*/
	struct SpireShader {};

	/*!
	@brief SpireModule objects provide reflection data about a module.
	Module objects can be obtained by calling spFindModule() once a module library is loaded via spLoadModuleLibrary().

	Related Functions
	- spLoadModuleLibrary()
	- spLoadModuleLibraryFromSource()
	- spFindModule()
	- spSpecializeModule()
	- spModuleGetParameterCount()
	- spModuleGetParameter()
	- spModuleGetParameterBufferSize()
	- spModuleGetRequiredComponents()
	*/
	struct SpireModule;

	/*!
	@brief Represents the compilation result, including error messages and compiled source code for each stage.

	Related Functions
	- spCompileShader()
	- spCompileShaderFromSource()
	- spIsCompilationSucessful()
	- spGetCompilerOutput()
	- spGetDiagnosticCount()
	- spGetDiagnosticByIndex()
	- spGetCompiledShaderNames()
	- spGetCompiledShaderStageNames()
	- spGetShaderStageSource()
	- spDestroyCompilationResult()
	*/
	struct SpireCompilationResult {};

    /*!
    @brief A collection of diagnostic messages output by the compiler.
    */
    typedef struct SpireDiagnosticSink SpireDiagnosticSink;

	/*!
	@brief Represents a diagnostic message from the compiler.
	*/
	struct SpireDiagnostic
	{
		const char * Message;    /**< Content of the message. Storage is owned by SpireCompilationContext.*/
        SpireSeverity severity;  /**< Severity of the diagnostic.*/
		int ErrorId;             /**< A unique identifier for this type of error.*/
		const char * FileName;   /**< The source file name of this error. Storage is owned by SpireCompilationContext*/
		int Line;                /**< The line number of this error.*/
		int Col;                 /**< The column position of this error.*/
	};

	/*!
	@brief Stores description of a component.
	*/
	struct SpireComponentInfo
	{
		const char * Name;         /**< The name of the component. Storage is owned by SpireCompilationContext.*/
		const char * TypeName;     /**< The type name of the component. Storage is owned by SpireCompilationContext.*/
		SpireBindableResourceType BindableResourceType; /**< The type of bindable resource this component represents.*/
		int Size;                  /**< The size (in bytes) of the component. For opaque types (e.g. sampler and texture), this value is 0.*/
		int Alignment;             /**< The alignment (in bytes) of the component. For opaque types (e.g. sampler and texture), this value is 0.*/
		int Offset;				   /**< The offset (in bytes) of the component. For opaque types (e.g. sampler and texture), this value is 0.*/
		bool Specialize;		   /**< Indicating whether this is a specialization parameter.*/
	};

	/*!
	@brief Represents a parameter set.
	*/
	typedef struct SpireParameterSet SpireParameterSet;

	/*!
	@brief Represents information on a binding slot of a parameter set.
	*/
	struct SpireResourceBindingInfo
	{
		SpireBindableResourceType Type; /**< The type of this binding slot. */
		int NumLegacyBindingPoints;     /**< The number of legacy binding points. */
		int * LegacyBindingPoints;      /**< The legacy binding points. Storage is owned by SpireCompilationResult.*/
		const char * Name;              /**< The shader code name of this resource. Storage is owned by SpireCompilationResult.*/
	};

	/*!
	@brief Create a compilation context.
	@param cacheDir The directory used to store cached compilation results. Pass NULL to disable caching.
	@return A new compilation context.
	*/
	SPIRE_API SpireCompilationContext * spCreateCompilationContext(const char * cacheDir);

	/*!
	@brief Sets the target for code generation.
	@param ctx The compilation context.
	@param target The code generation target. Possible values are:
	- SPIRE_GLSL. Generates GLSL code.
	- SPIRE_HLSL. Generates HLSL code.
	- SPIRE_SPIRV. Generates SPIR-V code.
	*/
	SPIRE_API void spSetCodeGenTarget(SpireCompilationContext * ctx, int target);

	/*!
	@brief Add a path in which source files are being search. When the programmer specifies @code using <file_name> @endcode in code, the compiler searches the file
	in all search pathes in order.
	@param ctx The compilation context.
	@param searchDir The additional search directory.
	*/
	SPIRE_API void spAddSearchPath(SpireCompilationContext * ctx, const char * searchDir);

	/*!
	@brief Add a macro definition to be used during preprocessing.
	@param key The name of the macro to define.
	@param value The value of the macro to define.
	*/
    SPIRE_API void spAddPreprocessorDefine(SpireCompilationContext * ctx, const char * key, const char * value);

	/*!
	@brief Sets a parameter used by the compiler back-end.
	@param ctx The compilation context.
	@param paramName The name of the parameter.
	@param value The value of the parameter.
	*/
	SPIRE_API void spSetBackendParameter(SpireCompilationContext * ctx, const char * paramName, const char * value);

	/*!
	@brief Sets a shader to compile. By default, the compiler will generate code for all shaders in current context. After setting this option,
	the compiler will only generate code for the specified shader.
	@param ctx The compilation context.
	@param shaderName The name of the shader to compile.
	*/
	SPIRE_API void spSetShaderToCompile(SpireCompilationContext * ctx, const char * shaderName);

	/*!
	@brief Destorys the compilation context. Destorying a compilation context will free the memory for all strings owned by the
	SpireComilationContext and all SpireModule objects. These objects will not be available after a call to spDestroyCompilationContext.
	However, all SpireCompilationResult objects will continue to be available until they are destroyed.
	@param ctx The compilation context to destroy.
	*/
	SPIRE_API void spDestroyCompilationContext(SpireCompilationContext * ctx);

    /*!
    @brief Create a sink for diagnostic messages.
    This sink can be used to capture diagnostic output from compilation operations, and
    can then be used to iterate over the diagnostics produced.
    @param ctx The compilation context.
    */
    SPIRE_API SpireDiagnosticSink* spCreateDiagnosticSink(SpireCompilationContext * ctx);

    /*!
    @brief Reset a diagnostic sink to its original state.
    This clears out any diagnostic messages that have been written to the sink.
    Re-using a single sink across multiple operations may be more efficeint than
    creating and destroying a sink every time.
    @param sink The diagnostic sink to reset.
    */
    SPIRE_API void spClearDiagnosticSink(SpireDiagnosticSink* sink);

    /*!
    @brief Destroy a diagnostic sink.
    @param sink The diagnostic sink to destroy.
    */
    SPIRE_API void spDestroyDiagnosticSink(SpireDiagnosticSink* sink);

	/*!
	@brief Load and precompile spire modules from spire source file. Compilation status and error messages can be obtained via spIsCompilationSucessful(),
	spGetDiagnosticCount() and spGetDiagnosticByIndex() functions.
	@param ctx The compilation context.
	@param fileName The filename of the spire source code.
    @param sink The sink where diagnostic output should be sent, or NULL to ignore messages.
	*/
	SPIRE_API void spLoadModuleLibrary(SpireCompilationContext * ctx, const char * fileName, SpireDiagnosticSink* sink);

	/*!
	@brief Load and precompile spire modules from spire source code in memory. Compilation status and error messages can be obtained via spIsCompilationSucessful(),
	spGetDiagnosticCount() and spGetDiagnosticByIndex() functions.
	@param ctx The compilation context.
	@param source The spire source code to precompile. All strings should be in UTF-8 encoding.
	@param fileName The filename used to report error messages regarding to code in @p source.
    @param sink The sink where diagnostic output should be sent, or NULL to ignore messages.
	*/
	SPIRE_API void spLoadModuleLibraryFromSource(SpireCompilationContext * ctx, const char * source, const char * fileName, SpireDiagnosticSink* sink);

	/*!
	@brief Store current compilation context to a stack. spLoadModuleLibrary() and spLoadModuleLibraryFromSource() load new symbols to
	       a compilation context. spPushContext() and spPopContext() can be used to save and restore context state.
	@param ctx The compilation context whose state to store.
	*/
	void spPushContext(SpireCompilationContext * ctx);
	/*!
	@brief Restore current compilation context to a previously saved state. spLoadModuleLibrary() and spLoadModuleLibraryFromSource() load new symbols to
		   a compilation context. spPushContext() and spPopContext() can be used to save and restore context state.
	@param ctx The compilation context whose state to restore.
	*/
	void spPopContext(SpireCompilationContext * ctx);

	/*!
	@brief Create a template shader object from Spire source code. This is equivalent to calling spLoadModuleLibrary() and spGetShader().
	@param ctx The compilation context.
	@param name The source code of the shader.
	*/
	SPIRE_API SpireShader* spCreateShaderFromSource(SpireCompilationContext * ctx, const char * source, SpireDiagnosticSink * sink);

	/*!
	@brief Find a template shader from current context.
	@param ctx The compilation context in which to find shaders.
	@param name The name of the shader object to find.
	@return A handle to the template shader object that can be used for code generation and reflection. NULL if the shader with specified name does not exist.
	*/
	SPIRE_API SpireShader* spFindShader(SpireCompilationContext * ctx, const char * name);

	/*!
	@brief Returns the total number of entry point shaders in current compilation context.
	@param ctx The compilation context.
	*/
	SPIRE_API int spGetShaderCount(SpireCompilationContext * ctx);

	/*!
	@brief Retrieves the handle to the specified shader object.
	@param ctx The compilation context in which to retrieve shaders.
	@param name The index of the shader object to retrieve.
	@return A handle to the template shader object that can be used for code generation and reflection. NULL if the shader at specified index does not exist.
	*/
	SPIRE_API SpireShader* spGetShader(SpireCompilationContext * ctx, int index);

	/*!
	@brief Create a template shader object from a Spire source file.
	@param ctx The compilation context.
	@param name The source code of the shader.
	*/
	SPIRE_API SpireShader* spCreateShaderFromFile(SpireCompilationContext * ctx, const char * fileName, SpireDiagnosticSink * sink);

	/*!
	@brief Retrieves the runtime unique Id of a shader.
	@param shader The shader object whose Id to retrieve.
	@return Id of the shader object.
	*/
	SPIRE_API unsigned int spShaderGetId(SpireShader * shader);

	/*!
	@brief Retrieves the name of a shader.
	@param shader The shader object whose name to retrieve.
	@return Name of the shader object.
	*/
	SPIRE_API const char * spShaderGetName(SpireShader * shader);

	SPIRE_API const char * spShaderGetParameterType(SpireShader * shader, int i);
	SPIRE_API const char * spShaderGetParameterName(SpireShader * shader, int i);

	SPIRE_API int spShaderGetParameterBinding(SpireShader * shader, int i);
	SPIRE_API int spShaderGetParameterCount(SpireShader * shader);

	/*!
	@brief Find a precompiled module in a SpireCompilationContext.
	@param ctx The compilation context.
	@param moduleName The name of the module to find.
	@return If a module with the specified name exists in the current context, a handle to the module is returned. Otherwise, the return value is NULL.
	@note All SpireModule objects are destroyed when its containing SpireCompilationContext is destroyed.
	*/
	SPIRE_API SpireModule * spFindModule(SpireCompilationContext * ctx, const char * moduleName);
	
	/*!
	@brief Retrieve the run-time unique Id of a SpireModule.
	@param module The module to get the unique Id of.
	@return The unique Id of the module.
	*/
	SPIRE_API unsigned int spGetModuleUID(SpireModule * module);

	/*!
	@brief Retrieve the name of a SpireModule.
	@param module The module to get the name of.
	@return The name of the module as a null-terminated string, or NULL if ther are any errors.
	@note The memory for the return value will be freed when the containing SpireCopmilationContext is destroyed.
	*/
	SPIRE_API const char * spGetModuleName(SpireModule * module);
	
	/*!
	@brief Create a specialized module from an existing module.
	@param ctx A spire compilation context used to hold the specialized module.
	@param module The module to create specialization from.
	@param paramValues The values of specialization parameters.
	@param numParams Number of entries in @p paramValues array.
	@param sink [Optional] A SpireDiagnosticSink object used to receive error messages.
	@return If succesfull, this function returns the specialized module; otherwise the return value is NULL.
	@note The memory for the returning SpireModule will be freed when the @p ctx is destroyed, or when the current context is poped via spPopContext(). 
	*/
	SPIRE_API SpireModule * spSpecializeModule(SpireCompilationContext * ctx, SpireModule * module, int * paramValues, int numParams, SpireDiagnosticSink * sink);

	/*!
	@brief Retrieves number of parameters defined by a module.
	@param module The module from which to retrieve parameters.
	@return Number of parameters defined in @p module.
	*/
	SPIRE_API int spModuleGetParameterCount(SpireModule * module);

	/*!
	@brief Retrieves buffer size required to hold all parameters defined by a module.
	@param module The module from which to retrieve parameter buffer size information.
	@return Number of bytes required to store all parameters defined by @p module.
	*/
	SPIRE_API int spModuleGetParameterBufferSize(SpireModule * module);

	/*!
	@brief Returns whether a module has defined an attribute with specified name.
	@param module The module from which to query attribute definition.
	@param componentName The name of the attribute to test existence of.
	@return 1 if the component is defined, 0 otherwise.
	*/
	SPIRE_API int spModuleHasAttrib(SpireModule * module, const char * attribName);

	/*!
	@brief Retrieves parameter info from a SpireModule.
	@param module The module from which to retrieve parameters.
	@param index Index of the requesting parameter.
	@param result A pointer to a SpireComponentInfo structure used to receive info on the specified parameter.
	@return
	If successful, this function returns 0. 
	Otherwise, the return value is one of the following error codes:
	- SPIRE_ERROR_INVALID_PARAMETER if any of the parameters are invalid.
	*/
	SPIRE_API int spModuleGetParameter(SpireModule * module, int index, SpireComponentInfo * result);
	
	/*!
	@brief Retrieve a list of components that are required by the specified module.
	@param module The module from where to retrieve components.
	@param buffer A user allocated buffer of SpireComponentInfo for receiving outputs.
	@param bufferSize The size (in number of SpireComponentInfo structs) of the specified buffer.
	@return
	If @p buffer is NULL, the return value is the required size, in number of SpireComponentInfo.
	Otherwise, if the function suceeds, the return value is the number of SpireComponentInfo instances written to
	@p buffer. The function returns a negative value if it does not suceed. Possible error codes are:
	- SPIRE_ERROR_INSUFFICIENT_BUFFER. The supplied buffer size was not large enough.
	- SPIRE_ERROR_INVALID_PARAMETER. Any of the parameter values was invalid.
	*/
	SPIRE_API int spModuleGetRequiredComponents(SpireModule * module, SpireComponentInfo * buffer, int bufferSize);

	/*!
	@brief Destroys a shader object.
	@param shader The shader object to destroy.
	@note You are responsible for destorying a shader object when it is no longer used (e.g. after it has been compiled). Destroying a SpireCompilationContext
	does not automatically destroy SpireShader objects.
	*/
	SPIRE_API void spDestroyShader(SpireShader * shader);

	/*!
	@brief Compiles a shader object.
	@param ctx A shader compilation context.
	@param shader The shader object to compile.
	@param args The modules used as template shader arguments.
	@param argCount The number of elements in @p args array.
	@param additionalSource Additional source code to append before passing to compiler.
    @param sink The sink where diagnostic output should be sent, or NULL to ignore messages.
	@return The return value is a handle to a SpireCompilationResult object that contains error messages and compiled source code.
	@note You are responsible for destorying a SpireCompilationResult object when it is no longer used. Destroying a SpireCompilationContext
	does not automatically destroy SpireCompilationResult objects.
	*/
	SPIRE_API SpireCompilationResult* spCompileShader(SpireCompilationContext * ctx, 
		SpireShader * shader, 
		SpireModule** args, 
		int argCount,
		const char * additionalSource,
		SpireDiagnosticSink* sink);

	/*!
	@brief Compiles a shader object.
	@param ctx A shader compilation context.
	@param source A string that represents the Spire source code that defines a shader.
	@param fileName The filename to use to report error messages regarding to @p source.
    @param sink The sink where diagnostic output should be sent, or NULL to ignore messages.
	@return The return value is a handle to a SpireCompilationResult object that contains error messages and compiled source code.
	@note You are responsible for destorying a SpireCompilationResult object when it is no longer used. Destroying a SpireCompilationContext
	does not automatically destroy SpireCompilationResult objects.
	@see spDestroyCompilationResult()
	*/
	SPIRE_API SpireCompilationResult* spCompileShaderFromSource(SpireCompilationContext * ctx, 
		const char * source, 
		const char * fileName,
		SpireDiagnosticSink* sink);  /*deprecated*/

	/*!
	@brief Checks if any errors have been output to the diagnostic sink.
    @param sink The SpireDiagnosticSink to be checked.
    @return 1 if any errors have been output, 0 otherwise.
	*/
	SPIRE_API int spDiagnosticSinkHasAnyErrors(SpireDiagnosticSink* sink);

	/*!
	@brief Retrieve the number of compiler diagnostics in a SpireCompilationResult object.
	@param result A SpireCompilationResult object.
	@return The number of diagnostics available.
	*/
	SPIRE_API int spGetDiagnosticCount(SpireDiagnosticSink* sink);

	/*!
	@brief Retrieve the content of compiler diagnostics in a SpireCompilationResult object.
	@param result A SpireCompilationResult object.
	@param index The index of the compiler diagnostic to retrieve.
	@param pMsg A pointer to a SpireDiagnostic structure to receive the diagnostic.
	@return 1 if successful. SPIRE_ERROR_INVALID_PARAMETER if any of the parameters is invalid.
	*/
	SPIRE_API int spGetDiagnosticByIndex(SpireDiagnosticSink* sink, int index, SpireDiagnostic * pMsg);

	/*!
	@brief Get compiler output messages as a single string.
	@param result A SpireCompilationResult object.
	@param buffer The buffer used to receive compiler messages. If this parameter is NULL, the function returns the number of bytes required for the buffer.
	@param bufferSize The size of @p buffer (in bytes).
	@return
		If successful, the return value is the number of bytes written to @p buffer. If @p buffer is NULL, the return value is the number of bytes required for @p buffer
		to store the entire output message. Otherwise, the function returns one of the following error codes:
		- SPIRE_ERROR_INSUFFICIENT_BUFFER. if @p bufferSize is smaller than required buffer size.
		- SPIRE_ERROR_INVALID_PARAMETER. if any of the parameters is invalid.
	*/
	SPIRE_API int spGetDiagnosticOutput(SpireDiagnosticSink* sink, char * buffer, int bufferSize);

	/*!
	@brief Retrieve a list of shader names that has been compiled.
	@param result A SpireCompilationResult object.
	@param buffer A buffer used to receive shader names. Shader names are separated by '\\n'. If this parameter is NULL, the function returns the required buffer size.
	@param bufferSize The size (in bytes) of @p buffer.
	@return If sucessful, the return value is greater or equal to 0 representing the number of charaters required or written to buffer, including the trailing 0.
	Otherwise, it returns one of the following error codes:
	- SPIRE_ERROR_INSUFFICIENT_BUFFER. The supplied buffer size was not large enough.
	- SPIRE_ERROR_INVALID_PARAMETER. Any of the parameter values was invalid.
	*/
	SPIRE_API int spGetCompiledShaderNames(SpireCompilationResult * result, char * buffer, int bufferSize);

	/*!
	@brief Retrieve a list of stage names in a compiled shader.
	@param result A SpireCompilationResult object.
	@param shaderName The name of a shader.
	@param buffer A buffer used to receive stage names. Stage names are separated by '\\n'. If this parameter is NULL, the function returns the required buffer size.
	@param bufferSize The size (in bytes) of @p buffer.
	@return If sucessful, the return value is greater or equal to 0 representing the number of charaters required or written to buffer, including the trailing 0.
	Otherwise, it returns one of the following error codes:
	- SPIRE_ERROR_INSUFFICIENT_BUFFER. The supplied buffer size was not large enough.
	- SPIRE_ERROR_INVALID_PARAMETER. Any of the parameter values was invalid.
	*/
	SPIRE_API int spGetCompiledShaderStageNames(SpireCompilationResult * result, const char * shaderName, char * buffer, int bufferSize);

	/*!
	@brief Retrieve the compiled code (binary or textual, depending on the target language) of a stage in a compiled shader.
	@param result A SpireCompilationResult object.
	@param shaderName The name of a shader. If @p shaderName is NULL, the function returns the source code of the first shader in @p result.
	@param stage The name of a stage.
	@param[out] length A pointer used to receive the length of the compiled code, can be set to NULL.
	@return If sucessful, the return value is a pointer to the buffer storing the compiled code. Otherwise, the return value is NULL.
	@note The backing memory of the returned code buffer is owned by the SpireCompilationResult object. Destroying the SpireCompilationResult object will render this code
	buffer unusable.
	*/
	SPIRE_API const char * spGetShaderStageSource(SpireCompilationResult * result, const char * shaderName, const char * stage, int * length);

	/*!
	@brief Retrieve the number of parameter sets defined by a compiled shader.
	@param result A SpireCompilationResult object, as a result of shader compilation.
	@param shaderName The name of a shader. If @p shaderName is NULL, the function returns the source code of the first shader in @p result.
	@return The number of parameter sets in specified shader.
	*/
	SPIRE_API int spGetShaderParameterSetCount(SpireCompilationResult * result, const char * shaderName);

	/*!
	@brief Retrieve a SpireParameterSet object representing a parameter set defined by a compiled shader.
	@param result A SpireCompilationResult object, as a result of shader compilation.
	@param shaderName The name of a shader. If @p shaderName is NULL, the function returns the source code of the first shader in @p result.
	@param index The index of the parameter set to return
	@return A SpireParameterSet object representing the requested parameter set. The life-time of the returned object is owned by @p result.
	*/
	SPIRE_API SpireParameterSet * spGetShaderParameterSet(SpireCompilationResult * result, const char * shaderName, int index);

	/*!
	@brief Get the required uniform buffer size of a SpireParameterSet object.
	@param set A SpireParameterSet object whose uniform buffer size to get.
	@return Required uniform buffer size.
	*/
	SPIRE_API int spParameterSetGetBufferSize(SpireParameterSet * set);

	/*!
	@brief Get the binding name of a SpireParameterSet object.
	@param set A SpireParameterSet object whose binding name to get.
	@return The binding name. The life-time of the returned string is owned by @p set.
	*/
	SPIRE_API const char * spParameterSetGetBindingName(SpireParameterSet * set);

	/*!
	@brief Get the binding index of a SpireParameterSet object.
	@param set A SpireParameterSet object whose binding index to get.
	@return The binding index.
	*/
	SPIRE_API int spParameterSetGetBindingIndex(SpireParameterSet * set);

	/*!
	@brief Get the legacy binding index for the uniform buffer of a SpireParameterSet object.
	@param set A SpireParameterSet object whose uniform buffer legacy binding index to get.
	@return The legacy binding index.
	*/
	SPIRE_API int spParameterSetGetUniformBufferLegacyBindingPoint(SpireParameterSet * set);

	/*!
	@brief Get the number of binding slots of a SpireParameterSet object.
	@param set A SpireParameterSet object whose number of binding slots to get.
	@return The number of binding slots.
	*/
	SPIRE_API int spParameterSetGetBindingSlotCount(SpireParameterSet * set);

	/*!
	@brief Get information on a binding slot in a SpireParameterSet object.
	@param set A SpireParameterSet object whose binding slot information to get.
	@return The pointer to a SpireResourceBindingInfo structure that holds information about the requested binding slot.
	        The life-time of the returned structure is owned by @p set and should be freed by the user.
	*/
	SPIRE_API SpireResourceBindingInfo * spParameterSetGetBindingSlot(SpireParameterSet * set, int index);

	/*!
	@brief Destroys the SpireCompilationResult object.
	@param result A SpireCompilationResult object to destroy.
	@note Destroying a SpireCompilationContext object does not automatically destroy SpireCompilationResult objects. You are required to destroy a SpireCompilationResult object
	once it is no longer in use.
	*/
	SPIRE_API void spDestroyCompilationResult(SpireCompilationResult * result);

#ifdef __cplusplus  
}
#endif  

#endif