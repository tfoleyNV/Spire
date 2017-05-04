// Reflection.cpp
#include "Reflection.h"

#include "ShaderCompiler.h"
#include "TypeLayout.h"

#include <assert.h>

namespace Spire {
namespace Compiler {

struct ReflectionGenerationContext
{
    List<uint8_t> data;
};

template<typename T>
struct NodePtr
{
    ReflectionGenerationContext*    context;
    size_t                          offset;

    NodePtr()
        : context(0), offset(0)
    {}

    NodePtr(
        ReflectionGenerationContext*    context,
        size_t                          offset)
        : context(context)
        , offset(offset)
    {}

    template<typename U>
    NodePtr(
        NodePtr<U> ptr,
        typename EnableIf<IsConvertible<T*, U*>::Value, void>::type * = 0)
        : context(ptr.context)
        , offset(ptr.offset)
    {}

    operator T*()
    {
        return (T*) (context->data.begin() + offset);
    }

    T* operator->()
    {
        return *this;
    }

    NodePtr<T> operator+(size_t index)
    {
        return NodePtr<T>(context, offset + index*sizeof(T));
    }

    template<typename U>
    NodePtr<U> Cast()
    {
        return NodePtr<U>(context, offset);
    }
};

template<typename T>
NodePtr<T> MakeNodePtr(ReflectionGenerationContext* context, T* rawPtr)
{
    NodePtr<T> nodePtr;
    nodePtr.context = context;
    nodePtr.offset = ((uint8_t*)rawPtr - context->data.begin());
    return nodePtr;
}

static size_t AllocateNodeData(
    ReflectionGenerationContext*    context,
    size_t                          size,
    size_t                          alignment,
    size_t                          count)
{
    size_t mask = alignment - 1;

    // Pad out the existing data to our alignment
    while( (context->data.Count() & mask) != 0 )
    {
        // TODO(tfoley): There's got to be something faster than this...
        context->data.Add(0);
    }

    // Starting offset for the new node data
    size_t result = context->data.Count();

    // Stride between elements (should always == size in C/C++)
    size_t stride = (size + alignment-1) & ~(alignment-1);

    // bytes to allocate
    size_t byteCount = stride * count;

    for( size_t bb = 0; bb < byteCount; ++bb )
    {
        // TODO(tfoley): There's got to be something faster than this...
        context->data.Add(0);
    }

    return result;
}

template<typename T>
static NodePtr<T> AllocateNode(
    ReflectionGenerationContext*    context)
{
    return NodePtr<T>(context, AllocateNodeData(context, sizeof(T), alignof(T), 1));
}

template<typename T>
static NodePtr<T> AllocateNodes(
    ReflectionGenerationContext*    context,
    size_t                          count = 1)
{
    return NodePtr<T>(context, AllocateNodeData(context, sizeof(T), alignof(T), count));
}

//

static NodePtr<ReflectionTypeNode> GenerateReflectionType(
    ReflectionGenerationContext*    context,
    RefPtr<ExpressionType>          type);

static NodePtr<ReflectionTypeLayoutNode> GenerateReflectionTypeLayout(
    ReflectionGenerationContext*    context,
    RefPtr<TypeLayout>              typeLayout);

//

static NodePtr<char> GenerateReflectionName(
    ReflectionGenerationContext*    context,
    String const&                   text)
{
    // TODO(tfoley): It would be a good idea to only emit each unique string
    // once, to avoid wasting a bunch of space.

    size_t textSize = text.Length();

// Need enough space for given text plus NULL byte
NodePtr<char> ptr = AllocateNodes<char>(context, textSize + 1);

memcpy(ptr, text.begin(), textSize);
ptr[textSize] = 0;

return ptr;
}

static void GenerateReflectionVar(
    ReflectionGenerationContext*    context,
    VarDeclBaseRef                  declRef,
    NodePtr<ReflectionVariableNode> info)
{
    info->flavor = ReflectionNodeFlavor::Variable;
    info->name = GenerateReflectionName(context, declRef.GetName());
    info->type = GenerateReflectionType(context, declRef.GetType());
}

static NodePtr<ReflectionTypeNode> GenerateReflectionType(
    ReflectionGenerationContext*    context,
    RefPtr<ExpressionType>          type)
{
    // TODO(tfoley: Don't emit the same type more than once...

    if (auto basicType = type->As<BasicExpressionType>())
    {
        auto info = AllocateNode<ReflectionScalarTypeNode>(context);
        info->flavor = ReflectionNodeFlavor::Type;
        info->SetKind(SPIRE_TYPE_KIND_SCALAR);
        switch (basicType->BaseType)
        {
#define CASE(BASE, TAG) \
        case BaseType::BASE: info->SetScalarType(SPIRE_SCALAR_TYPE_##TAG); break

            CASE(Void, VOID);
            CASE(Int, INT32);
            CASE(Float, FLOAT32);
            CASE(UInt, UINT32);
            CASE(Bool, BOOL);
            CASE(UInt64, UINT64);

#undef CASE

        default:
            assert(!"unexpected");
            info->SetScalarType(SPIRE_SCALAR_TYPE_NONE);
            break;
        }
        return info;
    }
    else if (auto vectorType = type->As<VectorExpressionType>())
    {
        auto info = AllocateNode<ReflectionVectorTypeNode>(context);
        info->flavor = ReflectionNodeFlavor::Type;
        info->SetKind(SPIRE_TYPE_KIND_VECTOR);
        info->SetElementCount(GetIntVal(vectorType->elementCount));
        info->elementType = GenerateReflectionType(context, vectorType->elementType).Cast<ReflectionScalarTypeNode>();
        return info;
    }
    else if (auto matrixType = type->As<MatrixExpressionType>())
    {
        auto info = AllocateNode<ReflectionMatrixTypeNode>(context);
        info->flavor = ReflectionNodeFlavor::Type;
        info->SetKind(SPIRE_TYPE_KIND_MATRIX);
        info->SetRowCount(GetIntVal(matrixType->rowCount));
        info->SetColumnCount(GetIntVal(matrixType->colCount));
        info->elementType = GenerateReflectionType(context, matrixType->elementType).Cast<ReflectionScalarTypeNode>();
        return info;
    }
    else if (auto constantBufferType = type->As<ConstantBufferType>())
    {
        auto info = AllocateNode<ReflectionConstantBufferTypeNode>(context);
        info->flavor = ReflectionNodeFlavor::Type;
        info->SetKind(SPIRE_TYPE_KIND_CONSTANT_BUFFER);

        auto elementTypeLayout = CreateTypeLayout(
            constantBufferType->elementType.Ptr(),
            // TODO: we should pick the layout rules from the definition!
            GetLayoutRulesImpl(LayoutRule::HLSLConstantBuffer));

        info->elementType = GenerateReflectionTypeLayout(
            context,
            elementTypeLayout);
        return info;
    }
    else if (auto samplerStateType = type->As<SamplerStateType>())
    {
        auto info = AllocateNode<ReflectionSamplerStateTypeNode>(context);
        info->flavor = ReflectionNodeFlavor::Type;
        info->SetKind(SPIRE_TYPE_KIND_SAMPLER_STATE);
        return info;
    }
    else if (auto textureType = type->As<TextureType>())
    {
        auto info = AllocateNode<ReflectionTextureTypeNode>(context);
        info->flavor = ReflectionNodeFlavor::Type;
        info->SetKind(SPIRE_TYPE_KIND_TEXTURE);
        info->SetShape(textureType->flavor);
        info->elementType = GenerateReflectionType(context, textureType->elementType);
        return info;
    }

    // TODO: need a better way to handle this stuff...
#define CASE(TYPE, FLAVOR)                                                              \
    else if(type->As<TYPE>()) do {                                                      \
    auto info = AllocateNode<ReflectionTextureTypeNode>(context);                       \
    info->flavor = ReflectionNodeFlavor::Type;                                          \
    info->SetKind(SPIRE_TYPE_KIND_TEXTURE);                                             \
    info->SetShape(FLAVOR);                                                             \
    info->elementType = GenerateReflectionType(context, type->As<TYPE>()->elementType); \
    return info;                                                                        \
    } while(0)

    CASE(HLSLBufferType,                    SPIRE_TEXTURE_BUFFER);
    CASE(HLSLRWBufferType,                  SPIRE_TEXTURE_BUFFER | SPIRE_TEXTURE_READ_WRITE_FLAG);
    CASE(HLSLStructuredBufferType,          SPIRE_TEXTURE_STRUCTURE_BUFFER);
    CASE(HLSLRWStructuredBufferType,        SPIRE_TEXTURE_STRUCTURE_BUFFER | SPIRE_TEXTURE_READ_WRITE_FLAG);

    // TODO: need to add flags for these cases too...
    CASE(HLSLAppendStructuredBufferType,    SPIRE_TEXTURE_STRUCTURE_BUFFER | SPIRE_TEXTURE_READ_WRITE_FLAG);
    CASE(HLSLConsumeStructuredBufferType,   SPIRE_TEXTURE_STRUCTURE_BUFFER | SPIRE_TEXTURE_READ_WRITE_FLAG);
#undef CASE

#define CASE(TYPE, FLAVOR)                                                              \
    else if(type->As<TYPE>()) do {                                                      \
    auto info = AllocateNode<ReflectionTextureTypeNode>(context);                       \
    info->flavor = ReflectionNodeFlavor::Type;                                          \
    info->SetKind(SPIRE_TYPE_KIND_TEXTURE);                                             \
    info->SetShape(FLAVOR);                                                             \
    info->elementType.raw = 0;                                                          \
    return info;                                                                        \
    } while(0)

    CASE(HLSLByteAddressBufferType,         SPIRE_TEXTURE_BYTE_ADDRESS_BUFFER);
    CASE(HLSLRWByteAddressBufferType,       SPIRE_TEXTURE_BYTE_ADDRESS_BUFFER | SPIRE_TEXTURE_READ_WRITE_FLAG);
#undef CASE


    else if (auto arrayType = type->As<ArrayExpressionType>())
    {
        auto info = AllocateNode<ReflectionArrayTypeNode>(context);
        info->flavor = ReflectionNodeFlavor::Type;
        info->SetKind(SPIRE_TYPE_KIND_ARRAY);
        info->elementType = GenerateReflectionType(context, arrayType->BaseType);
        info->elementCount = arrayType->ArrayLength ? GetIntVal(arrayType->ArrayLength) : 0;
        return info;
    }
    else if( auto declRefType = type->As<DeclRefType>() )
    {
        auto declRef = declRefType->declRef;
        if( auto structDeclRef = declRef.As<StructDeclRef>() )
        {
            auto info = AllocateNode<ReflectionStructTypeNode>(context);
            info->flavor = ReflectionNodeFlavor::Type;
            info->SetKind(SPIRE_TYPE_KIND_STRUCT);

            size_t fieldCount = structDeclRef.GetDecl()->GetFields().Count();
            info->SetFieldCount(fieldCount);

            auto fields = AllocateNodes<ReflectionVariableNode>(context, fieldCount);

            size_t fieldIndex = 0;
            for(auto ff : structDeclRef.GetFields())
            {
                auto field = fields + fieldIndex;

                // TODO: fill the damn thing in!!!!!
                GenerateReflectionVar(context, ff, field);

                fieldIndex++;
            }

            return info;
        }
    }

    assert(!"unexpected");
    return NodePtr<ReflectionTypeNode>();
}

static SpireParameterCategory ComputeReflectionParameterCategory(
    ReflectionGenerationContext*    context,
    LayoutResourceKind              kind)
{
    switch (kind)
    {
    case LayoutResourceKind::ConstantBuffer:
        return SPIRE_PARAMETER_CATEGORY_CONSTANT_BUFFER;
    case LayoutResourceKind::ShaderResource:
        return SPIRE_PARAMETER_CATEGORY_SHADER_RESOURCE;
    case LayoutResourceKind::UnorderedAccess:
        return SPIRE_PARAMETER_CATEGORY_UNORDERED_ACCESS;
    case LayoutResourceKind::SamplerState:
        return SPIRE_PARAMETER_CATEGORY_SAMPLER_STATE;

    default:
        return SPIRE_PARAMETER_CATEGORY_NONE;
    }
}

static SpireParameterCategory ComputeReflectionParameterCategory(
    ReflectionGenerationContext*        context,
    RefPtr<TypeLayout>                  typeLayout)
{
    // If we have any uniform data, then we are either uniform,
    // or mixed-type
    if (typeLayout->uniforms.size != 0)
    {
        // If we have any non-uniform data, then we are mixed.
        if (IsResourceKind(typeLayout->resources.kind))
        {
            return SPIRE_PARAMETER_CATEGORY_MIXED;
        }
        else
        {
            return SPIRE_PARAMETER_CATEGORY_UNIFORM;
        }
    }

    // Otherwise we only have resource fields.

    // If we have more then one kind of resource,
    // then the whole thing is mixed-type
    if (typeLayout->resources.next)
    {
        return SPIRE_PARAMETER_CATEGORY_MIXED;
    }
    
    // Otherwise look at the kind of resource
    return ComputeReflectionParameterCategory(context, typeLayout->resources.kind);
}


static NodePtr<ReflectionTypeLayoutNode> GenerateReflectionTypeLayout(
    ReflectionGenerationContext*    context,
    NodePtr<ReflectionTypeNode>     typeNode,
    RefPtr<TypeLayout>              typeLayout)
{
    // First create a type layout node of the correct kind
    NodePtr<ReflectionTypeLayoutNode> info;
    switch( typeNode->GetKind() )
    {
    default:
        assert(!"unexpected");
    case SPIRE_TYPE_KIND_CONSTANT_BUFFER:
        // TODO: do constant buffers need any special handling?
    case spire::TypeReflection::Kind::Scalar:
    case spire::TypeReflection::Kind::Vector:
    case spire::TypeReflection::Kind::Matrix:
    case SPIRE_TYPE_KIND_SAMPLER_STATE:
    case SPIRE_TYPE_KIND_TEXTURE:
        info = AllocateNode<ReflectionTypeLayoutNode>(context);
        break;

    case spire::TypeReflection::Kind::Array:
        {
            auto arrayTypeLayoutNode = AllocateNode<ReflectionArrayTypeLayoutNode>(context);
            info = arrayTypeLayoutNode;

            auto arrayTypeNode = typeNode.Cast<ReflectionArrayTypeNode>();

            auto arrayLayout = typeLayout.As<ArrayTypeLayout>();
            assert(arrayLayout);

            auto elementTypeNode = MakeNodePtr(context, arrayTypeNode->elementType.Ptr());
            auto elementTypeLayout = arrayLayout->elementTypeLayout;


            auto elementTypeLayoutNode = GenerateReflectionTypeLayout(
                context,
                elementTypeNode,
                elementTypeLayout);

            arrayTypeLayoutNode->elementTypeLayout = elementTypeLayoutNode;

            arrayTypeLayoutNode->elementStride = (ReflectionSize) arrayLayout->uniformStride;
        }
        break;

    case spire::TypeReflection::Kind::Struct:
        {
            auto structLayout = typeLayout.As<StructTypeLayout>();
            assert(structLayout);

            auto structInfo = AllocateNode<ReflectionStructTypeLayoutNode>(context);
            info = structInfo;

            auto structTypeNode = typeNode.Cast<ReflectionStructTypeNode>();

            size_t fieldCount = structTypeNode->GetFieldCount();
            assert(fieldCount == structLayout->fields999.Count());

            auto fieldLayoutNodes = AllocateNodes<ReflectionVariableLayoutNode>(context, fieldCount);
            for( size_t ff = 0; ff < fieldCount; ++ff )
            {
                auto fieldNode = MakeNodePtr(context, structTypeNode->GetFieldByIndex(ff));
                auto fieldLayoutNode = fieldLayoutNodes + ff;

                auto fieldVarLayout = structLayout->fields999[int(ff)];
                auto fieldTypeLayout = fieldVarLayout->typeLayout;

                auto fieldTypeLayoutNode = GenerateReflectionTypeLayout(
                    context,
                    MakeNodePtr(context, fieldNode->GetType()),
                    fieldTypeLayout);

                // Need to populate values for the field
                fieldLayoutNode->flavor = ReflectionNodeFlavor::VariableLayout;
                fieldLayoutNode->variable = fieldNode;
                fieldLayoutNode->typeLayout = fieldTypeLayoutNode;

                // Need to fill in offset information here!!!
                auto fieldCategory = ComputeReflectionParameterCategory(context, fieldTypeLayout);
                if(fieldCategory == SPIRE_PARAMETER_CATEGORY_MIXED)
                {
                    List<ReflectionSize> offsetsData;

                    // Offset data needs to match the ordering of categories
                    // in the associated type.
                    int categoryCount = fieldTypeLayoutNode->typeLayout.categoryCount;
                    for(int cc = 0; cc < categoryCount; ++cc)
                    {
                        auto category = fieldTypeLayoutNode->size.mixed[cc].category;
                        ReflectionSize offset = 0;
                        if(category == SPIRE_PARAMETER_CATEGORY_UNIFORM)
                        {
                            offset = ReflectionSize(fieldVarLayout->uniformOffset);
                        }
                        else
                        {
                            for(auto rr = &fieldVarLayout->resources; rr; rr = rr->next.Ptr())
                            {
                                if(ComputeReflectionParameterCategory(context, rr->kind) == category)
                                {
                                    offset = rr->index;
                                    break;
                                }
                            }
                        }

                        offsetsData.Add(offset);
                    }
                    assert(offsetsData.Count() == categoryCount);

                    auto offsets = AllocateNodes<ReflectionSize>(context, categoryCount);
                    for(int cc = 0; cc < categoryCount; ++cc)
                    {
                        offsets[cc] = offsetsData[cc];
                    }
                    fieldLayoutNode->offset.mixed = offsets;
                }
                else if(fieldCategory == SPIRE_PARAMETER_CATEGORY_UNIFORM)
                {
                    fieldLayoutNode->offset.simple = (ReflectionSize) fieldVarLayout->uniformOffset;
                }
                else
                {
                    assert(fieldVarLayout->resources.kind != LayoutResourceKind::Invalid);
                    fieldLayoutNode->offset.simple = (ReflectionSize) fieldVarLayout->resources.index;
                }
            }
        }
        break;
    }

    // Next, fill in the common fields, shared by all cases
    info->flavor = ReflectionNodeFlavor::TypeLayout;
    info->type = typeNode;

    auto category = ComputeReflectionParameterCategory(context, typeLayout);
    info->typeLayout.category = category;
    info->typeLayout.categoryCount = 0;
    if(category == SPIRE_PARAMETER_CATEGORY_MIXED)
    {
        List<ReflectionTypeSizeInfo> sizeInfosData;
        if(typeLayout->uniforms.size)
        {
            ReflectionTypeSizeInfo sizeInfo;
            sizeInfo.category = SPIRE_PARAMETER_CATEGORY_UNIFORM;
            sizeInfo.size = ReflectionSize(typeLayout->uniforms.size);
            sizeInfosData.Add(sizeInfo);
        }
        if(typeLayout->resources.kind != LayoutResourceKind::Invalid)
        {
            for(auto rr = &typeLayout->resources; rr; rr = rr->next.Ptr())
            {
                ReflectionTypeSizeInfo sizeInfo;
                sizeInfo.category = ComputeReflectionParameterCategory(context, rr->kind);
                sizeInfo.size = rr->count;
                sizeInfosData.Add(sizeInfo);
            }
        }

        int categoryCount = sizeInfosData.Count();
        auto sizeInfos = AllocateNodes<ReflectionTypeSizeInfo>(context, categoryCount);
        for( int cc = 0; cc < categoryCount; ++cc )
        {
            sizeInfos[cc] = sizeInfosData[cc];
        }

        info->typeLayout.categoryCount = ReflectionSize(categoryCount);
        info->size.mixed = sizeInfos;
    }
    else if(category == SPIRE_PARAMETER_CATEGORY_UNIFORM)
    {
        assert(typeLayout->uniforms.size);
        info->size.simple = (ReflectionSize) typeLayout->uniforms.size;
    }
    else
    {
        assert(typeLayout->resources.kind != LayoutResourceKind::Invalid);
        assert(typeLayout->resources.count);
        info->size.simple = typeLayout->resources.count;
    }

    return info;
}

static NodePtr<ReflectionTypeLayoutNode> GenerateReflectionTypeLayout(
    ReflectionGenerationContext*    context,
    RefPtr<TypeLayout>              typeLayout)
{
    auto typeNode = GenerateReflectionType(context, typeLayout->type);

    return GenerateReflectionTypeLayout(context, typeNode, typeLayout);
}

static bool IsReflectionParameter(
    ReflectionGenerationContext*        context,
    RefPtr<Decl>                        decl)
{
    if( auto varDecl = decl.As<VarDeclBase>() )
    {
        // We need to determine if this variable represents a shader
        // parameter, or just an ordinary global variable...
        if(varDecl->HasModifier<HLSLStaticModifier>())
            return false;

        // TODO(tfoley): there may be other cases that we need to handle here

        return true;
    }
    else
    {
        // Only variable declarations can represent parameters at global scope
        return false;
    }
}

static void GenerateReflectionParameter(
    ReflectionGenerationContext*        context,
    RefPtr<VarLayout>                   paramLayout,
    NodePtr<ReflectionParameterNode>    parameter)
{
    auto varDecl = paramLayout->varDecl.GetDecl();

    // Just to confirm that we aren't applying this logic to something we shouldn't
    assert(!varDecl->HasModifier<HLSLStaticModifier>());

    // Figure out what kind of parameter we are looking at:
    auto category = ComputeReflectionParameterCategory(context, paramLayout->typeLayout);

    parameter->flavor = ReflectionNodeFlavor::Parameter;
    parameter->name = GenerateReflectionName(context, varDecl->Name.Content);
    parameter->typeLayout = GenerateReflectionTypeLayout(context, paramLayout->typeLayout);
    parameter->binding.category = category;

    if (category == SPIRE_PARAMETER_CATEGORY_MIXED)
    {
        // More than one resource: need to handle this in a special way

        List<ReflectionParameterBindingInfo> bindingsData;

        // If there is any uniform data, then give it an offset
        if( paramLayout->typeLayout->uniforms.size )
        {
            ReflectionParameterBindingInfo info;
            info.category = SPIRE_PARAMETER_CATEGORY_UNIFORM;
            info.space = 0;
            info.index = (ReflectionSize) paramLayout->uniformOffset;

            bindingsData.Add(info);
        }
        for( auto rr = &paramLayout->resources; rr; rr = rr->next.Ptr() )
        {
            ReflectionParameterBindingInfo info;
            info.category = ComputeReflectionParameterCategory(context, rr->kind);
            info.space = (ReflectionSize) rr->space;
            info.index = (ReflectionSize) rr->index;

            bindingsData.Add(info);
        }

        ReflectionSize bindingCount = bindingsData.Count();
        auto bindings = AllocateNodes<ReflectionParameterBindingInfo>(context, bindingCount);
        for( ReflectionSize bb = 0; bb < bindingCount; ++bb )
        {
            bindings[bb] = bindingsData[bb];
        }

        parameter->binding.bindingCount = bindingCount;
        parameter->binding.bindings = bindings;
    }
    else if (category == SPIRE_PARAMETER_CATEGORY_UNIFORM)
    {
        // A uniform parameter inside of a parent buffer.
        parameter->binding.space = 0;
        parameter->binding.index = (ReflectionSize) paramLayout->uniformOffset;
    }
    else
    {
        // A resource parameter
        parameter->binding.space = (ReflectionSize) paramLayout->resources.space;
        parameter->binding.index = (ReflectionSize) paramLayout->resources.index;
    }
}


static ReflectionBlob* GenerateReflectionBlob(
    ReflectionGenerationContext*    context,
    CollectionOfTranslationUnits*   program)
{
    // We need the program to already have had parameter binding performed.
    auto programLayout = program->layout;
    if (!programLayout)
    {
        // TODO: error message
        return nullptr;
    }

    // We need to walk the declarations in the program, and look for those that identify logical shader parameters
    NodePtr<ReflectionBlob> blob = AllocateNode<ReflectionBlob>(context);
    blob->flavor = ReflectionNodeFlavor::Blob;

    // First let's count how many parameters there are to consider
    size_t parameterCount = programLayout->fields999.Count();
    blob->parameterCount = (ReflectionSize) parameterCount;

    // Now allocate space for the parameters (to go right after the blob itself) and fill them in
    NodePtr<ReflectionParameterNode> parameters = AllocateNodes<ReflectionParameterNode>(context, parameterCount);
    size_t parameterIndex = 0;
    for( auto paramLayout : programLayout->fields999 )
    {
        GenerateReflectionParameter(context, paramLayout, parameters + parameterIndex);
        parameterIndex++;
    }
    assert(parameterIndex == parameterCount);

    // We are done emitting things, so lets look at what we got


    // The total number of words emitted needs to fit in a word itself
    size_t dataSize = context->data.Count();

    // Need to update the blob info based on what we output
    blob->reflectionDataSize = (ReflectionSize) dataSize;

    // Make a raw memory allocation to hold the final data
    ReflectionBlob* result = (ReflectionBlob*) malloc(dataSize);
    memcpy(result, context->data.begin(), dataSize);

    return result;
}

// Debug helper code: dump reflection data after generation

struct PrettyWriter
{
    StringBuilder sb;
    bool startOfLine = true;
    int indent = 0;
};

static void adjust(PrettyWriter& writer)
{
    if (!writer.startOfLine)
        return;

    int indent = writer.indent;
    for (int ii = 0; ii < indent; ++ii)
        writer.sb << "    ";

    writer.startOfLine = false;
}

static void indent(PrettyWriter& writer)
{
    writer.indent++;
}

static void dedent(PrettyWriter& writer)
{
    writer.indent--;
}

static void write(PrettyWriter& writer, char const* text)
{
    // TODO: can do this more efficiently...
    char const* cursor = text;
    for(;;)
    {
        char c = *cursor++;
        if (!c) break;

        if (c == '\n')
        {
            writer.startOfLine = true;
        }
        else
        {
            adjust(writer);
        }

        writer.sb << c;
    }
}

static void write(PrettyWriter& writer, uint32_t val)
{
    adjust(writer);
    writer.sb << val;
}

static void write(PrettyWriter& writer, int32_t val)
{
    adjust(writer);
    writer.sb << val;
}

static void emitReflectionVarInfoJSON(PrettyWriter& writer, ReflectionVariableNode* var);
static void emitReflectionTypeLayoutJSON(PrettyWriter& writer, ReflectionTypeLayoutNode* type);
static void emitReflectionTypeJSON(PrettyWriter& writer, ReflectionTypeNode* type);

static void emitReflectionVarBindingInfoJSON(
    PrettyWriter&           writer,
    SpireParameterCategory  category,
    ReflectionSize          index,
    ReflectionSize          count,
    ReflectionSize          space = 0)
{
    if( category == SPIRE_PARAMETER_CATEGORY_UNIFORM )
    {
        write(writer,"\"kind\": \"uniform\"");
        write(writer, ", ");
        write(writer,"\"offset\": ");
        write(writer, index);
        write(writer, ", ");
        write(writer, "\"size\": ");
        write(writer, count);
    }
    else
    {
        write(writer, "\"kind\": \"");
        switch( category )
        {
    #define CASE(NAME, KIND) case SPIRE_PARAMETER_CATEGORY_##NAME: write(writer, #KIND); break
    CASE(CONSTANT_BUFFER, constantBuffer);
    CASE(SHADER_RESOURCE, shaderResource);
    CASE(UNORDERED_ACCESS, unorderedAccess);
    CASE(VERTEX_INPUT, vertexInput);
    CASE(FRAGMENT_OUTPUT, fragmentOutput);
    CASE(SAMPLER_STATE, samplerState);
    #undef CASE

        default:
            write(writer, "unknown");
            assert(!"unexpected");
            break;
        }
        write(writer, "\"");
        if( space )
        {
            write(writer, ", ");
            write(writer, "\"space\": ");
            write(writer, space);
        }
        write(writer, ", ");
        write(writer, "\"index\": ");
        write(writer, index);
        if( count != 1)
        {
            write(writer, ", ");
            write(writer, "\"count\": ");
            write(writer, count);
        }
    }
}

static void emitReflectionVarBindingInfoJSON(PrettyWriter& writer, ReflectionVariableLayoutNode* var)
{
    auto category = var->GetParameterCategory();
    if( category == SPIRE_PARAMETER_CATEGORY_MIXED )
    {
        write(writer,"\"bindings\": [\n");
        indent(writer);
        ReflectionSize bindingCount = var->GetTypeLayout()->typeLayout.categoryCount;
        assert(bindingCount);
        for( ReflectionSize bb = 0; bb < bindingCount; ++bb )
        {
            if (bb != 0) write(writer, ",\n");

            write(writer,"{");
            emitReflectionVarBindingInfoJSON(
                writer,
                var->GetTypeLayout()->size.mixed[bb].category,
                var->offset.mixed[bb],
                var->GetTypeLayout()->size.mixed[bb].size);
            write(writer,"}");
        }
        dedent(writer);
        write(writer,"\n]");
        return;
    }
    else
    {
        write(writer,"\"binding\": {");
        indent(writer);
        emitReflectionVarBindingInfoJSON(
            writer,
            category,
            var->offset.simple,
            var->GetTypeLayout()->size.simple);
        dedent(writer);
        write(writer,"}");
    }
}

static void emitReflectionNameInfoJSON(PrettyWriter& writer, char const* name)
{
    // TODO: deal with escaping special characters if/when needed
    write(writer, "\"name\": \"");
    write(writer, name);
    write(writer, "\"");
}

static void emitReflectionNameInfoJSON(PrettyWriter& writer, ReflectionVariableNode* var)
{
    emitReflectionNameInfoJSON(writer, var->GetName());
}

static void emitReflectionVarLayoutJSON(PrettyWriter& writer, ReflectionVariableLayoutNode* var)
{
    write(writer, "{\n");
    indent(writer);

    emitReflectionNameInfoJSON(writer, var->GetVariable());
    write(writer, ",\n");

    write(writer, "\"type\": ");
    emitReflectionTypeLayoutJSON(writer, var->GetTypeLayout());
    write(writer, ",\n");

    emitReflectionVarBindingInfoJSON(writer, var);

    dedent(writer);
    write(writer, "\n}");
}

static void emitReflectionScalarTypeInfoJSON(PrettyWriter& writer, SpireScalarType scalarType)
{
    write(writer, "\"scalarType\": \"");
    switch (scalarType)
    {
    default:
        write(writer, "unknown");
        assert(!"unexpected");
        break;
#define CASE(TAG, ID) case spire::TypeReflection::ScalarType::TAG: write(writer, #ID); break
        CASE(Void, void);
        CASE(Bool, bool);
        CASE(Int32, int32);
        CASE(UInt32, uint32);
        CASE(Int64, int64);
        CASE(UInt64, uint64);
        CASE(Float16, float16);
        CASE(Float32, float32);
        CASE(Float64, float64);
#undef CASE
    }
    write(writer, "\"");
}

static void emitReflectionTypeInfoJSON(PrettyWriter& writer, ReflectionTypeNode* type)
{
    switch( type->GetKind() )
    {
    case SPIRE_TYPE_KIND_SAMPLER_STATE:
        write(writer, "\"kind\": \"samplerState\"");
        break;

    case SPIRE_TYPE_KIND_TEXTURE:
        {
            auto shape = type->type.resource.shape;
            write(writer, "\"kind\": \"texture\"");
            write(writer, ",\n");
            write(writer, "\"baseShape\": \"");
            switch (shape & SPIRE_TEXTURE_BASE_SHAPE_MASK)
            {
            default:
                write(writer, "unknown");
                assert(!"unexpected");
                break;

#define CASE(SHAPE, NAME) case SPIRE_TEXTURE_##SHAPE: write(writer, #NAME); break
                CASE(1D, 1D);
                CASE(2D, 2D);
                CASE(3D, 3D);
                CASE(CUBE, cube);
                CASE(BUFFER, buffer);
                CASE(STRUCTURE_BUFFER, structuredBuffer);
                CASE(BYTE_ADDRESS_BUFFER, byteAddressBuffer);
#undef CASE
            }
            write(writer, "\"");
            if (shape & SPIRE_TEXTURE_ARRAY_FLAG)
            {
                write(writer, ",\n");
                write(writer, "\"array\": true");
            }
            if (shape & SPIRE_TEXTURE_MULTISAMPLE_FLAG)
            {
                write(writer, ",\n");
                write(writer, "\"multisample\": true");
            }
            if (shape & SPIRE_TEXTURE_READ_WRITE_FLAG)
            {
                write(writer, ",\n");
                write(writer, "\"readWrite\": true");
            }
            if (shape & SPIRE_TEXTURE_RASTER_ORDERED_FLAG)
            {
                write(writer, ",\n");
                write(writer, "\"rasterOrdered\": true");
            }
        }
        break;

    case SPIRE_TYPE_KIND_CONSTANT_BUFFER:
        write(writer, "\"kind\": \"constantBuffer\"");
        write(writer, ",\n");
        write(writer, "\"elementType\": ");
        emitReflectionTypeLayoutJSON(
            writer,
            ((ReflectionConstantBufferTypeNode*)type)->elementType);
        break;

    case SPIRE_TYPE_KIND_SCALAR:
        write(writer, "\"kind\": \"scalar\"");
        write(writer, ",\n");
        emitReflectionScalarTypeInfoJSON(writer, type->type.scalar.scalarType);
        break;

    case SPIRE_TYPE_KIND_VECTOR:
        write(writer, "\"kind\": \"vector\"");
        write(writer, ",\n");
        write(writer, "\"elementCount\": ");
        write(writer, type->type.vector.elementCount);
        write(writer, ",\n");
        write(writer, "\"elementType\": ");
        emitReflectionTypeJSON(
            writer,
            ((ReflectionVectorTypeNode*)type)->elementType);
        break;

    case SPIRE_TYPE_KIND_MATRIX:
        write(writer, "\"kind\": \"matrix\"");
        write(writer, ",\n");
        write(writer, "\"rowCount\": ");
        write(writer, type->type.matrix.rowCount);
        write(writer, ",\n");
        write(writer, "\"columnCount\": ");
        write(writer, type->type.matrix.columnCount);
        write(writer, ",\n");
        write(writer, "\"elementType\": ");
        emitReflectionTypeJSON(
            writer,
            ((ReflectionMatrixTypeNode*)type)->elementType);
        break;

    case SPIRE_TYPE_KIND_ARRAY:
        {
            auto arrayType = type->AsArray();
            write(writer, "\"kind\": \"array\"");
            write(writer, ",\n");
            write(writer, "\"elementCount\": ");
            write(writer, arrayType->GetElementCount());
            write(writer, ",\n");
            write(writer, "\"elementType\": ");
            emitReflectionTypeJSON(writer, arrayType->GetElementType());
        }
        break;

    case SPIRE_TYPE_KIND_STRUCT:
        {
            write(writer, "\"kind\": \"struct\",\n");
            write(writer, "\"fields\": [\n");
            indent(writer);

            auto structType = type->AsStruct();
            auto fieldCount = structType->GetFieldCount();
            for( uint32_t ff = 0; ff < fieldCount; ++ff )
            {
                if (ff != 0) write(writer, ",\n");
                emitReflectionVarInfoJSON(
                    writer,
                    structType->GetFieldByIndex(ff));
            }
            dedent(writer);
            write(writer, "\n]");
        }
        break;

    default:
        assert(!"unimplemented");
        break;
    }
}

static void emitReflectionTypeLayoutInfoJSON(PrettyWriter& writer, ReflectionTypeLayoutNode* type)
{
    switch( type->GetKind() )
    {
    default:
        emitReflectionTypeInfoJSON(writer, type->GetType());
        break;

    case SPIRE_TYPE_KIND_ARRAY:
        {
            auto arrayTypeLayout = type->AsArray();
            auto elementTypeLayout = arrayTypeLayout->GetElementTypeLayout();
            write(writer, "\"kind\": \"array\"");
            write(writer, ",\n");
            write(writer, "\"elementCount\": ");
            write(writer, arrayTypeLayout->GetElementCount());
            write(writer, ",\n");
            write(writer, "\"elementType\": ");
            emitReflectionTypeLayoutJSON(
                writer,
                elementTypeLayout);
            if (arrayTypeLayout->GetSize(SPIRE_PARAMETER_CATEGORY_UNIFORM) != 0)
            {
                write(writer, ",\n");
                write(writer, "\"uniformStride\": ");
                write(writer, arrayTypeLayout->GetElementStride(SPIRE_PARAMETER_CATEGORY_UNIFORM));
            }
        }
        break;

    case SPIRE_TYPE_KIND_STRUCT:
        {
            write(writer, "\"kind\": \"struct\",\n");
            write(writer, "\"fields\": [\n");
            indent(writer);

            auto structTypeLayout = type->AsStruct();
            auto fieldCount = structTypeLayout->GetFieldCount();
            for( uint32_t ff = 0; ff < fieldCount; ++ff )
            {
                if (ff != 0) write(writer, ",\n");
                emitReflectionVarLayoutJSON(
                    writer,
                    structTypeLayout->GetFieldByIndex(ff));
            }
            dedent(writer);
            write(writer, "\n]");
        }
        break;
    }

    // TODO: emit size info for types
}

static void emitReflectionTypeLayoutJSON(PrettyWriter& writer, ReflectionTypeLayoutNode* type)
{
    write(writer, "{\n");
    indent(writer);
    emitReflectionTypeLayoutInfoJSON(writer, type);
    dedent(writer);
    write(writer, "\n}");
}

static void emitReflectionTypeJSON(PrettyWriter& writer, ReflectionTypeNode* type)
{
    if( auto typeLayout = type->AsTypeLayout() )
    {
        emitReflectionTypeLayoutJSON(writer, typeLayout);
        return;
    }

    write(writer, "{\n");
    indent(writer);
    emitReflectionTypeInfoJSON(writer, type);
    dedent(writer);
    write(writer, "\n}");
}

static void emitReflectionVarInfoJSON(PrettyWriter& writer, ReflectionVariableNode* var)
{
    emitReflectionNameInfoJSON(writer, var);
    write(writer, ",\n");

    write(writer, "\"type\": ");
    emitReflectionTypeJSON(writer, var->GetType());
}

static void emitReflectionBindingInfoJSON(PrettyWriter& writer, ReflectionParameterNode* param)
{
    auto info = &param->binding;

    if( info->category == SPIRE_PARAMETER_CATEGORY_MIXED )
    {
        write(writer,"\"bindings\": [\n");
        indent(writer);

        ReflectionSize bindingCount = info->bindingCount;
        assert(bindingCount);
        ReflectionParameterBindingInfo* bindings = info->bindings;
        for( ReflectionSize bb = 0; bb < bindingCount; ++bb )
        {
            if (bb != 0) write(writer, ",\n");

            write(writer,"{");
            auto& binding = bindings[bb];
            emitReflectionVarBindingInfoJSON(
                writer,
                binding.category,
                binding.index,
                1, // TODO: compute the count to use
                binding.space);

            write(writer,"}");
        }
        dedent(writer);
        write(writer,"\n]");
    }
    else
    {
        write(writer,"\"binding\": {");
        indent(writer);

        emitReflectionVarBindingInfoJSON(
            writer,
            info->category,
            info->index,
            1, // TODO: compute the count to use
            info->space);

        dedent(writer);
        write(writer,"}");
    }
}

static void emitReflectionParamJSON(PrettyWriter& writer, ReflectionParameterNode* param)
{
    write(writer, "{\n");
    indent(writer);

    emitReflectionNameInfoJSON(writer, param->GetName());
    write(writer, ",\n");

    emitReflectionBindingInfoJSON(writer, param);
    write(writer, ",\n");

    write(writer, "\"type\": ");
    emitReflectionTypeLayoutJSON(writer, param->GetTypeLayout());

    dedent(writer);
    write(writer, "\n}");
}

static void emitReflectionBlobJSON(PrettyWriter& writer, ReflectionBlob* blob)
{
    write(writer, "{\n");
    indent(writer);
    write(writer, "\"parameters\": [\n");
    indent(writer);

    uint32_t paramCount = blob->GetParameterCount();
    for( uint32_t pp = 0; pp < paramCount; ++pp )
    {
        if (pp != 0) write(writer, ",\n");
        emitReflectionParamJSON(writer, blob->GetParameterByIndex(pp));
    }

    dedent(writer);
    write(writer, "\n]");
    dedent(writer);
    write(writer, "\n}\n");
}

ReflectionBlob* ReflectionBlob::Create(
    CollectionOfTranslationUnits*   program)
{
    ReflectionGenerationContext context;
    ReflectionBlob* blob = GenerateReflectionBlob(&context, program);
#if 0
    String debugDump = blob->emitAsJSON();
    OutputDebugStringA("REFLECTION BLOB\n");
    OutputDebugStringA(debugDump.begin());
#endif
    return blob;
}

// JSON emit logic

String ReflectionBlob::emitAsJSON()
{
    PrettyWriter writer;
    emitReflectionBlobJSON(writer, this);
    return writer.sb.ProduceString();
}

//

size_t ReflectionTypeLayoutNode::GetSize(SpireParameterCategory category) const
{
    auto thisCategory = GetParameterCategory();
    if( thisCategory == SPIRE_PARAMETER_CATEGORY_MIXED )
    {
        // Need to search for it!
        int categoryCount = typeLayout.categoryCount;
        for(int cc = 0; cc < categoryCount; ++cc)
        {
            if(size.mixed[cc].category == category)
                return size.mixed[cc].size;
        }
    }
    else if( category == thisCategory )
    {
        return size.simple;
    }

    // Default case: this type doesn't consume any resources of the given kind.
    return 0;
}


uint32_t ReflectionParameterNode::GetOffset(SpireParameterCategory category)
{
    auto thisCategory = GetCategory();
    if( thisCategory == SPIRE_PARAMETER_CATEGORY_MIXED )
    {
        // Need to search for it!
        ReflectionSize bindingCount = binding.bindingCount;
        ReflectionParameterBindingInfo const* bindings = binding.bindings;
        for( ReflectionSize bb = 0; bb < bindingCount; ++bb )
        {
            if(bindings[bb].category == category)
                return bindings[bb].index;
        }
    }
    else if( category == thisCategory )
    {
        return binding.index;
    }

    return 0;
}

uint32_t ReflectionParameterNode::GetSpace(SpireParameterCategory category)
{
    auto thisCategory = GetCategory();
    if( thisCategory == SPIRE_PARAMETER_CATEGORY_MIXED )
    {
        // Need to search for it!
        ReflectionSize bindingCount = binding.bindingCount;
        ReflectionParameterBindingInfo const* bindings = binding.bindings;
        for( ReflectionSize bb = 0; bb < bindingCount; ++bb )
        {
            if(bindings[bb].category == category)
                return bindings[bb].space;
        }
    }
    else if( category == thisCategory )
    {
        return binding.space;
    }

    return 0;
}



size_t ReflectionVariableLayoutNode::GetOffset(SpireParameterCategory category) const
{
    auto thisCategory = GetParameterCategory();
    if( thisCategory == SPIRE_PARAMETER_CATEGORY_MIXED )
    {
        // Need to search for it!
        auto typeLayout = GetTypeLayout();
        int categoryCount = typeLayout->typeLayout.categoryCount;
        for(int cc = 0; cc < categoryCount; ++cc)
        {
            if(typeLayout->size.mixed[cc].category == category)
                return offset.mixed[cc];

        }
    }
    else if( category == thisCategory )
    {
        return offset.simple;
    }

    return 0;
}

uint32_t ReflectionArrayTypeLayoutNode::GetElementStride(SpireParameterCategory category) const
{
    // For uniform data, the array may have a stride greater than the
    // size of the element type (even accounting for alignment)
    if( category == SPIRE_PARAMETER_CATEGORY_UNIFORM )
    {
        return elementStride;
    }

    // In all other cases, though, we can just use the size of the
    // element (e.g., the number of texture slots) as the stride
    // of the array.
    return (uint32_t) elementTypeLayout->GetSize(category);
}


}}
