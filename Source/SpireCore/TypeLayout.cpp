// TypeLayout.cpp
#include "TypeLayout.h"

#include "Syntax.h"

#include <assert.h>

namespace Spire {
namespace Compiler {

size_t RoundToAlignment(size_t offset, size_t alignment)
{
    size_t remainder = offset % alignment;
    if (remainder == 0)
        return offset;
    else
        return offset + (alignment - remainder);
}

static size_t RoundUpToPowerOfTwo( size_t value )
{
    // TODO(tfoley): I know this isn't a fast approach
    size_t result = 1;
    while (result < value)
        result *= 2;
    return result;
}

struct DefaultLayoutRulesImpl : SimpleLayoutRulesImpl
{
    // Get size and alignment for a single value of base type.
    SimpleLayoutInfo GetScalarLayout(BaseType baseType) override
    {
        switch (baseType)
        {
        case BaseType::Int:
        case BaseType::UInt:
        case BaseType::Float:
        case BaseType::Bool:
            return SimpleLayoutInfo( LayoutResourceKind::Uniform, 4, 4 );

        default:
            assert(!"unimplemented");
            return SimpleLayoutInfo( LayoutResourceKind::Uniform, 0, 1 );
        }
    }

    virtual SimpleLayoutInfo GetScalarLayout(spire::TypeReflection::ScalarType scalarType)
    {
        switch( scalarType )
        {
        case spire::TypeReflection::ScalarType::Void:       return SimpleLayoutInfo();
        case spire::TypeReflection::ScalarType::None:       return SimpleLayoutInfo();

        // TODO(tfoley): At some point we don't want to lay out `bool` as 4 bytes by default...
        case spire::TypeReflection::ScalarType::Bool:       return SimpleLayoutInfo( LayoutResourceKind::Uniform, 4,4);
        case spire::TypeReflection::ScalarType::Int32:      return SimpleLayoutInfo( LayoutResourceKind::Uniform, 4,4);
        case spire::TypeReflection::ScalarType::UInt32:     return SimpleLayoutInfo( LayoutResourceKind::Uniform, 4,4);
        case spire::TypeReflection::ScalarType::Int64:      return SimpleLayoutInfo( LayoutResourceKind::Uniform, 8,8);
        case spire::TypeReflection::ScalarType::UInt64:     return SimpleLayoutInfo( LayoutResourceKind::Uniform, 8,8);

        // TODO(tfoley): What actually happens if you use `half` in a constant buffer?
        case spire::TypeReflection::ScalarType::Float16:    return SimpleLayoutInfo( LayoutResourceKind::Uniform, 2,2);
        case spire::TypeReflection::ScalarType::Float32:    return SimpleLayoutInfo( LayoutResourceKind::Uniform, 4,4);
        case spire::TypeReflection::ScalarType::Float64:    return SimpleLayoutInfo( LayoutResourceKind::Uniform, 8,8);

        default:
            assert(!"unimplemented");
            return SimpleLayoutInfo();
        }
    }

    SimpleArrayLayoutInfo GetArrayLayout( SimpleLayoutInfo elementInfo, size_t elementCount) override
    {
        size_t stride = elementInfo.size;

        SimpleArrayLayoutInfo arrayInfo;
        arrayInfo.kind = elementInfo.kind;
        arrayInfo.size = stride * elementCount;
        arrayInfo.alignment = elementInfo.alignment;
        arrayInfo.elementStride = stride;
        return arrayInfo;
    }

    SimpleLayoutInfo GetVectorLayout(SimpleLayoutInfo elementInfo, size_t elementCount) override
    {
        SimpleLayoutInfo vectorInfo;
        vectorInfo.kind = elementInfo.kind;
        vectorInfo.size = elementInfo.size * elementCount;
        vectorInfo.alignment = elementInfo.alignment;
        return vectorInfo;
    }

    SimpleLayoutInfo GetMatrixLayout(SimpleLayoutInfo elementInfo, size_t rowCount, size_t columnCount) override
    {
        return GetArrayLayout(
            GetVectorLayout(elementInfo, columnCount),
            rowCount);
    }

    UniformLayoutInfo BeginStructLayout() override
    {
        UniformLayoutInfo structInfo(0, 1);
        return structInfo;
    }

    size_t AddStructField(UniformLayoutInfo* ioStructInfo, UniformLayoutInfo fieldInfo) override
    {
        // Skip zero-size fields
        if(fieldInfo.size == 0)
            return ioStructInfo->size;

        ioStructInfo->alignment = std::max(ioStructInfo->alignment, fieldInfo.alignment);
        ioStructInfo->size = RoundToAlignment(ioStructInfo->size, fieldInfo.alignment);
        size_t fieldOffset = ioStructInfo->size;
        ioStructInfo->size += fieldInfo.size;
        return fieldOffset;
    }


    void EndStructLayout(UniformLayoutInfo* ioStructInfo) override
    {
        ioStructInfo->size = RoundToAlignment(ioStructInfo->size, ioStructInfo->alignment);
    }
};

// Capture common behavior betwen HLSL and GLSL (`std140`) constnat buffer rules
struct DefaultConstantBufferLayoutRulesImpl : DefaultLayoutRulesImpl
{
    // The `std140` rules require that all array elements
    // be a multiple of 16 bytes.
    //
    // HLSL agrees.
    SimpleArrayLayoutInfo GetArrayLayout(SimpleLayoutInfo elementInfo, size_t elementCount) override
    {
        if(elementInfo.kind == LayoutResourceKind::Uniform)
        {
            if (elementInfo.alignment < 16)
                elementInfo.alignment = 16;
            elementInfo.size = RoundToAlignment(elementInfo.size, elementInfo.alignment);
        }
        return DefaultLayoutRulesImpl::GetArrayLayout(elementInfo, elementCount);
    }

    // The `std140` rules require that a `struct` type be
    // aligned to at least 16.
    //
    // HLSL agrees.
    UniformLayoutInfo BeginStructLayout() override
    {
        return UniformLayoutInfo(0, 16);
    }
};

struct GLSLConstantBufferLayoutRulesImpl : DefaultConstantBufferLayoutRulesImpl
{
};

struct Std140LayoutRulesImpl : GLSLConstantBufferLayoutRulesImpl
{
    // The `std140` rules require vectors to be aligned to the next power of two
    // up from their size (so a `float2` is 8-byte aligned, and a `float3` is
    // 16-byte aligned).
    SimpleLayoutInfo GetVectorLayout(SimpleLayoutInfo elementInfo, size_t elementCount) override
    {
        assert(elementInfo.kind == LayoutResourceKind::Uniform);
        SimpleLayoutInfo vectorInfo(
            LayoutResourceKind::Uniform,
            elementInfo.size * elementCount,
            RoundUpToPowerOfTwo(elementInfo.size * elementInfo.alignment));
        return vectorInfo;
    }
};

struct HLSLConstantBufferLayoutRulesImpl : DefaultConstantBufferLayoutRulesImpl
{
    // Can't let a `struct` field straddle a register (16-byte) boundary
    size_t AddStructField(UniformLayoutInfo* ioStructInfo, UniformLayoutInfo fieldInfo) override
    {
        // Skip zero-size fields
        if(fieldInfo.size == 0)
            return ioStructInfo->size;

        ioStructInfo->alignment = std::max(ioStructInfo->alignment, fieldInfo.alignment);
        ioStructInfo->size = RoundToAlignment(ioStructInfo->size, fieldInfo.alignment);

        size_t fieldOffset = ioStructInfo->size;
        size_t fieldSize = fieldInfo.size;

        // Would this field cross a 16-byte boundary?
        auto registerSize = 16;
        auto startRegister = fieldOffset / registerSize;
        auto endRegister = (fieldOffset + fieldSize - 1) / registerSize;
        if (startRegister != endRegister)
        {
            ioStructInfo->size = RoundToAlignment(ioStructInfo->size, size_t(registerSize));
            fieldOffset = ioStructInfo->size;
        }

        ioStructInfo->size += fieldInfo.size;
        return fieldOffset;
    }
};

struct HLSLStructuredBufferLayoutRulesImpl : DefaultLayoutRulesImpl
{
    // TODO: customize these to be correct...
};

struct Std430LayoutRulesImpl : GLSLConstantBufferLayoutRulesImpl
{
};

struct DefaultVaryingLayoutRulesImpl : DefaultLayoutRulesImpl
{
    LayoutResourceKind kind;

    DefaultVaryingLayoutRulesImpl(LayoutResourceKind kind)
        : kind(kind)
    {}


    // hook to allow differentiating for input/output
    virtual LayoutResourceKind getKind()
    {
        return kind;
    }

    SimpleLayoutInfo GetScalarLayout(BaseType baseType) override
    {
        // Assume that all scalars take up one "slot"
        return SimpleLayoutInfo(
            getKind(),
            1);
    }

    virtual SimpleLayoutInfo GetScalarLayout(spire::TypeReflection::ScalarType scalarType)
    {
        // Assume that all scalars take up one "slot"
        return SimpleLayoutInfo(
            getKind(),
            1);
    }

    SimpleLayoutInfo GetVectorLayout(SimpleLayoutInfo elementInfo, size_t elementCount) override
    {
        // Vectors take up one slot by default
        //
        // TODO: some platforms may decide that vectors of `double` need
        // special handling
        return SimpleLayoutInfo(
            getKind(),
            1);
    }
};

struct GLSLVaryingLayoutRulesImpl : DefaultVaryingLayoutRulesImpl
{
    GLSLVaryingLayoutRulesImpl(LayoutResourceKind kind)
        : DefaultVaryingLayoutRulesImpl(kind)
    {}
};

struct HLSLVaryingLayoutRulesImpl : DefaultVaryingLayoutRulesImpl
{
    HLSLVaryingLayoutRulesImpl(LayoutResourceKind kind)
        : DefaultVaryingLayoutRulesImpl(kind)
    {}
};

//

struct GLSLSpecializationConstantLayoutRulesImpl : DefaultLayoutRulesImpl
{
    LayoutResourceKind getKind()
    {
        return LayoutResourceKind::SpecializationConstant;
    }

    SimpleLayoutInfo GetScalarLayout(BaseType baseType) override
    {
        // Assume that all scalars take up one "slot"
        return SimpleLayoutInfo(
            getKind(),
            1);
    }

    virtual SimpleLayoutInfo GetScalarLayout(spire::TypeReflection::ScalarType scalarType)
    {
        // Assume that all scalars take up one "slot"
        return SimpleLayoutInfo(
            getKind(),
            1);
    }

    SimpleLayoutInfo GetVectorLayout(SimpleLayoutInfo elementInfo, size_t elementCount) override
    {
        // GLSL doesn't support vectors of specialization constants,
        // but we will assume that, if supported, they would use one slot per element.
        return SimpleLayoutInfo(
            getKind(),
            elementCount);
    }
};

GLSLSpecializationConstantLayoutRulesImpl kGLSLSpecializationConstantLayoutRulesImpl;

//

struct GLSLObjectLayoutRulesImpl : ObjectLayoutRulesImpl
{
    virtual SimpleLayoutInfo GetObjectLayout(ShaderParameterKind kind) override
    {
        switch( kind )
        {
        case ShaderParameterKind::ConstantBuffer:
        case ShaderParameterKind::TextureUniformBuffer:
        case ShaderParameterKind::ShaderStorageBuffer:
        case ShaderParameterKind::StructuredBuffer:
        case ShaderParameterKind::SampledBuffer:
        case ShaderParameterKind::RawBuffer:
        case ShaderParameterKind::Buffer:
        case ShaderParameterKind::Texture:
        case ShaderParameterKind::MutableStructuredBuffer:
        case ShaderParameterKind::MutableSampledBuffer:
        case ShaderParameterKind::MutableRawBuffer:
        case ShaderParameterKind::MutableBuffer:
        case ShaderParameterKind::MutableTexture:
        case ShaderParameterKind::TextureSampler:
        case ShaderParameterKind::MutableTextureSampler:
        case ShaderParameterKind::SamplerState:
            return SimpleLayoutInfo(LayoutResourceKind::DescriptorTableSlot, 1);

        default:
            assert(!"unimplemented");
            return SimpleLayoutInfo();
        }
    }
};
GLSLObjectLayoutRulesImpl kGLSLObjectLayoutRulesImpl;

struct HLSLObjectLayoutRulesImpl : ObjectLayoutRulesImpl
{
    virtual SimpleLayoutInfo GetObjectLayout(ShaderParameterKind kind) override
    {
        switch( kind )
        {
        case ShaderParameterKind::ConstantBuffer:
            return SimpleLayoutInfo(LayoutResourceKind::ConstantBuffer, 1);

        case ShaderParameterKind::TextureUniformBuffer:
        case ShaderParameterKind::StructuredBuffer:
        case ShaderParameterKind::SampledBuffer:
        case ShaderParameterKind::RawBuffer:
        case ShaderParameterKind::Buffer:
        case ShaderParameterKind::Texture:
            return SimpleLayoutInfo(LayoutResourceKind::ShaderResource, 1);

        case ShaderParameterKind::MutableStructuredBuffer:
        case ShaderParameterKind::MutableSampledBuffer:
        case ShaderParameterKind::MutableRawBuffer:
        case ShaderParameterKind::MutableBuffer:
        case ShaderParameterKind::MutableTexture:
            return SimpleLayoutInfo(LayoutResourceKind::UnorderedAccess, 1);

        case ShaderParameterKind::SamplerState:
            return SimpleLayoutInfo(LayoutResourceKind::SamplerState, 1);

        case ShaderParameterKind::TextureSampler:
        case ShaderParameterKind::MutableTextureSampler:
            // TODO: how to handle these?
        default:
            assert(!"unimplemented");
            return SimpleLayoutInfo();
        }
    }
};
HLSLObjectLayoutRulesImpl kHLSLObjectLayoutRulesImpl;

Std140LayoutRulesImpl kStd140LayoutRulesImpl;
Std430LayoutRulesImpl kStd430LayoutRulesImpl;
HLSLConstantBufferLayoutRulesImpl kHLSLConstantBufferLayoutRulesImpl;
HLSLStructuredBufferLayoutRulesImpl kHLSLStructuredBufferLayoutRulesImpl;

GLSLVaryingLayoutRulesImpl kGLSLVaryingInputLayoutRulesImpl(LayoutResourceKind::VertexInput);
GLSLVaryingLayoutRulesImpl kGLSLVaryingOutputLayoutRulesImpl(LayoutResourceKind::FragmentOutput);

HLSLVaryingLayoutRulesImpl kHLSLVaryingInputLayoutRulesImpl(LayoutResourceKind::VertexInput);
HLSLVaryingLayoutRulesImpl kHLSLVaryingOutputLayoutRulesImpl(LayoutResourceKind::FragmentOutput);

//

struct GLSLLayoutRulesFamilyImpl : LayoutRulesFamilyImpl
{
    virtual LayoutRulesImpl* getConstantBufferRules() override;
    virtual LayoutRulesImpl* getTextureBufferRules() override;
    virtual LayoutRulesImpl* getVaryingInputRules() override;
    virtual LayoutRulesImpl* getVaryingOutputRules() override;
    virtual LayoutRulesImpl* getSpecializationConstantRules() override;
    virtual LayoutRulesImpl* getShaderStorageBufferRules() override;
};

struct HLSLLayoutRulesFamilyImpl : LayoutRulesFamilyImpl
{
    virtual LayoutRulesImpl* getConstantBufferRules() override;
    virtual LayoutRulesImpl* getTextureBufferRules() override;
    virtual LayoutRulesImpl* getVaryingInputRules() override;
    virtual LayoutRulesImpl* getVaryingOutputRules() override;
    virtual LayoutRulesImpl* getSpecializationConstantRules() override;
    virtual LayoutRulesImpl* getShaderStorageBufferRules() override;
};

GLSLLayoutRulesFamilyImpl kGLSLLayoutRulesFamilyImpl;
HLSLLayoutRulesFamilyImpl kHLSLLayoutRulesFamilyImpl;


// GLSL cases

LayoutRulesImpl kStd140LayoutRulesImpl_ = {
    &kGLSLLayoutRulesFamilyImpl, &kStd140LayoutRulesImpl, &kGLSLObjectLayoutRulesImpl,
};

LayoutRulesImpl kStd430LayoutRulesImpl_ = {
    &kGLSLLayoutRulesFamilyImpl, &kStd430LayoutRulesImpl, &kGLSLObjectLayoutRulesImpl,
};

LayoutRulesImpl kGLSLVaryingInputLayoutRulesImpl_ = {
    &kGLSLLayoutRulesFamilyImpl, &kGLSLVaryingInputLayoutRulesImpl, &kGLSLObjectLayoutRulesImpl,
};

LayoutRulesImpl kGLSLVaryingOutputLayoutRulesImpl_ = {
    &kGLSLLayoutRulesFamilyImpl, &kGLSLVaryingOutputLayoutRulesImpl, &kGLSLObjectLayoutRulesImpl,
};

LayoutRulesImpl kGLSLSpecializationConstantLayoutRulesImpl_ = {
    &kGLSLLayoutRulesFamilyImpl, &kGLSLSpecializationConstantLayoutRulesImpl, &kGLSLObjectLayoutRulesImpl,
};

// HLSL cases

LayoutRulesImpl kHLSLConstantBufferLayoutRulesImpl_ = {
    &kHLSLLayoutRulesFamilyImpl, &kHLSLConstantBufferLayoutRulesImpl, &kHLSLObjectLayoutRulesImpl,
};

LayoutRulesImpl kHLSLStructuredBufferLayoutRulesImpl_ = {
    &kHLSLLayoutRulesFamilyImpl, &kHLSLStructuredBufferLayoutRulesImpl, &kHLSLObjectLayoutRulesImpl,
};

LayoutRulesImpl kHLSLVaryingInputLayoutRulesImpl_ = {
    &kHLSLLayoutRulesFamilyImpl, &kHLSLVaryingInputLayoutRulesImpl, &kHLSLObjectLayoutRulesImpl,
};

LayoutRulesImpl kHLSLVaryingOutputLayoutRulesImpl_ = {
    &kHLSLLayoutRulesFamilyImpl, &kHLSLVaryingOutputLayoutRulesImpl, &kHLSLObjectLayoutRulesImpl,
};

//

LayoutRulesImpl* GLSLLayoutRulesFamilyImpl::getConstantBufferRules()
{
    return &kStd140LayoutRulesImpl_;
}

LayoutRulesImpl* GLSLLayoutRulesFamilyImpl::getTextureBufferRules()
{
    return nullptr;
}

LayoutRulesImpl* GLSLLayoutRulesFamilyImpl::getVaryingInputRules()
{
    return &kGLSLVaryingInputLayoutRulesImpl_;
}

LayoutRulesImpl* GLSLLayoutRulesFamilyImpl::getVaryingOutputRules()
{
    return &kGLSLVaryingOutputLayoutRulesImpl_;
}

LayoutRulesImpl* GLSLLayoutRulesFamilyImpl::getSpecializationConstantRules()
{
    return &kGLSLSpecializationConstantLayoutRulesImpl_;
}

LayoutRulesImpl* GLSLLayoutRulesFamilyImpl::getShaderStorageBufferRules()
{
    return &kStd430LayoutRulesImpl_;
}

//

LayoutRulesImpl* HLSLLayoutRulesFamilyImpl::getConstantBufferRules()
{
    return &kHLSLConstantBufferLayoutRulesImpl_;
}

LayoutRulesImpl* HLSLLayoutRulesFamilyImpl::getTextureBufferRules()
{
    return nullptr;
}

LayoutRulesImpl* HLSLLayoutRulesFamilyImpl::getVaryingInputRules()
{
    return &kHLSLVaryingInputLayoutRulesImpl_;
}

LayoutRulesImpl* HLSLLayoutRulesFamilyImpl::getVaryingOutputRules()
{
    return &kHLSLVaryingOutputLayoutRulesImpl_;
}

LayoutRulesImpl* HLSLLayoutRulesFamilyImpl::getSpecializationConstantRules()
{
    return nullptr;
}

LayoutRulesImpl* HLSLLayoutRulesFamilyImpl::getShaderStorageBufferRules()
{
    return nullptr;
}

//

LayoutRulesImpl* GetLayoutRulesImpl(LayoutRule rule)
{
    switch (rule)
    {
    case LayoutRule::Std140:                return &kStd140LayoutRulesImpl_;
    case LayoutRule::Std430:                return &kStd430LayoutRulesImpl_;
    case LayoutRule::HLSLConstantBuffer:    return &kHLSLConstantBufferLayoutRulesImpl_;
    case LayoutRule::HLSLStructuredBuffer:  return &kHLSLStructuredBufferLayoutRulesImpl_;
    default:
        return nullptr;
    }
}

LayoutRulesFamilyImpl* GetLayoutRulesFamilyImpl(LayoutRulesFamily rule)
{
    switch (rule)
    {
    case LayoutRulesFamily::HLSL:   return &kHLSLLayoutRulesFamilyImpl;
    case LayoutRulesFamily::GLSL:   return &kGLSLLayoutRulesFamilyImpl;
    default:
        return nullptr;
    }
}

LayoutRulesFamilyImpl* GetLayoutRulesFamilyImpl(SourceLanguage language)
{
    switch (language)
    {
    case SourceLanguage::Spire:
    case SourceLanguage::HLSL:
        return &kHLSLLayoutRulesFamilyImpl;

    case SourceLanguage::GLSL:
        return &kGLSLLayoutRulesFamilyImpl;

    default:
        return nullptr;
    }
}


static int GetElementCount(RefPtr<IntVal> val)
{
    if (auto constantVal = val.As<ConstantIntVal>())
    {
        return constantVal->value;
    }
    assert(!"unexpected");
    return 0;
}

bool IsResourceKind(LayoutResourceKind kind)
{
    switch (kind)
    {
    case LayoutResourceKind::None:
    case LayoutResourceKind::Uniform:
        return false;

    default:
        return true;
    }

}

SimpleLayoutInfo GetSimpleLayoutImpl(
    SimpleLayoutInfo        info,
    RefPtr<ExpressionType>  type,
    LayoutRulesImpl*        rules,
    RefPtr<TypeLayout>*     outTypeLayout)
{
    if (outTypeLayout)
    {
        RefPtr<TypeLayout> typeLayout = new TypeLayout();
        *outTypeLayout = typeLayout;

        typeLayout->type = type;
        typeLayout->rules = rules;

        typeLayout->uniformAlignment = info.alignment;

        typeLayout->addResourceUsage(info.kind, info.size);
    }

    return info;
}

static SimpleLayoutInfo getParameterBlockLayoutInfo(
    RefPtr<ParameterBlockType>  type,
    LayoutRulesImpl*            rules)
{
    if( type->As<ConstantBufferType>() )
    {
        return rules->GetObjectLayout(ShaderParameterKind::ConstantBuffer);
    }
    else if( type->As<TextureBufferType>() )
    {
        return rules->GetObjectLayout(ShaderParameterKind::TextureUniformBuffer);
    }
    else if( type->As<GLSLShaderStorageBufferType>() )
    {
        return rules->GetObjectLayout(ShaderParameterKind::ShaderStorageBuffer);
    }
    // TODO: the vertex-input and fragment-output cases should
    // only actually apply when we are at the appropriate stage in
    // the pipeline...
    else if( type->As<GLSLInputParameterBlockType>() )
    {
        return SimpleLayoutInfo(LayoutResourceKind::VertexInput, 0);
    }
    else if( type->As<GLSLOutputParameterBlockType>() )
    {
        return SimpleLayoutInfo(LayoutResourceKind::FragmentOutput, 0);
    }
    else
    {
        assert(!"unexpected");
        return SimpleLayoutInfo();
    }
}


RefPtr<ParameterBlockTypeLayout>
createParameterBlockTypeLayout(
    RefPtr<ParameterBlockType>  parameterBlockType,
    RefPtr<TypeLayout>          elementTypeLayout,
    LayoutRulesImpl*            rules)
{
    auto info = getParameterBlockLayoutInfo(
        parameterBlockType,
        rules);

    auto typeLayout = new ParameterBlockTypeLayout();

    typeLayout->type = parameterBlockType;
    typeLayout->rules = rules;

    typeLayout->elementTypeLayout = elementTypeLayout;

    // The layout of the constant buffer if it gets stored
    // in another constant buffer is just what we computed
    // originally (which should be a single binding "slot"
    // and hence no uniform data).
    // 
    typeLayout->uniformAlignment = info.alignment;
    assert(!typeLayout->FindResourceInfo(LayoutResourceKind::Uniform));
    assert(typeLayout->uniformAlignment == 1);

    // TODO(tfoley): There is a subtle question here of whether
    // a constant buffer declaration that then contains zero
    // bytes of uniform data should actually allocate a CB
    // binding slot. For now I'm going to try to ignore it,
    // but handling this robustly could let other code
    // simply handle the "global scope" as a giant outer
    // CB declaration...

    // Make sure that we allocate resource usage for the
    // parameter block itself.
    if( info.size )
    {
        typeLayout->addResourceUsage(
            info.kind,
            info.size);
    }

    // Now, if the element type itself had any resources, then
    // we need to make these part of the layout for our block
    //
    // TODO: re-consider this decision, since it creates
    // complications...
    for( auto elementResourceInfo : elementTypeLayout->resourceInfos )
    {
        // Skip uniform data, since that is encapsualted behind the constant buffer
        if(elementResourceInfo.kind == LayoutResourceKind::Uniform)
            break;

        typeLayout->addResourceUsage(elementResourceInfo);
    }

    return typeLayout;
}

LayoutRulesImpl* getParameterBufferElementTypeLayoutRules(
    RefPtr<ParameterBlockType>  parameterBlockType,
    LayoutRulesImpl*            rules)
{
    if( parameterBlockType->As<ConstantBufferType>() )
    {
        return rules->getLayoutRulesFamily()->getConstantBufferRules();
    }
    else if( parameterBlockType->As<TextureBufferType>() )
    {
        return rules->getLayoutRulesFamily()->getTextureBufferRules();
    }
    else if( parameterBlockType->As<GLSLInputParameterBlockType>() )
    {
        return rules->getLayoutRulesFamily()->getVaryingInputRules();
    }
    else if( parameterBlockType->As<GLSLOutputParameterBlockType>() )
    {
        return rules->getLayoutRulesFamily()->getVaryingOutputRules();
    }
    else if( parameterBlockType->As<GLSLShaderStorageBufferType>() )
    {
        return rules->getLayoutRulesFamily()->getShaderStorageBufferRules();
    }
    else
    {
        assert(!"unexpected");
        return nullptr;
    }
}

RefPtr<ParameterBlockTypeLayout>
createParameterBlockTypeLayout(
    RefPtr<ParameterBlockType>  parameterBlockType,
    LayoutRulesImpl*            rules)
{
    // Determine the layout rules to use for the contents of the block
    auto parameterBlockLayoutRules = getParameterBufferElementTypeLayoutRules(
        parameterBlockType,
        rules);

    // Create and save type layout for the buffer contents.
    auto elementTypeLayout = CreateTypeLayout(
        parameterBlockType->elementType.Ptr(),
        parameterBlockLayoutRules);

    return createParameterBlockTypeLayout(
        parameterBlockType,
        elementTypeLayout,
        rules);
}

// Create a type layout for a structured buffer type.
RefPtr<StructuredBufferTypeLayout>
createStructuredBufferTypeLayout(
    ShaderParameterKind     kind,
    RefPtr<ExpressionType>  structuredBufferType,
    RefPtr<TypeLayout>      elementTypeLayout,
    LayoutRulesImpl*        rules)
{
    auto info = rules->GetObjectLayout(kind);

    auto typeLayout = new StructuredBufferTypeLayout();

    typeLayout->type = structuredBufferType;
    typeLayout->rules = rules;

    typeLayout->elementTypeLayout = elementTypeLayout;

    typeLayout->uniformAlignment = info.alignment;
    assert(!typeLayout->FindResourceInfo(LayoutResourceKind::Uniform));
    assert(typeLayout->uniformAlignment == 1);

    if( info.size != 0 )
    {
        typeLayout->addResourceUsage(info.kind, info.size);
    }

    // Note: for now we don't deal with the case of a structured
    // buffer that might contain anything other than "uniform" data,
    // because there really isn't a way to implement that.

    return typeLayout;
}

// Create a type layout for a structured buffer type.
RefPtr<StructuredBufferTypeLayout>
createStructuredBufferTypeLayout(
    ShaderParameterKind     kind,
    RefPtr<ExpressionType>  structuredBufferType,
    RefPtr<ExpressionType>  elementType,
    LayoutRulesImpl*        rules)
{
    // TODO(tfoley): need to compute the layout for the constant
    // buffer's contents...
    auto structuredBufferLayoutRules = GetLayoutRulesImpl(
        LayoutRule::HLSLStructuredBuffer);

    // Create and save type layout for the buffer contents.
    auto elementTypeLayout = CreateTypeLayout(
        elementType.Ptr(),
        structuredBufferLayoutRules);

    return createStructuredBufferTypeLayout(
        kind,
        structuredBufferType,
        elementTypeLayout,
        rules);

}

SimpleLayoutInfo GetLayoutImpl(
    ExpressionType*     type,
    LayoutRulesImpl*    rules,
    RefPtr<TypeLayout>* outTypeLayout)
{
    if (auto parameterBlockType = type->As<ParameterBlockType>())
    {
        // If the user is just interested in uniform layout info,
        // then this is easy: a `ConstantBuffer<T>` is really no
        // different from a `Texture2D<U>` in terms of how it
        // should be handled as a member of a container.
        //
        auto info = getParameterBlockLayoutInfo(parameterBlockType, rules);

        // The more interesting case, though, is when the user
        // is requesting us to actually create a `TypeLayout`,
        // since in that case we need to:
        //
        // 1. Compute a layout for the data inside the constant
        //    buffer, including offsets, etc.
        //
        // 2. Compute information about any object types inside
        //    the constant buffer, which need to be surfaces out
        //    to the top level.
        //
        if (outTypeLayout)
        {
            *outTypeLayout = createParameterBlockTypeLayout(
                parameterBlockType,
                rules);
        }

        return info;
    }
    else if (auto samplerStateType = type->As<SamplerStateType>())
    {
        return GetSimpleLayoutImpl(
            rules->GetObjectLayout(ShaderParameterKind::SamplerState),
            type,
            rules,
            outTypeLayout);
    }
    else if (auto textureType = type->As<TextureType>())
    {
        // TODO: the logic here should really be defined by the rules,
        // and not at this top level...
        ShaderParameterKind kind;
        switch( textureType->getAccess() )
        {
        default:
            kind = ShaderParameterKind::MutableTexture;
            break;

        case SPIRE_RESOURCE_ACCESS_READ:
            kind = ShaderParameterKind::Texture;
            break;
        }

        return GetSimpleLayoutImpl(
            rules->GetObjectLayout(kind),
            type,
            rules,
            outTypeLayout);
    }
    else if (auto textureSamplerType = type->As<TextureSamplerType>())
    {
        // TODO: the logic here should really be defined by the rules,
        // and not at this top level...
        ShaderParameterKind kind;
        switch( textureSamplerType->getAccess() )
        {
        default:
            kind = ShaderParameterKind::MutableTextureSampler;
            break;

        case SPIRE_RESOURCE_ACCESS_READ:
            kind = ShaderParameterKind::TextureSampler;
            break;
        }

        return GetSimpleLayoutImpl(
            rules->GetObjectLayout(kind),
            type,
            rules,
            outTypeLayout);
    }

    // TODO: need a better way to handle this stuff...
#define CASE(TYPE, KIND)                                                \
    else if(auto type_##TYPE = type->As<TYPE>()) do {                   \
        auto info = rules->GetObjectLayout(ShaderParameterKind::KIND);  \
        if (outTypeLayout)                                              \
        {                                                               \
            *outTypeLayout = createStructuredBufferTypeLayout(          \
                ShaderParameterKind::KIND,                              \
                type_##TYPE,                                            \
                type_##TYPE->elementType.Ptr(),                         \
                rules);                                                 \
        }                                                               \
        return info;                                                    \
    } while(0)

    CASE(HLSLStructuredBufferType,          StructuredBuffer);
    CASE(HLSLRWStructuredBufferType,        MutableStructuredBuffer);
    CASE(HLSLAppendStructuredBufferType,    MutableStructuredBuffer);
    CASE(HLSLConsumeStructuredBufferType,   MutableStructuredBuffer);

#undef CASE


    // TODO: need a better way to handle this stuff...
#define CASE(TYPE, KIND)                                        \
    else if(type->As<TYPE>()) do {                              \
        return GetSimpleLayoutImpl(                             \
            rules->GetObjectLayout(ShaderParameterKind::KIND),  \
            type, rules, outTypeLayout);                        \
    } while(0)

    CASE(HLSLBufferType,                    SampledBuffer);
    CASE(HLSLRWBufferType,                  MutableSampledBuffer);
    CASE(HLSLByteAddressBufferType,         RawBuffer);
    CASE(HLSLRWByteAddressBufferType,       MutableRawBuffer);

    // This case is mostly to allow users to add new resource types...
    CASE(UntypedBufferResourceType,         RawBuffer);

#undef CASE

    //
    // TODO(tfoley): Need to recognize any UAV types here
    //
    else if(auto basicType = type->As<BasicExpressionType>())
    {
        return GetSimpleLayoutImpl(
            rules->GetScalarLayout(basicType->BaseType),
            type,
            rules,
            outTypeLayout);
    }
    else if(auto vecType = type->As<VectorExpressionType>())
    {
        return GetSimpleLayoutImpl(
            rules->GetVectorLayout(
                GetLayout(vecType->elementType.Ptr(), rules),
                GetIntVal(vecType->elementCount)),
            type,
            rules,
            outTypeLayout);
    }
    else if(auto matType = type->As<MatrixExpressionType>())
    {
        return GetSimpleLayoutImpl(
            rules->GetMatrixLayout(
                GetLayout(matType->elementType.Ptr(), rules),
                GetIntVal(matType->rowCount),
                GetIntVal(matType->rowCount)),
            type,
            rules,
            outTypeLayout);
    }
    else if (auto arrayType = type->As<ArrayExpressionType>())
    {
        RefPtr<TypeLayout> elementTypeLayout;
        auto elementInfo = GetLayoutImpl(
            arrayType->BaseType.Ptr(),
            rules,
            outTypeLayout ? &elementTypeLayout : nullptr);

        // For layout purposes, we treat an unsized array as an array of zero elements.
        //
        // TODO: Longer term we are going to need to be careful to include some indication
        // that a type has logically "infinite" size in some resource kind. In particular
        // this affects how we would allocate space for parameter binding purposes.
        auto elementCount = arrayType->ArrayLength ? GetElementCount(arrayType->ArrayLength) : 0;
        auto arrayUniformInfo = rules->GetArrayLayout(
            elementInfo,
            elementCount).getUniformLayout();

        if (outTypeLayout)
        {
            RefPtr<ArrayTypeLayout> typeLayout = new ArrayTypeLayout();
            *outTypeLayout = typeLayout;

            typeLayout->type = type;
            typeLayout->elementTypeLayout = elementTypeLayout;
            typeLayout->rules = rules;

            typeLayout->uniformAlignment = arrayUniformInfo.alignment;
            typeLayout->uniformStride = arrayUniformInfo.elementStride;

            typeLayout->addResourceUsage(LayoutResourceKind::Uniform, arrayUniformInfo.size);

            // translate element-type resources into array-type resources
            for( auto elementResourceInfo : elementTypeLayout->resourceInfos )
            {
                // The uniform case was already handled above
                if( elementResourceInfo.kind == LayoutResourceKind::Uniform )
                    continue;
            
                typeLayout->addResourceUsage(
                    elementResourceInfo.kind,
                    elementResourceInfo.count * elementCount);
            }
        }
        return arrayUniformInfo;
    }
    else if (auto declRefType = type->As<DeclRefType>())
    {
        auto declRef = declRefType->declRef;

        if (auto structDeclRef = declRef.As<StructDeclRef>())
        {
            RefPtr<StructTypeLayout> typeLayout;
            if (outTypeLayout)
            {
                typeLayout = new StructTypeLayout();
                typeLayout->type = type;
                typeLayout->rules = rules;
                *outTypeLayout = typeLayout;
            }

            UniformLayoutInfo info = rules->BeginStructLayout();

            for (auto field : structDeclRef.GetFields())
            {
                RefPtr<TypeLayout> fieldTypeLayout;
                UniformLayoutInfo fieldInfo = GetLayoutImpl(
                    field.GetType().Ptr(),
                    rules,
                    outTypeLayout ? &fieldTypeLayout : nullptr).getUniformLayout();

                // Note: we don't add any zero-size fields
                // when computing structure layout, just
                // to avoid having a resource type impact
                // the final layout.
                //
                // This means that the code to generate final
                // declarations needs to *also* eliminate zero-size
                // fields to be safe...
                size_t uniformOffset = info.size;
                if(fieldInfo.size != 0)
                {
                    uniformOffset = rules->AddStructField(&info, fieldInfo);
                }

                if (outTypeLayout)
                {
                    // If we are computing a complete layout,
                    // then we need to create variable layouts
                    // for each field of the structure.
                    RefPtr<VarLayout> fieldLayout = new VarLayout();
                    fieldLayout->varDecl = field;
                    fieldLayout->typeLayout = fieldTypeLayout;
                    typeLayout->fields.Add(fieldLayout);
                    typeLayout->mapVarToLayout.Add(field.GetDecl(), fieldLayout);

                    // Set up uniform offset information, if there is any uniform data in the field
                    if( fieldTypeLayout->FindResourceInfo(LayoutResourceKind::Uniform) )
                    {
                        fieldLayout->AddResourceInfo(LayoutResourceKind::Uniform)->index = uniformOffset;
                    }

                    // Add offset information for any other resource kinds
                    for( auto fieldTypeResourceInfo : fieldTypeLayout->resourceInfos )
                    {
                        // Uniforms were dealt with above
                        if(fieldTypeResourceInfo.kind == LayoutResourceKind::Uniform)
                            continue;

                        // We should not have already processed this resource type
                        assert(!fieldLayout->FindResourceInfo(fieldTypeResourceInfo.kind));

                        // The field will need offset information for this kind
                        auto fieldResourceInfo = fieldLayout->AddResourceInfo(fieldTypeResourceInfo.kind);

                        // Check how many slots of the given kind have already been added to the type
                        auto structTypeResourceInfo = typeLayout->findOrAddResourceInfo(fieldTypeResourceInfo.kind);
                        fieldResourceInfo->index = structTypeResourceInfo->count;
                        structTypeResourceInfo->count += fieldTypeResourceInfo.count;
                    }
                }
            }

            rules->EndStructLayout(&info);
            if (outTypeLayout)
            {
                typeLayout->uniformAlignment = info.alignment;
                typeLayout->addResourceUsage(LayoutResourceKind::Uniform, info.size);
            }

            return info;
        }
    }

    // catch-all case in case nothing matched
    assert(!"unimplemented");
    SimpleLayoutInfo info;
    return GetSimpleLayoutImpl(
        info,
        type,
        rules,
        outTypeLayout);
}

SimpleLayoutInfo GetLayout(ExpressionType* inType, LayoutRulesImpl* rules)
{
    return GetLayoutImpl(inType, rules, nullptr);
}

RefPtr<TypeLayout> CreateTypeLayout(ExpressionType* type, LayoutRulesImpl* rules)
{
    RefPtr<TypeLayout> typeLayout;
    GetLayoutImpl(type, rules, &typeLayout);
    return typeLayout;
}

SimpleLayoutInfo GetLayout(ExpressionType* type, LayoutRule rule)
{
    LayoutRulesImpl* rulesImpl = GetLayoutRulesImpl(rule);
    return GetLayout(type, rulesImpl);
}

}}
