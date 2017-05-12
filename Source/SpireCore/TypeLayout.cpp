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

struct DefaultLayoutRulesImpl : LayoutRulesImpl
{
    // Get size and alignment for a single value of base type.
    LayoutInfo GetScalarLayout(BaseType baseType) override
    {
        switch (baseType)
        {
        case BaseType::Int:
        case BaseType::UInt:
        case BaseType::Float:
        case BaseType::Bool:
            return{ 4, 4 };

        default:
            assert(!"unimplemented");
            return{ 0, 1 };
        }
    }

    LayoutInfo GetScalarLayout(ILBaseType baseType) override
    {
        switch (baseType)
        {
        case ILBaseType::Int:
        case ILBaseType::UInt:
        case ILBaseType::Float:
            return{ 4, 4 };
        default:
            assert(!"unimplemented");
            return{ 0, 1 };
        }
    }

    virtual LayoutInfo GetScalarLayout(spire::TypeReflection::ScalarType scalarType)
    {
        switch( scalarType )
        {
        case spire::TypeReflection::ScalarType::Void:       return {0,1};
        case spire::TypeReflection::ScalarType::None:       return {0,1};

        // TODO(tfoley): At some point we don't want to lay out `bool` as 4 bytes by default...
        case spire::TypeReflection::ScalarType::Bool:       return {4,4};
        case spire::TypeReflection::ScalarType::Int32:      return {4,4};
        case spire::TypeReflection::ScalarType::UInt32:     return {4,4};
        case spire::TypeReflection::ScalarType::Int64:      return {8,8};
        case spire::TypeReflection::ScalarType::UInt64:     return {8,8};

        // TODO(tfoley): What actually happens if you use `half` in a constant buffer?
        case spire::TypeReflection::ScalarType::Float16:    return {2,2};
        case spire::TypeReflection::ScalarType::Float32:    return {4,4};
        case spire::TypeReflection::ScalarType::Float64:    return {8,8};

        default:
            assert(!"unimplemented");
            return{ 0, 1 };
        }
    }

    virtual ObjectLayoutInfo GetObjectLayout(LayoutResourceKind kind) override
    {
        return ObjectLayoutInfo(kind);
    }

    ArrayLayoutInfo GetArrayLayout( LayoutInfo elementInfo, size_t elementCount) override
    {
        size_t stride = elementInfo.size;

        ArrayLayoutInfo arrayInfo;
        arrayInfo.size = stride * elementCount;
        arrayInfo.alignment = elementInfo.alignment;
        arrayInfo.elementStride = stride;
        return arrayInfo;
    }

    LayoutInfo GetVectorLayout(LayoutInfo elementInfo, size_t elementCount) override
    {
        LayoutInfo vectorInfo;
        vectorInfo.size = elementInfo.size * elementCount;
        vectorInfo.alignment = elementInfo.alignment;
        return vectorInfo;
    }

    LayoutInfo GetMatrixLayout(LayoutInfo elementInfo, size_t rowCount, size_t columnCount) override
    {
        return GetArrayLayout(
            GetVectorLayout(elementInfo, columnCount),
            rowCount);
    }

    LayoutInfo BeginStructLayout() override
    {
        LayoutInfo structInfo;
        structInfo.size = 0;
        structInfo.alignment = 1;
        return structInfo;
    }

    size_t AddStructField(LayoutInfo* ioStructInfo, LayoutInfo fieldInfo) override
    {
        ioStructInfo->alignment = std::max(ioStructInfo->alignment, fieldInfo.alignment);
        ioStructInfo->size = RoundToAlignment(ioStructInfo->size, fieldInfo.alignment);
        size_t fieldOffset = ioStructInfo->size;
        ioStructInfo->size += fieldInfo.size;
        return fieldOffset;
    }


    void EndStructLayout(LayoutInfo* ioStructInfo) override
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
    ArrayLayoutInfo GetArrayLayout(LayoutInfo elementInfo, size_t elementCount) override
    {
        if (elementInfo.alignment < 16)
            elementInfo.alignment = 16;
        elementInfo.size = RoundToAlignment(elementInfo.size, elementInfo.alignment);

        return DefaultLayoutRulesImpl::GetArrayLayout(elementInfo, elementCount);
    }

    // The `std140` rules require that a `struct` type be
    // alinged to at least 16.
    //
    // HLSL agrees.
    LayoutInfo BeginStructLayout() override
    {
        LayoutInfo structInfo;
        structInfo.size = 0;
        structInfo.alignment = 16;
        return structInfo;
    }
};


struct Std140LayoutRulesImpl : DefaultConstantBufferLayoutRulesImpl
{
    // The `std140` rules require vectors to be aligned to the next power of two
    // up from their size (so a `float2` is 8-byte aligned, and a `float3` is
    // 16-byte aligned).
    LayoutInfo GetVectorLayout(LayoutInfo elementInfo, size_t elementCount) override
    {
        LayoutInfo vectorInfo;
        vectorInfo.size = elementInfo.size * elementCount;
        vectorInfo.alignment = RoundUpToPowerOfTwo(elementInfo.size * elementInfo.alignment);
        return vectorInfo;
    }
};

struct HLSLConstantBufferLayoutRulesImpl : DefaultConstantBufferLayoutRulesImpl
{
    // Can't let a `struct` field straddle a register (16-byte) boundary
    size_t AddStructField(LayoutInfo* ioStructInfo, LayoutInfo fieldInfo) override
    {
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

struct Std430LayoutRulesImpl : DefaultLayoutRulesImpl
{
};

struct PackedLayoutRulesImpl : DefaultLayoutRulesImpl
{
};

Std140LayoutRulesImpl kStd140LayoutRulesImpl;
Std430LayoutRulesImpl kStd430LayoutRulesImpl;
PackedLayoutRulesImpl kPackedLayoutRulesImpl;
HLSLConstantBufferLayoutRulesImpl kHLSLConstantBufferLayoutRulesImpl;
HLSLStructuredBufferLayoutRulesImpl kHLSLStructuredBufferLayoutRulesImpl;

LayoutRulesImpl* GetLayoutRulesImpl(LayoutRule rule)
{
    switch (rule)
    {
    case LayoutRule::Std140:                return &kStd140LayoutRulesImpl;
    case LayoutRule::Std430:                return &kStd430LayoutRulesImpl;
    case LayoutRule::Packed:                return &kPackedLayoutRulesImpl;
    case LayoutRule::HLSLConstantBuffer:    return &kHLSLConstantBufferLayoutRulesImpl;
    case LayoutRule::HLSLStructuredBuffer:  return &kHLSLStructuredBufferLayoutRulesImpl;
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
    case LayoutResourceKind::Invalid:
    case LayoutResourceKind::Uniform:
        return false;

    default:
        return true;
    }

}

LayoutInfo GetSimpleLayoutImpl(
    ObjectLayoutInfo        info,
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
        typeLayout->uniforms = info;
        if(IsResourceKind(info.kind))
        {
            typeLayout->resources.kind = info.kind;
            typeLayout->resources.count = (int) info.size;
        }
    }

    return info;
}

LayoutInfo GetLayoutImpl(
    ExpressionType*     type,
    LayoutRulesImpl*    rules,
    RefPtr<TypeLayout>* outTypeLayout)
{
    if (auto constantBufferType = type->As<ConstantBufferType>())
    {
        // If the user is just interested in uniform layout info,
        // then this is easy: a `ConstantBuffer<T>` is really no
        // different from a `Texture2D<U>` in terms of how it
        // should be handled as a member of a container.
        //
        auto info = rules->GetObjectLayout(LayoutResourceKind::ConstantBuffer);

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
            auto typeLayout = new ConstantBufferTypeLayout();
            *outTypeLayout = typeLayout;

            typeLayout->type = type;
            typeLayout->rules = rules;

            // TODO(tfoley): need to compute the layout for the constant
            // buffer's contents...
            auto constantBufferLayoutRules = GetLayoutRulesImpl(
                LayoutRule::HLSLConstantBuffer);

            // Create and save type layout for the buffer contents.
            auto elementTypeLayout = CreateTypeLayout(
                constantBufferType->elementType.Ptr(),
                constantBufferLayoutRules);
            typeLayout->elementTypeLayout = elementTypeLayout;

            // The layout of the constant buffer if it gets stored
            // in another constant buffer is just what we computed
            // originally (which should be a single binding "slot"
            // and hence no uniform data).
            // 
            typeLayout->uniforms = info;
            assert(typeLayout->uniforms.size == 0);
            assert(typeLayout->uniforms.alignment == 1);

            // TODO(tfoley): There is a subtle question here of whether
            // a constant buffer declaration that then contains zero
            // bytes of uniform data should actually allocate a CB
            // binding slot. For now I'm going to try to ignore it,
            // but handling this robustly could let other code
            // simply handle the "global scope" as a giant outer
            // CB declaration...

            // The CB will always allocate at least one resource
            // binding slot for the CB itself.
            typeLayout->resources.count = 1;
            typeLayout->resources.kind = LayoutResourceKind::ConstantBuffer;
            typeLayout->resources.next = 0;

            // Now, if the element type itself had any resources, then
            // we need to make these part of the layout for our CB
            if( elementTypeLayout->resources.kind != LayoutResourceKind::Invalid )
            {
                RefPtr<TypeLayout::ResourceInfo>* link = &typeLayout->resources.next;
                for( auto rr = &elementTypeLayout->resources; rr; rr = rr->next.Ptr() )
                {
                    assert(rr->kind != LayoutResourceKind::Invalid);

                    // If we have nested constant buffers, then just increase the number of slots reserved
                    if( rr->kind == LayoutResourceKind::ConstantBuffer )
                    {
                        typeLayout->resources.count += rr->count;
                    }
                    else
                    {
                        // Otherwise, we are appending a new slot type to the array (need to make a copy here...)
                        RefPtr<TypeLayout::ResourceInfo> info = new TypeLayout::ResourceInfo();
                        info->kind = rr->kind;
                        info->count = rr->count;
                        info->next = nullptr;

                        *link = info;
                        link = &info->next;
                    }
                }
            }
        }

        return info;
    }
    else if (auto samplerStateType = type->As<SamplerStateType>())
    {
        return GetSimpleLayoutImpl(
            rules->GetObjectLayout(LayoutResourceKind::SamplerState),
            type,
            rules,
            outTypeLayout);
    }
    else if (auto textureType = type->As<TextureType>())
    {
        return GetSimpleLayoutImpl(
            rules->GetObjectLayout(LayoutResourceKind::ShaderResource),
            type,
            rules,
            outTypeLayout);
    }

    // TODO: need a better way to handle this stuff...
#define CASE(TYPE, KIND)                                        \
    else if(type->As<TYPE>()) do {                              \
        return GetSimpleLayoutImpl(                             \
            rules->GetObjectLayout(LayoutResourceKind::KIND),   \
            type, rules, outTypeLayout);                        \
    } while(0)

    CASE(HLSLBufferType,                    ShaderResource);
    CASE(HLSLRWBufferType,                  UnorderedAccess);
    CASE(HLSLStructuredBufferType,          ShaderResource);
    CASE(HLSLRWStructuredBufferType,        UnorderedAccess);
    CASE(HLSLByteAddressBufferType,         ShaderResource);
    CASE(HLSLRWByteAddressBufferType,       UnorderedAccess);
    CASE(HLSLAppendStructuredBufferType,    UnorderedAccess);
    CASE(HLSLConsumeStructuredBufferType,   UnorderedAccess);

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

        auto elementCount = GetElementCount(arrayType->ArrayLength);
        auto arrayInfo = rules->GetArrayLayout(
            elementInfo,
            elementCount);

        if (outTypeLayout)
        {
            RefPtr<ArrayTypeLayout> typeLayout = new ArrayTypeLayout();
            *outTypeLayout = typeLayout;

            typeLayout->type = type;
            typeLayout->elementTypeLayout = elementTypeLayout;
            typeLayout->rules = rules;
            typeLayout->uniforms = arrayInfo;
            typeLayout->uniformStride = arrayInfo.elementStride;

            // translate element-type resources into array-type resources
            if(IsResourceKind(elementTypeLayout->resources.kind))
            {
                // The first resource info is stored directly
                typeLayout->resources.kind = elementTypeLayout->resources.kind;
                typeLayout->resources.count = elementCount
                    * elementTypeLayout->resources.count;

                // the rest are in a linked list that we need to copy
                auto link = &typeLayout->resources.next;
                for (auto rr = elementTypeLayout->resources.next; rr; rr = rr->next)
                {
                    RefPtr<ArrayTypeLayout::ResourceInfo> res = new ArrayTypeLayout::ResourceInfo;
                    res->kind = rr->kind;
                    res->count = elementCount * rr->count;
                    
                    *link = res;
                    link = &res->next;
                }
            }
        }
        return arrayInfo;
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

            LayoutInfo info = rules->BeginStructLayout();

            for (auto field : structDeclRef.GetFields())
            {
                RefPtr<TypeLayout> fieldTypeLayout;
                LayoutInfo fieldInfo = GetLayoutImpl(
                    field.GetType().Ptr(),
                    rules,
                    outTypeLayout ? &fieldTypeLayout : nullptr);

                // Note: we don't add any zero-size fields
                // when computing structure layout, just
                // to avoid having a resource type impact
                // the final layout.
                //
                // This means that the code to generate final
                // declarations needs to *also* eliminate zero-size
                // fields to be safe...
                size_t uniformOffset = info.size;
                if (fieldInfo.size != 0)
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
                    typeLayout->fields999.Add(fieldLayout);
                    typeLayout->mapVarToLayout.Add(field.GetDecl(), fieldLayout);

                    // Uniform-related information for the field
                    // is simple to set up.
                    fieldLayout->uniformOffset = uniformOffset;

                    // Reource-related information takes more
                    // work, because we need to handle the possibility
                    // of zero or more resource types...
                    auto fieldTypeRes = &fieldTypeLayout->resources;
                    if (IsResourceKind(fieldTypeRes->kind))
                    {
                        auto fieldRes = &fieldLayout->resources;
                        for (;;)
                        {
                            fieldRes->kind = fieldTypeRes->kind;
                            assert(fieldRes->kind != LayoutResourceKind::Invalid);
                            fieldRes->space = 0;

                            // To compute the right index, we need
                            // to look for any existing information
                            // in the struct type that will match
                            fieldRes->index = 0;

                            auto structRes = &typeLayout->resources;
                            TypeLayout::ResourceInfo* foundStructRes = nullptr;
                            while (structRes)
                            {
                                if (structRes->kind == fieldRes->kind)
                                {
                                    // we found one!
                                    foundStructRes = structRes;
                                    break;
                                }
                                structRes = structRes->next.Ptr();
                            }
                            if (foundStructRes)
                            {
                                fieldRes->index = foundStructRes->count;
                                foundStructRes->count += fieldTypeRes->count;
                            }
                            else
                            {
                                // This is the first field of its kind...
                                fieldRes->index = 0;

                                structRes = &typeLayout->resources;
                                if (!IsResourceKind(structRes->kind))
                                {
                                    // first resource field in the struct
                                    foundStructRes = structRes;
                                }
                                else
                                {
                                    // There have been other resources,
                                    // so we place ours after those...
                                    while (structRes->next)
                                        structRes = structRes->next.Ptr();

                                    foundStructRes = new TypeLayout::ResourceInfo();
                                    structRes->next = foundStructRes;
                                }
                                foundStructRes->kind = fieldTypeRes->kind;
                                foundStructRes->count = fieldTypeRes->count;
                            }

                            // Okay, now we move to the next resource kind
                            fieldTypeRes = fieldTypeRes->next.Ptr();
                            if (!fieldTypeRes)
                                break;

                            // Create a node to hold offset information in
                            // the field layout...
                            auto nextFieldRes = new VarLayout::ResourceInfo();
                            fieldRes->next = nextFieldRes;
                            fieldRes = nextFieldRes;
                        }
                    }
                }
            }

            rules->EndStructLayout(&info);
            if (outTypeLayout)
            {
                typeLayout->uniforms = info;
            }

            return info;
        }
    }

    // catch-all case in case nothing matched
    assert(!"unimplemented");
    LayoutInfo info = { 0, 1 };
    return GetSimpleLayoutImpl(
        info,
        type,
        rules,
        outTypeLayout);
}

LayoutInfo GetLayout(ExpressionType* inType, LayoutRulesImpl* rules)
{
    return GetLayoutImpl(inType, rules, nullptr);
}

RefPtr<TypeLayout> CreateTypeLayout(ExpressionType* type, LayoutRulesImpl* rules)
{
    RefPtr<TypeLayout> typeLayout;
    GetLayoutImpl(type, rules, &typeLayout);
    return typeLayout;
}

LayoutInfo GetLayout(ILType* type, LayoutRulesImpl* rules)
{
    if (auto basicType = dynamic_cast<ILBasicType*>(type))
    {
        return rules->GetScalarLayout(basicType->Type);
    }
    else if (auto vectorType = dynamic_cast<ILVectorType*>(type))
    {
        return rules->GetVectorLayout(rules->GetScalarLayout(vectorType->BaseType), vectorType->Size);
    }
    else if (auto matrixType = dynamic_cast<ILMatrixType*>(type))
    {
        return rules->GetVectorLayout(rules->GetScalarLayout(matrixType->BaseType), matrixType->Size[1]);
    }
    else if (auto textureType = dynamic_cast<TextureType*>(type))
    {
        return{ 8, 8 };
    }
    else if (auto ptrLike = dynamic_cast<ILPointerLikeType*>(type))
    {
        return{ 8, 8 };
    }
    else if (auto arrayLike = dynamic_cast<ILArrayLikeType*>(type))
    {
        return{ 8, 8 };
    }
    else if (auto arrayType = dynamic_cast<ILArrayType*>(type))
    {
        return rules->GetArrayLayout(
            GetLayout(arrayType->BaseType.Ptr(), rules),
            arrayType->ArrayLength);
    }
    else if (auto structType = dynamic_cast<ILStructType*>(type))
    {
        LayoutInfo info = rules->BeginStructLayout();

        for (auto field : structType->Members)
        {
            rules->AddStructField(&info,
                GetLayout(field.Type.Ptr(), rules));
        }

        rules->EndStructLayout(&info);
        return info;
    }
    else if (auto recordType = dynamic_cast<ILRecordType*>(type))
    {
        // TODO: this need to be implemented
        LayoutInfo info = { 0, 1 };
        return info;
    }
    else if (auto genericType = dynamic_cast<ILGenericType*>(type))
    {
        return GetLayout(genericType->BaseType.Ptr(), rules);
    }
    else
    {
        assert(!"unimplemented");
        return{ 0, 1 };
    }
}

LayoutInfo GetLayout(ExpressionType* type, LayoutRule rule)
{
    LayoutRulesImpl* rulesImpl = GetLayoutRulesImpl(rule);
    return GetLayout(type, rulesImpl);
}

LayoutInfo GetLayout(ILType* type, LayoutRule rule)
{
    LayoutRulesImpl* rulesImpl = GetLayoutRulesImpl(rule);
    return GetLayout(type, rulesImpl);
}

}}
