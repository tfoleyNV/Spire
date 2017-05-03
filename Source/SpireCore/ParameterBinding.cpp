// ParameterBinding.cpp
#include "ParameterBinding.h"

#include "ShaderCompiler.h"
#include "TypeLayout.h"

#include "../../Spire.h"

namespace Spire {
namespace Compiler {

// Information on ranges of registers already claimed/used
struct UsedRange
{
    int begin;
    int end;
};
bool operator<(UsedRange left, UsedRange right)
{
    if (left.begin != right.begin)
        return left.begin < right.begin;
    if (left.end != right.end)
        return left.end < right.end;
    return false;
}

struct UsedRanges
{
    List<UsedRange> ranges;

    // Add a range to the set, either by extending
    // an existing range, or by adding a new one...
    void Add(UsedRange const& range)
    {
        for (auto& rr : ranges)
        {
            if (rr.begin == range.end)
            {
                rr.begin = range.begin;
                return;
            }
            else if (rr.end == range.begin)
            {
                rr.end = range.end;
                return;
            }
        }
        ranges.Add(range);
        ranges.Sort();
    }

    void Add(int begin, int end)
    {
        UsedRange range;
        range.begin = begin;
        range.end = end;
        Add(range);
    }


    // Try to find space for `count` entries
    int Allocate(int count)
    {
        int begin = 0;

        int rangeCount = ranges.Count();
        for (int rr = 0; rr < rangeCount; ++rr)
        {
            // try to fit in before this range...

            int end = ranges[rr].begin;

            // If there is enough space...
            if (end >= begin + count)
            {
                // ... then claim it and be done
                Add(begin, begin + count);
                return begin;
            }

            // ... otherwise, we need to look at the
            // space between this range and the next
            begin = ranges[rr].end;
        }

        // We've run out of ranges to check, so we
        // can safely go after the last one!
        Add(begin, begin + count);
        return begin;
    }
};

struct ParameterBindingInfo
{
    size_t              space;
    size_t              index;
    size_t              count;
};

// Information on a single parameter
struct ParameterInfo : RefObject
{
    // Layout info for the concrete variables that will make up this parameter
    List<RefPtr<VarLayout>> varLayouts;

    ParameterBindingInfo    bindingInfo[(int)LayoutResourceKind::Count];

    // The next parameter that has the same name...
    ParameterInfo* nextOfSameName;

    ParameterInfo()
    {
        // Make sure we aren't claiming any resources yet
        for( int ii = 0; ii < (int) LayoutResourceKind::Count; ++ii )
        {
            bindingInfo[ii].count = 0;
        }
    }
};

struct SharedParameterBindingContext
{
    LayoutRulesImpl* defaultLayoutRules;

    // All shader parameters we've discovered so far, and started to lay out...
    List<RefPtr<ParameterInfo>> parameters;

    // A dictionary to accellerate looking up parameters by name
    Dictionary<String, ParameterInfo*> mapNameToParameterInfo;
};

struct ParameterBindingContext
{
    SharedParameterBindingContext* shared;

    // TODO: information for implicit constant buffer!

    // The layout rules to use while computing usage...
    LayoutRulesImpl* layoutRules;

    UsedRanges usedResourceRanges[(int)LayoutResourceKind::Count];
};

// Check whether a global variable declaration represents
// a shader parameter, or is there for some other purpose.
static bool IsGlobalVariableAShaderParameter(
    ParameterBindingContext*    /*context*/,
    RefPtr<VarDeclBase>         varDecl)
{
    // We need to determine if this variable represents a shader
    // parameter, or just an ordinary global variable...
    if(varDecl->HasModifier<HLSLStaticModifier>())
        return false;

    // TODO(tfoley): there may be other cases that we need to handle here

    // TODO(tfoley): GLSL rules are almost completely flipped from HLSL.
    // A GLSL global is "just a global" unless it is specifically decorated
    // as a shader parameter...

    return true;
}

struct LayoutSemanticInfo
{
    LayoutResourceKind  kind; // the register kind
    int                 space;
    int                 index;

    // TODO: need to deal with component-granularity binding...
};

LayoutSemanticInfo ExtractLayoutSemanticInfo(
    ParameterBindingContext*    /*context*/,
    HLSLLayoutSemantic*         semantic)
{
    LayoutSemanticInfo info;
    info.space = 0;
    info.index = 0;
    info.kind = LayoutResourceKind::Invalid;

    auto registerName = semantic->registerName.Content;
    if (registerName.Length() == 0)
        return info;

    LayoutResourceKind kind = LayoutResourceKind::Invalid;
    switch (registerName[0])
    {
    case 'b':
        kind = LayoutResourceKind::ConstantBuffer;
        break;

    case 't':
        kind = LayoutResourceKind::ShaderResource;
        break;

    case 'u':
        kind = LayoutResourceKind::UnorderedAccess;
        break;

    case 's':
        kind = LayoutResourceKind::SamplerState;
        break;

    default:
        // TODO: issue an error here!
        return info;
    }

    // TODO: need to parse and handle `space` binding
    int space = 0;

    int index = 0;
    for (int ii = 1; ii < registerName.Length(); ++ii)
    {
        int c = registerName[ii];
        if (c >= '0' && c <= '9')
        {
            index = index * 10 + (c - '0');
        }
        else
        {
            // TODO: issue an error here!
            return info;
        }
    }

    // TODO: handle component mask part of things...

    info.kind = kind;
    info.index = index;
    info.space = space;
    return info;
}

static bool doesParameterMatch(
    ParameterBindingContext*    context,
    RefPtr<VarLayout>           varLayout,
    ParameterInfo*              parameterInfo)
{
    // TODO: need to implement this eventually
    return true;
}

// Collect a single declaration into our set of parameters
static void collectParameter(
    ParameterBindingContext*    context,
    RefPtr<VarDeclBase>         varDecl)
{
    // Find out the layout for the type...
    RefPtr<TypeLayout> typeLayout = CreateTypeLayout(
        varDecl->Type.Ptr(),
        context->layoutRules);

    // Now create a variable layout that we can use
    RefPtr<VarLayout> varLayout = new VarLayout();
    varLayout->typeLayout = typeLayout;
    varLayout->varDecl = DeclRef(varDecl.Ptr(), nullptr).As<VarDeclBaseRef>();

    // This declaration may represent the same logical parameter
    // as a declaration that came from a different translation unit.
    // If that is the case, we want to re-use the same `VarLayout`
    // across both parameters.
    //
    // First we look for an existing entry matching the name
    // of this parameter:
    auto parameterName = varDecl->Name.Content;
    ParameterInfo* parameterInfo = nullptr;
    if( context->shared->mapNameToParameterInfo.TryGetValue(parameterName, parameterInfo) )
    {
        // If the parameters have the same name, but don't "match" according to some reasonable rules,
        // then we need to bail out.
        if( !doesParameterMatch(context, varLayout, parameterInfo) )
        {
            parameterInfo = nullptr;
        }
    }

    // If we didn't find a matching parameter, then we need to create one here
    if( !parameterInfo )
    {
        parameterInfo = new ParameterInfo();
        context->shared->parameters.Add(parameterInfo);
        context->shared->mapNameToParameterInfo.Add(parameterName, parameterInfo);
    }
    else
    {
        varLayout->flags |= VarLayoutFlag::IsRedeclaration;
    }

    // Add this variable declaration to the list of declarations for the parameter
    parameterInfo->varLayouts.Add(varLayout);
}

// Given a single parameter, collect whatever information we have on
// how it has been explicitly bound, which may come from multiple declarations
void generateParameterBindings(
    ParameterBindingContext*    context,
    RefPtr<ParameterInfo>       parameterInfo)
{
    // There must be at least one declaration for the parameter.
    assert(parameterInfo->varLayouts.Count() != 0);

    // Iterate over all declarations looking for explicit binding information.
    for( auto& varLayout : parameterInfo->varLayouts )
    {
        auto typeLayout = varLayout->typeLayout;
        auto varDecl = varLayout->varDecl;

        // If the declaration has explicit binding modifiers, then
        // here is where we want to extract and apply them...

        // Look for HLSL `register` or `packoffset` semantics.
        for (auto semantic : varDecl.GetDecl()->GetModifiersOfType<HLSLLayoutSemantic>())
        {
            // Need to extract the information encoded in the semantic
            LayoutSemanticInfo semanticInfo = ExtractLayoutSemanticInfo(context, semantic);
            auto kind = semanticInfo.kind;
            if (kind == LayoutResourceKind::Invalid)
                continue;

            // TODO: need to special-case when this is a `c` register binding...

            // Find the appropriate resource-binding information
            // inside the type, to see if we even use any resources
            // of the given kind.

            auto typeRes = typeLayout->FindResourceInfo(kind);
            int count = 0;
            if (typeRes)
            {
                count = typeRes->count;
            }
            else
            {
                // TODO: warning here!
            }

            auto& bindingInfo = parameterInfo->bindingInfo[(int)kind];
            if( bindingInfo.count != 0 )
            {
                // We already have a binding here, so we want to
                // confirm that it matches the new one that is
                // incoming...
                if( bindingInfo.count != count
                    || bindingInfo.index != semanticInfo.index
                    || bindingInfo.space != semanticInfo.space )
                {
                    // TODO: diagnose!
                }

                // TODO(tfoley): `register` semantics can technically be
                // profile-specific (not sure if anybody uses that)...
            }
            else
            {
                bindingInfo.count = count;
                bindingInfo.index = semanticInfo.index;
                bindingInfo.space = semanticInfo.space;

                // If things are bound in `space0` (the default), then we need
                // to lay claim to the register range used, so that automatic
                // assignment doesn't go and use the same registers.
                if (semanticInfo.space == 0)
                {
                    context->usedResourceRanges[(int)semanticInfo.kind].Add(
                        semanticInfo.index,
                        semanticInfo.index + count);
                }
            }
        }
    }
}

// Generate the binding information for a shader parameter.
static void completeBindingsForParameter(
    ParameterBindingContext*    context,
    RefPtr<ParameterInfo>       parameterInfo)
{
    // For any resource kind used by the parameter
    // we need to update its layout information
    // to include a binding for that resource kind.
    //
    // We will use the first declaration of the parameter as
    // a stand-in for all the declarations, so it is important
    // that earlier code has validated that the declarations
    // "match".

    assert(parameterInfo->varLayouts.Count() != 0);
    auto firstVarLayout = parameterInfo->varLayouts.First();
    auto firstTypeLayout = firstVarLayout->typeLayout;

    for (auto typeRes = &firstTypeLayout->resources;
        typeRes && IsResourceKind(typeRes->kind);
        typeRes = typeRes->next.Ptr())
    {
        // Did we already apply some explicit binding information
        // for this resource kind?
        auto kind = typeRes->kind;
        auto& bindingInfo = parameterInfo->bindingInfo[(int)kind];
        if( bindingInfo.count != 0 )
        {
            // If things have already been bound, our work is done.
            continue;
        }

        auto count = typeRes->count;
        bindingInfo.count = count;
        bindingInfo.index = context->usedResourceRanges[(int)kind].Allocate(count);

        // For now we only auto-generate bindings in space zero
        bindingInfo.space = 0;
    }

    // At this point we should have explicit binding locations chosen for
    // all the relevant resource kinds, so we can apply these to the
    // declarations:

    for(auto& varLayout : parameterInfo->varLayouts)
    {
        for(auto k = 0; k < (int)LayoutResourceKind::Count; ++k)
        {
            auto kind = LayoutResourceKind(k);
            auto& bindingInfo = parameterInfo->bindingInfo[k];

            // skip resources we aren't consuming
            if(bindingInfo.count == 0)
                continue;

            // Add a record to the variable layout
            auto varRes = varLayout->AddResourceInfo(kind);
            varRes->space = (int) bindingInfo.space;
            varRes->index = (int) bindingInfo.index;
        }
    }
}

static void collectParameters(
    ParameterBindingContext*    context,
    ProgramSyntaxNode*          program)
{
    // First enumerate parameters at global scope
    for( auto decl : program->Members )
    {
        // A shader parameter is always a variable,
        // so skip declarations that aren't variables.
        auto varDecl = decl.As<VarDeclBase>();
        if (!varDecl)
            continue;

        // Skip globals that don't look like parameters
        if (!IsGlobalVariableAShaderParameter(context, varDecl))
            continue;

        collectParameter(context, varDecl);
    }

    // Next, we need to enumerate the parameters of
    // each entry point (which requires knowing what the
    // entry points *are*)

    // TODO(tfoley): Entry point functions should be identified
    // by looking for a generated modifier that is attached
    // to global-scope function declarations.
}

static void collectParameters(
    ParameterBindingContext*        context,
    CollectionOfTranslationUnits*   program)
{
    for( auto& translationUnit : program->translationUnits )
    {
        collectParameters(context, translationUnit.SyntaxNode.Ptr());
    }
}

void GenerateParameterBindings(
    CollectionOfTranslationUnits*   program)
{
    // Create a context to hold shared state during the process
    // of generating parameter bindings
    SharedParameterBindingContext sharedContext;
    sharedContext.defaultLayoutRules = GetLayoutRulesImpl(LayoutRule::HLSLConstantBuffer);

    // Create a sub-context to collect parameters that get
    // declared into the global scope
    ParameterBindingContext context;
    context.shared = &sharedContext;
    context.layoutRules = sharedContext.defaultLayoutRules;

    // Walk through global scope to discover all the parameters
    collectParameters(&context, program);

    // Now walk through the parameters to generate initial binding information
    for( auto& parameter : sharedContext.parameters )
    {
        generateParameterBindings(&context, parameter);
    }

    // Now walk through again to actually give everything
    // ranges of registers...
    for( auto& parameter : sharedContext.parameters )
    {
        completeBindingsForParameter(&context, parameter);
    }

    // We now have a bunch of layout information, which we should
    // record into a suitable object that represents the program
    RefPtr<ProgramLayout> programLayout = new ProgramLayout;
    programLayout->rules = context.layoutRules;

    for( auto& parameterInfo : sharedContext.parameters )
    {
        assert(parameterInfo->varLayouts.Count() != 0);
        auto firstVarLayout = parameterInfo->varLayouts.First();

        programLayout->fields999.Add(firstVarLayout);

        for( auto& varLayout : parameterInfo->varLayouts )
        {
            programLayout->mapVarToLayout.Add(varLayout->varDecl.GetDecl(), varLayout);
        }
    }

    program->layout = programLayout;
}

}}
