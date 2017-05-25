// lookup.cpp
#include "lookup.h"

namespace Spire {
namespace Compiler {

//

// Helper for constructing breadcrumb trails during lookup, without unnecessary heap allocaiton
struct BreadcrumbInfo
{
    LookupResultItem::Breadcrumb::Kind kind;
    DeclRef declRef;
    BreadcrumbInfo* prev = nullptr;
};

void DoLocalLookupImpl(
    String const&		    name,
    ContainerDeclRef	    containerDeclRef,
    LookupRequest const&    request,
    LookupResult&		    result,
    BreadcrumbInfo*		    inBreadcrumbs);

//

void buildMemberDictionary(ContainerDecl* decl)
{
    // Don't rebuild if already built
    if (decl->memberDictionaryIsValid)
        return;

    decl->memberDictionary.Clear();
    decl->transparentMembers.Clear();

    for (auto m : decl->Members)
    {
        auto name = m->Name.Content;

        // Add any transparent members to a separate list for lookup
        if (m->HasModifier<TransparentModifier>())
        {
            TransparentMemberInfo info;
            info.decl = m.Ptr();
            decl->transparentMembers.Add(info);
        }

        // Ignore members with an empty name
        if (name.Length() == 0)
            continue;

        m->nextInContainerWithSameName = nullptr;

        Decl* next = nullptr;
        if (decl->memberDictionary.TryGetValue(name, next))
            m->nextInContainerWithSameName = next;

        decl->memberDictionary[name] = m.Ptr();

    }
    decl->memberDictionaryIsValid = true;
}


bool DeclPassesLookupMask(Decl* decl, LookupMask mask)
{
    // type declarations
    if(auto aggTypeDecl = dynamic_cast<AggTypeDecl*>(decl))
    {
        return int(mask) & int(LookupMask::Type);
    }
    else if(auto simpleTypeDecl = dynamic_cast<SimpleTypeDecl*>(decl))
    {
        return int(mask) & int(LookupMask::Type);
    }
    // function declarations
    else if(auto funcDecl = dynamic_cast<FunctionDeclBase*>(decl))
    {
        return (int(mask) & int(LookupMask::Function)) != 0;
    }

    // default behavior is to assume a value declaration
    // (no overloading allowed)

    return (int(mask) & int(LookupMask::Value)) != 0;
}

void AddToLookupResult(
    LookupResult&		result,
    LookupResultItem	item)
{
    if (!result.isValid())
    {
        // If we hadn't found a hit before, we have one now
        result.item = item;
    }
    else if (!result.isOverloaded())
    {
        // We are about to make this overloaded
        result.items.Add(result.item);
        result.items.Add(item);
    }
    else
    {
        // The result was already overloaded, so we pile on
        result.items.Add(item);
    }
}

LookupResult refineLookup(LookupResult const& inResult, LookupMask mask)
{
    if (!inResult.isValid()) return inResult;
    if (!inResult.isOverloaded()) return inResult;

    LookupResult result;
    for (auto item : inResult.items)
    {
        if (!DeclPassesLookupMask(item.declRef.GetDecl(), mask))
            continue;

        AddToLookupResult(result, item);
    }
    return result;
}

LookupResultItem CreateLookupResultItem(
    DeclRef declRef,
    BreadcrumbInfo* breadcrumbInfos)
{
    LookupResultItem item;
    item.declRef = declRef;

    // breadcrumbs were constructed "backwards" on the stack, so we
    // reverse them here by building a linked list the other way
    RefPtr<LookupResultItem::Breadcrumb> breadcrumbs;
    for (auto bb = breadcrumbInfos; bb; bb = bb->prev)
    {
        breadcrumbs = new LookupResultItem::Breadcrumb(
            bb->kind,
            bb->declRef,
            breadcrumbs);
    }
    item.breadcrumbs = breadcrumbs;
    return item;
}

void DoMemberLookupImpl(
    String const&			name,
    RefPtr<ExpressionType>	baseType,
    LookupRequest const&    request,
    LookupResult&			ioResult,
    BreadcrumbInfo*			breadcrumbs)
{
    // If the type was pointer-like, then dereference it
    // automatically here.
    if (auto pointerLikeType = baseType->As<PointerLikeType>())
    {
        // Need to leave a breadcrumb to indicate that we
        // did an implicit dereference here
        BreadcrumbInfo derefBreacrumb;
        derefBreacrumb.kind = LookupResultItem::Breadcrumb::Kind::Deref;
        derefBreacrumb.prev = breadcrumbs;

        // Recursively perform lookup on the result of deref
        return DoMemberLookupImpl(name, pointerLikeType->elementType, request, ioResult, &derefBreacrumb);
    }

    // Default case: no dereference needed

    if (auto baseDeclRefType = baseType->As<DeclRefType>())
    {
        if (auto baseAggTypeDeclRef = baseDeclRefType->declRef.As<AggTypeDeclRef>())
        {
            DoLocalLookupImpl(name, baseAggTypeDeclRef, request, ioResult, breadcrumbs);
        }
    }

    // TODO(tfoley): any other cases to handle here?
}

void DoMemberLookupImpl(
    String const&	        name,
    DeclRef			        baseDeclRef,
    LookupRequest const&    request,
    LookupResult&	        ioResult,
    BreadcrumbInfo*	        breadcrumbs)
{
    auto baseType = getTypeForDeclRef(baseDeclRef);
    return DoMemberLookupImpl(name, baseType, request, ioResult, breadcrumbs);
}

// Look for members of the given name in the given container for declarations
void DoLocalLookupImpl(
    String const&		    name,
    ContainerDeclRef	    containerDeclRef,
    LookupRequest const&    request,
    LookupResult&		    result,
    BreadcrumbInfo*		    inBreadcrumbs)
{
    ContainerDecl* containerDecl = containerDeclRef.GetDecl();

    // Ensure that the lookup dictionary in the container is up to date
    if (!containerDecl->memberDictionaryIsValid)
    {
        buildMemberDictionary(containerDecl);
    }

    // Look up the declarations with the chosen name in the container.
    Decl* firstDecl = nullptr;
    containerDecl->memberDictionary.TryGetValue(name, firstDecl);

    // Now iterate over those declarations (if any) and see if
    // we find any that meet our filtering criteria.
    // For example, we might be filtering so that we only consider
    // type declarations.
    for (auto m = firstDecl; m; m = m->nextInContainerWithSameName)
    {
        if (!DeclPassesLookupMask(m, request.mask))
            continue;

        // The declaration passed the test, so add it!
        AddToLookupResult(result, CreateLookupResultItem(DeclRef(m, containerDeclRef.substitutions), inBreadcrumbs));
    }


    // TODO(tfoley): should we look up in the transparent decls
    // if we already has a hit in the current container?

    for(auto transparentInfo : containerDecl->transparentMembers)
    {
        // The reference to the transparent member should use whatever
        // substitutions we used in referring to its outer container
        DeclRef transparentMemberDeclRef(transparentInfo.decl, containerDeclRef.substitutions);

        // We need to leave a breadcrumb so that we know that the result
        // of lookup involves a member lookup step here

        BreadcrumbInfo memberRefBreadcrumb;
        memberRefBreadcrumb.kind = LookupResultItem::Breadcrumb::Kind::Member;
        memberRefBreadcrumb.declRef = transparentMemberDeclRef;
        memberRefBreadcrumb.prev = inBreadcrumbs;

        DoMemberLookupImpl(name, transparentMemberDeclRef, request, result, &memberRefBreadcrumb);
    }

    // TODO(tfoley): need to consider lookup via extension here?
}

void DoLookupImpl(
    String const&           name,
    LookupRequest const&    request,
    LookupResult&           result)
{
    auto scope      = request.scope;
    auto endScope   = request.endScope;
    for (;scope != endScope; scope = scope->parent)
    {
        // Note that we consider all "peer" scopes together,
        // so that a hit in one of them does not proclude
        // also finding a hit in another
        for(auto link = scope; link; link = link->nextSibling)
        {
            if(!link->containerDecl)
                continue;

            ContainerDeclRef containerRef = DeclRef(link->containerDecl, nullptr).As<ContainerDeclRef>();
            DoLocalLookupImpl(name, containerRef, request, result, nullptr);
        }

        if (result.isValid())
        {
            // If we've found a result in this scope, then there
            // is no reason to look further up (for now).
            return;
        }
    }

    // If we run out of scopes, then we are done.
}

LookupResult DoLookup(String const& name, LookupRequest const& request)
{
    LookupResult result;
    DoLookupImpl(name, request, result);
    return result;
}

LookupResult LookUp(String const& name, RefPtr<Scope> scope)
{
    LookupRequest request;
    request.scope = scope;
    return DoLookup(name, request);
}

// perform lookup within the context of a particular container declaration,
// and do *not* look further up the chain
LookupResult LookUpLocal(String const& name, ContainerDeclRef containerDeclRef)
{
    LookupRequest request;
    LookupResult result;
    DoLocalLookupImpl(name, containerDeclRef, request, result, nullptr);
    return result;
}

LookupResult LookUpLocal(String const& name, ContainerDecl* containerDecl)
{
    ContainerDeclRef containerRef = DeclRef(containerDecl, nullptr).As<ContainerDeclRef>();
    return LookUpLocal(name, containerRef);
}


}}
