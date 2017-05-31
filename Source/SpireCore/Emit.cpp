// Emit.cpp
#include "Emit.h"

#include "Syntax.h"
#include "TypeLayout.h"

#include <assert.h>

#ifdef _WIN32
#include <d3dcompiler.h>
#pragma warning(disable:4996)
#endif

namespace Spire { namespace Compiler {

struct EmitContext
{
    StringBuilder sb;

    // Current source position for tracking purposes...
    CodePosition loc;

    // The target language we want to generate code for
    CodeGenTarget target;
};

//

static void EmitDecl(EmitContext* context, RefPtr<Decl> decl);
static void EmitDecl(EmitContext* context, RefPtr<DeclBase> declBase);
static void EmitDeclUsingLayout(EmitContext* context, RefPtr<Decl> decl, RefPtr<VarLayout> layout);
static void EmitType(EmitContext* context, RefPtr<ExpressionType> type, String const& name);
static void EmitType(EmitContext* context, RefPtr<ExpressionType> type);
static void EmitExpr(EmitContext* context, RefPtr<ExpressionSyntaxNode> expr);
static void EmitStmt(EmitContext* context, RefPtr<StatementSyntaxNode> stmt);
static void EmitDeclRef(EmitContext* context, DeclRef declRef);

// Low-level emit logic

static void emitRawTextSpan(EmitContext* context, char const* textBegin, char const* textEnd)
{
    // TODO(tfoley): Need to make "corelib" not use `int` for pointer-sized things...
    auto len = int(textEnd - textBegin);

    context->sb.Append(textBegin, len);
}

static void emitRawText(EmitContext* context, char const* text)
{
    emitRawTextSpan(context, text, text + strlen(text));
}

static void emitTextSpan(EmitContext* context, char const* textBegin, char const* textEnd)
{
    // Emit the raw text
    emitRawTextSpan(context, textBegin, textEnd);

    // Update our logical position
    // TODO(tfoley): Need to make "corelib" not use `int` for pointer-sized things...
    auto len = int(textEnd - textBegin);
    context->loc.Col += len;
}

static void Emit(EmitContext* context, char const* textBegin, char const* textEnd)
{
    char const* spanBegin = textBegin;

    char const* spanEnd = spanBegin;
    for(;;)
    {
        if(spanEnd == textEnd)
        {
            // We have a whole range of text waiting to be flushed
            emitTextSpan(context, spanBegin, spanEnd);
            return;
        }

        auto c = *spanEnd++;

        if( c == '\n' )
        {
            // At the end of a line, we need to update our tracking
            // information on code positions
            emitTextSpan(context, spanBegin, spanEnd);
            context->loc.Line++;
            context->loc.Col = 1;

            // Start a new span for emit purposes
            spanBegin = spanEnd;
        }
    }
}

static void Emit(EmitContext* context, char const* text)
{
    Emit(context, text, text + strlen(text));
}

static void Emit(EmitContext* context, String const& text)
{
    Emit(context, text.begin(), text.end());
}

static void Emit(EmitContext* context, UInt value)
{
    char buffer[32];
    sprintf(buffer, "%llu", (unsigned long long)(value));
    Emit(context, buffer);
}

static void Emit(EmitContext* context, int value)
{
    char buffer[16];
    sprintf(buffer, "%d", value);
    Emit(context, buffer);
}

static void Emit(EmitContext* context, double value)
{
    // TODO(tfoley): need to print things in a way that can round-trip
    char buffer[128];
    sprintf(buffer, "%f", value);
    Emit(context, buffer);
}

// Expressions

// Determine if an expression should not be emitted when it is the base of
// a member reference expression.
static bool IsBaseExpressionImplicit(EmitContext* /*context*/, RefPtr<ExpressionSyntaxNode> expr)
{
    // HACK(tfoley): For now, anything with a constant-buffer type should be
    // left implicit.

    // Look through any dereferencing that took place
    RefPtr<ExpressionSyntaxNode> e = expr;
    while (auto derefExpr = e.As<DerefExpr>())
    {
        e = derefExpr->base;
    }
    // Is the expression referencing a constant buffer?
    if (auto cbufferType = e->Type->As<ConstantBufferType>())
    {
        return true;
    }

    return false;
}

enum
{
    kPrecedence_None,
    kPrecedence_Comma,

    kPrecedence_Assign,
    kPrecedence_AddAssign = kPrecedence_Assign,
    kPrecedence_SubAssign = kPrecedence_Assign,
    kPrecedence_MulAssign = kPrecedence_Assign,
    kPrecedence_DivAssign = kPrecedence_Assign,
    kPrecedence_ModAssign = kPrecedence_Assign,
    kPrecedence_LshAssign = kPrecedence_Assign,
    kPrecedence_RshAssign = kPrecedence_Assign,
    kPrecedence_OrAssign = kPrecedence_Assign,
    kPrecedence_AndAssign = kPrecedence_Assign,
    kPrecedence_XorAssign = kPrecedence_Assign,

    kPrecedence_General = kPrecedence_Assign,

    kPrecedence_Conditional, // "ternary"
    kPrecedence_Or,
    kPrecedence_And,
    kPrecedence_BitOr,
    kPrecedence_BitXor,
    kPrecedence_BitAnd,

    kPrecedence_Eql,
    kPrecedence_Neq = kPrecedence_Eql,

    kPrecedence_Less,
    kPrecedence_Greater = kPrecedence_Less,
    kPrecedence_Leq = kPrecedence_Less,
    kPrecedence_Geq = kPrecedence_Less,

    kPrecedence_Lsh,
    kPrecedence_Rsh = kPrecedence_Lsh,

    kPrecedence_Add,
    kPrecedence_Sub = kPrecedence_Add,

    kPrecedence_Mul,
    kPrecedence_Div = kPrecedence_Mul,
    kPrecedence_Mod = kPrecedence_Mul,

    kPrecedence_Prefix,
    kPrecedence_Postifx,
    kPrecedence_Atomic = kPrecedence_Postifx
};

static void EmitExprWithPrecedence(EmitContext* context, RefPtr<ExpressionSyntaxNode> expr, int outerPrec);

static void EmitPostfixExpr(EmitContext* context, RefPtr<ExpressionSyntaxNode> expr)
{
    EmitExprWithPrecedence(context, expr, kPrecedence_Postifx);
}

static void EmitExpr(EmitContext* context, RefPtr<ExpressionSyntaxNode> expr)
{
    EmitExprWithPrecedence(context, expr, kPrecedence_General);
}

static bool MaybeEmitParens(EmitContext* context, int outerPrec, int prec)
{
    if (prec <= outerPrec)
    {
        Emit(context, "(");
        return true;
    }
    return false;
}

static void EmitBinExpr(EmitContext* context, int outerPrec, int prec, char const* op, RefPtr<OperatorExpressionSyntaxNode> binExpr)
{
    bool needsClose = MaybeEmitParens(context, outerPrec, prec);
    EmitExprWithPrecedence(context, binExpr->Arguments[0], prec);
    Emit(context, " ");
    Emit(context, op);
    Emit(context, " ");
    EmitExprWithPrecedence(context, binExpr->Arguments[1], prec);
    if (needsClose)
    {
        Emit(context, ")");
    }
}

static void EmitUnaryExpr(
    EmitContext* context,
    int outerPrec,
    int prec,
    char const* preOp,
    char const* postOp,
    RefPtr<OperatorExpressionSyntaxNode> binExpr)
{
    bool needsClose = MaybeEmitParens(context, outerPrec, prec);
    Emit(context, preOp);
    EmitExprWithPrecedence(context, binExpr->Arguments[0], prec);
    Emit(context, postOp);
    if (needsClose)
    {
        Emit(context, ")");
    }
}

static void EmitExprWithPrecedence(EmitContext* context, RefPtr<ExpressionSyntaxNode> expr, int outerPrec)
{
    bool needClose = false;
    if (auto selectExpr = expr.As<SelectExpressionSyntaxNode>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Conditional);

        EmitExprWithPrecedence(context, selectExpr->Arguments[0], kPrecedence_Conditional);
        Emit(context, " ? ");
        EmitExprWithPrecedence(context, selectExpr->Arguments[1], kPrecedence_Conditional);
        Emit(context, " : ");
        EmitExprWithPrecedence(context, selectExpr->Arguments[2], kPrecedence_Conditional);
    }
    else if (auto binExpr = expr.As<OperatorExpressionSyntaxNode>())
    {
        switch (binExpr->Operator)
        {
#define CASE(NAME, OP) case Operator::NAME: EmitBinExpr(context, outerPrec, kPrecedence_##NAME, #OP, binExpr); break
            CASE(Mul, *);
            CASE(Div, / );
            CASE(Mod, %);
            CASE(Add, +);
            CASE(Sub, -);
            CASE(Lsh, << );
            CASE(Rsh, >> );
            CASE(Eql, == );
            CASE(Neq, != );
            CASE(Greater, >);
            CASE(Less, <);
            CASE(Geq, >= );
            CASE(Leq, <= );
            CASE(BitAnd, &);
            CASE(BitXor, ^);
            CASE(BitOr, | );
            CASE(And, &&);
            CASE(Or, || );
            CASE(Assign, =);
            CASE(AddAssign, +=);
            CASE(SubAssign, -=);
            CASE(MulAssign, *=);
            CASE(DivAssign, /=);
            CASE(ModAssign, %=);
            CASE(LshAssign, <<=);
            CASE(RshAssign, >>=);
            CASE(OrAssign, |=);
            CASE(AndAssign, &=);
            CASE(XorAssign, ^=);
#undef CASE
        case Operator::Sequence: EmitBinExpr(context, outerPrec, kPrecedence_Comma, ",", binExpr); break;
#define PREFIX(NAME, OP) case Operator::NAME: EmitUnaryExpr(context, outerPrec, kPrecedence_Prefix, #OP, "", binExpr); break
#define POSTFIX(NAME, OP) case Operator::NAME: EmitUnaryExpr(context, outerPrec, kPrecedence_Postfix, "", #OP, binExpr); break

            PREFIX(Neg, -);
            PREFIX(Not, !);
            PREFIX(BitNot, ~);
            PREFIX(PreInc, ++);
            PREFIX(PreDec, --);
            PREFIX(PostInc, ++);
            PREFIX(PostDec, --);
#undef PREFIX
#undef POSTFIX
        default:
            assert(!"unreachable");
            break;
        }
    }
    else if (auto appExpr = expr.As<InvokeExpressionSyntaxNode>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Postifx);

        auto funcExpr = appExpr->FunctionExpr;
        if (auto funcDeclRefExpr = funcExpr.As<DeclRefExpr>())
        {
            auto declRef = funcDeclRefExpr->declRef;
            if (auto ctorDeclRef = declRef.As<ConstructorDeclRef>())
            {
                // We really want to emit a reference to the type begin constructed
                EmitType(context, expr->Type);
            }
            else
            {
                // default case: just emit the decl ref
                EmitExpr(context, funcExpr);
            }
        }
        else
        {
            // default case: just emit the expression
            EmitPostfixExpr(context, funcExpr);
        }

        Emit(context, "(");
        int argCount = appExpr->Arguments.Count();
        for (int aa = 0; aa < argCount; ++aa)
        {
            if (aa != 0) Emit(context, ", ");
            EmitExpr(context, appExpr->Arguments[aa]);
        }
        Emit(context, ")");
    }
    else if (auto memberExpr = expr.As<MemberExpressionSyntaxNode>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Postifx);

        // TODO(tfoley): figure out a good way to reference
        // declarations that might be generic and/or might
        // not be generated as lexically nested declarations...

        // TODO(tfoley): also, probably need to special case
        // this for places where we are using a built-in...

        auto base = memberExpr->BaseExpression;
        if (IsBaseExpressionImplicit(context, base))
        {
            // don't emit the base expression
        }
        else
        {
            EmitExprWithPrecedence(context, memberExpr->BaseExpression, kPrecedence_Postifx);
            Emit(context, ".");
        }

        Emit(context, memberExpr->declRef.GetName());
    }
    else if (auto swizExpr = expr.As<SwizzleExpr>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Postifx);

        EmitExprWithPrecedence(context, swizExpr->base, kPrecedence_Postifx);
        Emit(context, ".");
        static const char* kComponentNames[] = { "x", "y", "z", "w" };
        int elementCount = swizExpr->elementCount;
        for (int ee = 0; ee < elementCount; ++ee)
        {
            Emit(context, kComponentNames[swizExpr->elementIndices[ee]]);
        }
    }
    else if (auto indexExpr = expr.As<IndexExpressionSyntaxNode>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Postifx);

        EmitExprWithPrecedence(context, indexExpr->BaseExpression, kPrecedence_Postifx);
        Emit(context, "[");
        EmitExpr(context, indexExpr->IndexExpression);
        Emit(context, "]");
    }
    else if (auto varExpr = expr.As<VarExpressionSyntaxNode>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Atomic);

        // Because of the "rewriter" use case, it is possible that we will
        // be trying to emit an expression that hasn't been wired up to
        // any associated declaration. In that case, we will just emit
        // the variable name.
        //
        // TODO: A better long-term solution here is to have a distinct
        // case for an "unchecked" `NameExpr` that doesn't include
        // a declaration reference.

        if(varExpr->declRef)
        {
            EmitDeclRef(context, varExpr->declRef);
        }
        else
        {
            Emit(context, varExpr->Variable);
        }
    }
    else if (auto derefExpr = expr.As<DerefExpr>())
    {
        // TODO(tfoley): dereference shouldn't always be implicit
        EmitExprWithPrecedence(context, derefExpr->base, outerPrec);
    }
    else if (auto litExpr = expr.As<ConstantExpressionSyntaxNode>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Atomic);

        switch (litExpr->ConstType)
        {
        case ConstantExpressionSyntaxNode::ConstantType::Int:
            Emit(context, litExpr->IntValue);
            break;
        case ConstantExpressionSyntaxNode::ConstantType::Float:
            Emit(context, litExpr->FloatValue);
            break;
        case ConstantExpressionSyntaxNode::ConstantType::Bool:
            Emit(context, litExpr->IntValue ? "true" : "false");
            break;
        default:
            assert(!"unreachable");
            break;
        }
    }
    else if (auto castExpr = expr.As<TypeCastExpressionSyntaxNode>())
    {
        needClose = MaybeEmitParens(context, outerPrec, kPrecedence_Prefix);

        Emit(context, "(");
        EmitType(context, castExpr->Type);
        Emit(context, ") ");
        EmitExpr(context, castExpr->Expression);
    }
    else if(auto initExpr = expr.As<InitializerListExpr>())
    {
        Emit(context, "{ ");
        for(auto& arg : initExpr->args)
        {
            EmitExpr(context, arg);
            Emit(context, ", ");
        }
        Emit(context, "}");
    }
    else
    {
        throw "unimplemented";
    }
    if (needClose)
    {
        Emit(context, ")");
    }
}

// Types

void Emit(EmitContext* context, RefPtr<IntVal> val)
{
    Emit(context, GetIntVal(val));
}

// represents a declarator for use in emitting types
struct EDeclarator
{
    enum class Flavor
    {
        Name,
        Array,
        UnsizedArray,
    };
    Flavor flavor;
    EDeclarator* next = nullptr;

    // Used for `Flavor::Name`
    String name;

    // Used for `Flavor::Array`
    int elementCount;
};

static void EmitDeclarator(EmitContext* context, EDeclarator* declarator)
{
    if (!declarator) return;

    Emit(context, " ");

    switch (declarator->flavor)
    {
    case EDeclarator::Flavor::Name:
        Emit(context, declarator->name);
        break;

    case EDeclarator::Flavor::Array:
        EmitDeclarator(context, declarator->next);
        Emit(context, "[");
        if(auto elementCount = declarator->elementCount)
        {
            Emit(context, elementCount);
        }
        Emit(context, "]");
        break;

    case EDeclarator::Flavor::UnsizedArray:
        EmitDeclarator(context, declarator->next);
        Emit(context, "[]");
        break;

    default:
        assert(!"unreachable");
        break;
    }
}

static void emitGLSLTypePrefix(
    EmitContext*            context,
    RefPtr<ExpressionType>  type)
{
    if(auto basicElementType = type->As<BasicExpressionType>())
    {
        switch (basicElementType->BaseType)
        {
        case BaseType::Float:
            // no prefix
            break;

        case BaseType::Int:		Emit(context, "i");		break;
        case BaseType::UInt:	Emit(context, "u");		break;
        case BaseType::Bool:	Emit(context, "b");		break;
        default:
            assert(!"unreachable");
            break;
        }
    }
    else if(auto vectorType = type->As<VectorExpressionType>())
    {
        emitGLSLTypePrefix(context, vectorType->elementType);
    }
    else if(auto matrixType = type->As<MatrixExpressionType>())
    {
        emitGLSLTypePrefix(context, matrixType->getElementType());
    }
    else
    {
        assert(!"unreachable");
    }
}

static void emitHLSLTextureType(
    EmitContext*        context,
    RefPtr<TextureType> texType)
{
    switch(texType->getAccess())
    {
    case SPIRE_RESOURCE_ACCESS_READ:
        break;

    case SPIRE_RESOURCE_ACCESS_READ_WRITE:
        Emit(context, "RW");
        break;

    case SPIRE_RESOURCE_ACCESS_RASTER_ORDERED:
        Emit(context, "RasterizerOrdered");
        break;

    case SPIRE_RESOURCE_ACCESS_APPEND:
        Emit(context, "Append");
        break;

    case SPIRE_RESOURCE_ACCESS_CONSUME:
        Emit(context, "Consume");
        break;

    default:
        assert(!"unreachable");
        break;
    }

    switch (texType->GetBaseShape())
    {
    case TextureType::Shape1D:		Emit(context, "Texture1D");		break;
    case TextureType::Shape2D:		Emit(context, "Texture2D");		break;
    case TextureType::Shape3D:		Emit(context, "Texture3D");		break;
    case TextureType::ShapeCube:	Emit(context, "TextureCube");	break;
    default:
        assert(!"unreachable");
        break;
    }

    if (texType->isMultisample())
    {
        Emit(context, "MS");
    }
    if (texType->isArray())
    {
        Emit(context, "Array");
    }
    Emit(context, "<");
    EmitType(context, texType->elementType);
    Emit(context, ">");
}

static void emitGLSLTextureOrTextureSamplerType(
    EmitContext*            context,
    RefPtr<TextureTypeBase> type,
    char const*             baseName)
{
    emitGLSLTypePrefix(context, type->elementType);

    Emit(context, baseName);
    switch (type->GetBaseShape())
    {
    case TextureType::Shape1D:		Emit(context, "1D");		break;
    case TextureType::Shape2D:		Emit(context, "2D");		break;
    case TextureType::Shape3D:		Emit(context, "3D");		break;
    case TextureType::ShapeCube:	Emit(context, "Cube");	break;
    default:
        assert(!"unreachable");
        break;
    }
}

static void emitGLSLTextureType(
    EmitContext*        context,
    RefPtr<TextureType> texType)
{
    emitGLSLTextureOrTextureSamplerType(context, texType, "texture");
}

static void emitGLSLTextureSamplerType(
    EmitContext*                context,
    RefPtr<TextureSamplerType>  type)
{
    emitGLSLTextureOrTextureSamplerType(context, type, "sampler");
}

static void emitTextureType(
    EmitContext*        context,
    RefPtr<TextureType> texType)
{
    switch(context->target)
    {
    case CodeGenTarget::HLSL:
        emitHLSLTextureType(context, texType);
        break;

    case CodeGenTarget::GLSL:
        emitGLSLTextureType(context, texType);
        break;

    default:
        assert(!"unreachable");
        break;
    }
}

static void emitTextureSamplerType(
    EmitContext*                context,
    RefPtr<TextureSamplerType>  type)
{
    switch(context->target)
    {
    case CodeGenTarget::GLSL:
        emitGLSLTextureSamplerType(context, type);
        break;

    default:
        assert(!"unreachable");
        break;
    }
}

static void EmitType(EmitContext* context, RefPtr<ExpressionType> type, EDeclarator* declarator)
{
    if (auto basicType = type->As<BasicExpressionType>())
    {
        switch (basicType->BaseType)
        {
        case BaseType::Void:	Emit(context, "void");		break;
        case BaseType::Int:		Emit(context, "int");		break;
        case BaseType::Float:	Emit(context, "float");		break;
        case BaseType::UInt:	Emit(context, "uint");		break;
        case BaseType::Bool:	Emit(context, "bool");		break;
        default:
            assert(!"unreachable");
            break;
        }

        EmitDeclarator(context, declarator);
        return;
    }
    else if (auto vecType = type->As<VectorExpressionType>())
    {
        switch(context->target)
        {
        case CodeGenTarget::GLSL:
        case CodeGenTarget::GLSL_Vulkan:
        case CodeGenTarget::GLSL_Vulkan_OneDesc:
            {
                emitGLSLTypePrefix(context, vecType->elementType);
                Emit(context, "vec");
                Emit(context, vecType->elementCount);
            }
            break;

        case CodeGenTarget::HLSL:
            // TODO(tfoley): should really emit these with sugar
            Emit(context, "vector<");
            EmitType(context, vecType->elementType);
            Emit(context, ",");
            Emit(context, vecType->elementCount);
            Emit(context, ">");
            break;

        default:
            assert(!"unreachable");
            break;
        }

        Emit(context, " ");
        EmitDeclarator(context, declarator);
        return;
    }
    else if (auto matType = type->As<MatrixExpressionType>())
    {
        switch(context->target)
        {
        case CodeGenTarget::GLSL:
        case CodeGenTarget::GLSL_Vulkan:
        case CodeGenTarget::GLSL_Vulkan_OneDesc:
            {
                emitGLSLTypePrefix(context, matType->getElementType());
                Emit(context, "mat");
                Emit(context, matType->getRowCount());
                // TODO(tfoley): only emit the next bit
                // for non-square matrix
                Emit(context, "x");
                Emit(context, matType->getColumnCount());
            }
            break;

        case CodeGenTarget::HLSL:
            // TODO(tfoley): should really emit these with sugar
            Emit(context, "matrix<");
            EmitType(context, matType->getElementType());
            Emit(context, ",");
            Emit(context, matType->getRowCount());
            Emit(context, ",");
            Emit(context, matType->getColumnCount());
            Emit(context, "> ");
            break;

        default:
            assert(!"unreachable");
            break;
        }

        Emit(context, " ");
        EmitDeclarator(context, declarator);
        return;
    }
    else if (auto texType = type->As<TextureType>())
    {
        emitTextureType(context, texType);
        Emit(context, " ");
        EmitDeclarator(context, declarator);
        return;
    }
    else if (auto textureSamplerType = type->As<TextureSamplerType>())
    {
        emitTextureSamplerType(context, textureSamplerType);
        Emit(context, " ");
        EmitDeclarator(context, declarator);
        return;
    }
    else if (auto samplerStateType = type->As<SamplerStateType>())
    {
        switch (samplerStateType->flavor)
        {
        case SamplerStateType::Flavor::SamplerState:			Emit(context, "SamplerState");				break;
        case SamplerStateType::Flavor::SamplerComparisonState:	Emit(context, "SamplerComparisonState");	break;
        default:
            assert(!"unreachable");
            break;
        }

        EmitDeclarator(context, declarator);
        return;
    }
    else if (auto declRefType = type->As<DeclRefType>())
    {
        EmitDeclRef(context,  declRefType->declRef);

        EmitDeclarator(context, declarator);
        return;
    }
    else if( auto arrayType = type->As<ArrayExpressionType>() )
    {
        EDeclarator arrayDeclarator;
        arrayDeclarator.next = declarator;

        if(arrayType->ArrayLength)
        {
            arrayDeclarator.flavor = EDeclarator::Flavor::Array;
            arrayDeclarator.elementCount = GetIntVal(arrayType->ArrayLength);
        }
        else
        {
            arrayDeclarator.flavor = EDeclarator::Flavor::UnsizedArray;
        }


        EmitType(context, arrayType->BaseType, &arrayDeclarator);
        return;
    }

    throw "unimplemented";
}

static void EmitType(EmitContext* context, RefPtr<ExpressionType> type, String const& name)
{
    EDeclarator nameDeclarator;
    nameDeclarator.flavor = EDeclarator::Flavor::Name;
    nameDeclarator.name = name;
    EmitType(context, type, &nameDeclarator);
}

static void EmitType(EmitContext* context, RefPtr<ExpressionType> type)
{
    EmitType(context, type, nullptr);
}

// Statements

// Emit a statement as a `{}`-enclosed block statement, but avoid adding redundant
// curly braces if the statement is itself a block statement.
static void EmitBlockStmt(EmitContext* context, RefPtr<StatementSyntaxNode> stmt)
{
    // TODO(tfoley): support indenting
    Emit(context, "{\n");
    if( auto blockStmt = stmt.As<BlockStatementSyntaxNode>() )
    {
        for (auto s : blockStmt->Statements)
        {
            EmitStmt(context, s);
        }
    }
    else
    {
        EmitStmt(context, stmt);
    }
    Emit(context, "}\n");
}

static void EmitLoopAttributes(EmitContext* context, RefPtr<StatementSyntaxNode> decl)
{
    // TODO(tfoley): There really ought to be a semantic checking step for attributes,
    // that turns abstract syntax into a concrete hierarchy of attribute types (e.g.,
    // a specific `LoopModifier` or `UnrollModifier`).

    for(auto attr : decl->GetModifiersOfType<HLSLUncheckedAttribute>())
    {
        if(attr->nameToken.Content == "loop")
        {
            Emit(context, "[loop]");
        }
        else if(attr->nameToken.Content == "unroll")
        {
            Emit(context, "[unroll]");
        }
    }
}

static void advanceToSourceLocation(
    EmitContext*        context,
    CodePosition const& sourceLocation)
{
    // If we are currently emitting code at a source location with
    // a differnet file or line, *or* if the source location is
    // somehow later on the line than what we want to emit,
    // then we need to emit a new `#line` directive.
    if(sourceLocation.FileName != context->loc.FileName
        || sourceLocation.Line != context->loc.Line
        || sourceLocation.Col < context->loc.Col)
    {
        emitRawText(context, "\n#line ");

        char buffer[16];
        sprintf(buffer, "%d", sourceLocation.Line);
        emitRawText(context, buffer);

        emitRawText(context, "\"");
        for(auto c : sourceLocation.FileName)
        {
            char charBuffer[] = { c, 0 };
            switch(c)
            {
            default:
                emitRawText(context, charBuffer);
                break;

            // TODO: should probably canonicalize paths to not use backslash somewhere else
            // in the compilation pipeline...
            case '\\':
                emitRawText(context, "/");
                break;
            }
        }
        emitRawText(context, "\"\n");
    
        context->loc.FileName = sourceLocation.FileName;
        context->loc.Line = sourceLocation.Line;
        context->loc.Col = 1;
    }

    // Now indent up to the appropriate column, so that error messages
    // that reference columns will be correct.
    //
    // TODO: This logic does not take into account whether indentation
    // came in as spaces or tabs, so there is necessarily going to be
    // coupling between how the downstream compiler counts columns,
    // and how we do.
    if(sourceLocation.Col > context->loc.Col)
    {
        int delta = sourceLocation.Col - context->loc.Col;
        for( int ii = 0; ii < delta; ++ii )
        {
            emitRawText(context, " ");
        }
        context->loc.Col = sourceLocation.Col;
    }
}

static void emitTokenWithLocation(EmitContext* context, Token const& token)
{
    if( token.Position.FileName.Length() != 0 )
    {
        advanceToSourceLocation(context, token.Position);
    }
    else
    {
        // If we don't have the original position info, we need to play
        // it safe and emit whitespace to line things up nicely

        if(token.flags & TokenFlag::AtStartOfLine)
            Emit(context, "\n");
        // TODO(tfoley): macro expansion can currently lead to whitespace getting dropped,
        // so we will just insert it aggressively, to play it safe.
        else //  if(token.flags & TokenFlag::AfterWhitespace)
            Emit(context, " ");
    }

    // Emit the raw textual content of the token
    Emit(context, token.Content);
}

static void EmitUnparsedStmt(EmitContext* context, RefPtr<UnparsedStmt> stmt)
{
    // TODO: actually emit the tokens that made up the statement...
    Emit(context, "{\n");
    for( auto& token : stmt->tokens )
    {
        emitTokenWithLocation(context, token);
    }
    Emit(context, "}\n");
}

static void EmitStmt(EmitContext* context, RefPtr<StatementSyntaxNode> stmt)
{
    if (auto blockStmt = stmt.As<BlockStatementSyntaxNode>())
    {
        EmitBlockStmt(context, blockStmt);
        return;
    }
    else if( auto unparsedStmt = stmt.As<UnparsedStmt>() )
    {
        EmitUnparsedStmt(context, unparsedStmt);
        return;
    }
    else if (auto exprStmt = stmt.As<ExpressionStatementSyntaxNode>())
    {
        EmitExpr(context, exprStmt->Expression);
        Emit(context, ";\n");
        return;
    }
    else if (auto returnStmt = stmt.As<ReturnStatementSyntaxNode>())
    {
        Emit(context, "return");
        if (auto expr = returnStmt->Expression)
        {
            Emit(context, " ");
            EmitExpr(context, expr);
        }
        Emit(context, ";\n");
        return;
    }
    else if (auto declStmt = stmt.As<VarDeclrStatementSyntaxNode>())
    {
        EmitDecl(context, declStmt->decl);
        return;
    }
    else if (auto ifStmt = stmt.As<IfStatementSyntaxNode>())
    {
        Emit(context, "if(");
        EmitExpr(context, ifStmt->Predicate);
        Emit(context, ")\n");
        EmitBlockStmt(context, ifStmt->PositiveStatement);
        if(auto elseStmt = ifStmt->NegativeStatement)
        {
            Emit(context, "\nelse\n");
            EmitBlockStmt(context, elseStmt);
        }
        return;
    }
    else if (auto forStmt = stmt.As<ForStatementSyntaxNode>())
    {
        EmitLoopAttributes(context, forStmt);

        Emit(context, "for(");
        if (auto initStmt = forStmt->InitialStatement)
        {
            EmitStmt(context, initStmt);
        }
        else
        {
            Emit(context, ";");
        }
        if (auto testExp = forStmt->PredicateExpression)
        {
            EmitExpr(context, testExp);
        }
        Emit(context, ";");
        if (auto incrExpr = forStmt->SideEffectExpression)
        {
            EmitExpr(context, incrExpr);
        }
        Emit(context, ")\n");
        EmitBlockStmt(context, forStmt->Statement);
        return;
    }
    else if (auto discardStmt = stmt.As<DiscardStatementSyntaxNode>())
    {
        Emit(context, "discard;\n");
        return;
    }
    else if (auto emptyStmt = stmt.As<EmptyStatementSyntaxNode>())
    {
        return;
    }
    else if (auto switchStmt = stmt.As<SwitchStmt>())
    {
        Emit(context, "switch(");
        EmitExpr(context, switchStmt->condition);
        Emit(context, ")\n");
        EmitBlockStmt(context, switchStmt->body);
        return;
    }
    else if (auto caseStmt = stmt.As<CaseStmt>())
    {
        Emit(context, "case ");
        EmitExpr(context, caseStmt->expr);
        Emit(context, ":\n");
        return;
    }
    else if (auto defaultStmt = stmt.As<DefaultStmt>())
    {
        Emit(context, "default:{}\n");
        return;
    }
    else if (auto breakStmt = stmt.As<BreakStatementSyntaxNode>())
    {
        Emit(context, "break;\n");
        return;
    }
    else if (auto continueStmt = stmt.As<ContinueStatementSyntaxNode>())
    {
        Emit(context, "continue;\n");
        return;
    }

    throw "unimplemented";

}

// Declaration References

static void EmitVal(EmitContext* context, RefPtr<Val> val)
{
    if (auto type = val.As<ExpressionType>())
    {
        EmitType(context, type);
    }
    else if (auto intVal = val.As<IntVal>())
    {
        Emit(context, intVal);
    }
    else
    {
        // Note(tfoley): ignore unhandled cases for semantics for now...
//		assert(!"unimplemented");
    }
}

static void EmitDeclRef(EmitContext* context, DeclRef declRef)
{
    // TODO: need to qualify a declaration name based on parent scopes/declarations

    // Emit the name for the declaration itself
    Emit(context, declRef.GetName());

    // If the declaration is nested directly in a generic, then
    // we need to output the generic arguments here
    auto parentDeclRef = declRef.GetParent();
    if (auto genericDeclRef = parentDeclRef.As<GenericDeclRef>())
    {
        // Only do this for declarations of appropriate flavors
        if(auto funcDeclRef = declRef.As<FuncDeclBaseRef>())
        {
            // Don't emit generic arguments for functions, because HLSL doesn't allow them
            return;
        }

        Substitutions* subst = declRef.substitutions.Ptr();
        Emit(context, "<");
        int argCount = subst->args.Count();
        for (int aa = 0; aa < argCount; ++aa)
        {
            if (aa != 0) Emit(context, ",");
            EmitVal(context, subst->args[aa]);
        }
        Emit(context, ">");
    }

}

// Declarations

// Emit any modifiers that should go in front of a declaration
static void EmitModifiers(EmitContext* context, RefPtr<Decl> decl)
{
    // Emit any GLSL `layout` modifiers first
    bool anyLayout = false;
    for( auto mod : decl->GetModifiersOfType<GLSLUnparsedLayoutModifier>())
    {
        if(!anyLayout)
        {
            Emit(context, "layout(");
            anyLayout = true;
        }
        else
        {
            Emit(context, ", ");
        }

        Emit(context, mod->nameToken.Content);
        if(mod->valToken.Type != TokenType::Unknown)
        {
            Emit(context, " = ");
            Emit(context, mod->valToken.Content);
        }
    }
    if(anyLayout)
    {
        Emit(context, ")\n");
    }

    for (auto mod = decl->modifiers.first; mod; mod = mod->next)
    {
        if (0) {}

        #define CASE(TYPE, KEYWORD) \
            else if(auto mod_##TYPE = mod.As<TYPE>()) Emit(context, #KEYWORD " ")

        CASE(RowMajorLayoutModifier, row_major);
        CASE(ColumnMajorLayoutModifier, column_major);
        CASE(HLSLNoInterpolationModifier, nointerpolation);
        CASE(HLSLPreciseModifier, precise);
        CASE(HLSLEffectSharedModifier, shared);
        CASE(HLSLGroupSharedModifier, groupshared);
        CASE(HLSLStaticModifier, static);
        CASE(HLSLUniformModifier, uniform);
        CASE(HLSLVolatileModifier, volatile);

        CASE(InOutModifier, inout);
        CASE(InModifier, in);
        CASE(OutModifier, out);

        CASE(HLSLPointModifier, point);
        CASE(HLSLLineModifier, line);
        CASE(HLSLTriangleModifier, triangle);
        CASE(HLSLLineAdjModifier, lineadj);
        CASE(HLSLTriangleAdjModifier, triangleadj);

        CASE(HLSLLinearModifier, linear);
        CASE(HLSLSampleModifier, sample);
        CASE(HLSLCentroidModifier, centroid);

        CASE(ConstModifier, const);

        #undef CASE

        // TODO: eventually we should be checked these modifiers, but for
        // now we can emit them unchecked, I guess
        else if (auto uncheckedAttr = mod.As<HLSLAttribute>())
        {
            Emit(context, "[");
            Emit(context, uncheckedAttr->nameToken.Content);
            auto& args = uncheckedAttr->args;
            auto argCount = args.Count();
            if (argCount != 0)
            {
                Emit(context, "(");
                for (int aa = 0; aa < argCount; ++aa)
                {
                    if (aa != 0) Emit(context, ", ");
                    EmitExpr(context, args[aa]);
                }
                Emit(context, ")");
            }
            Emit(context, "]");
        }

        else if(auto simpleModifier = mod.As<SimpleModifier>())
        {
            Emit(context, simpleModifier->nameToken.Content);
            Emit(context, " ");
        }

        else
        {
            // skip any extra modifiers
        }
    }
}


typedef unsigned int ESemanticMask;
enum
{
    kESemanticMask_None = 0,

    kESemanticMask_NoPackOffset = 1 << 0,

    kESemanticMask_Default = kESemanticMask_NoPackOffset,
};

static void EmitSemantic(EmitContext* context, RefPtr<HLSLSemantic> semantic, ESemanticMask /*mask*/)
{
    if (auto simple = semantic.As<HLSLSimpleSemantic>())
    {
        Emit(context, ": ");
        Emit(context, simple->name.Content);
    }
    else if(auto registerSemantic = semantic.As<HLSLRegisterSemantic>())
    {
        // Don't print out semantic from the user, since we are going to print the same thing our own way...
#if 0
        Emit(context, ": register(");
        Emit(context, registerSemantic->registerName.Content);
        if(registerSemantic->componentMask.Type != TokenType::Unknown)
        {
            Emit(context, ".");
            Emit(context, registerSemantic->componentMask.Content);
        }
        Emit(context, ")");
#endif
    }
    else if(auto packOffsetSemantic = semantic.As<HLSLPackOffsetSemantic>())
    {
        // Don't print out semantic from the user, since we are going to print the same thing our own way...
#if 0
        if(mask & kESemanticMask_NoPackOffset)
            return;

        Emit(context, ": packoffset(");
        Emit(context, packOffsetSemantic->registerName.Content);
        if(packOffsetSemantic->componentMask.Type != TokenType::Unknown)
        {
            Emit(context, ".");
            Emit(context, packOffsetSemantic->componentMask.Content);
        }
        Emit(context, ")");
#endif
    }
    else
    {
        assert(!"unimplemented");
    }
}


static void EmitSemantics(EmitContext* context, RefPtr<Decl> decl, ESemanticMask mask = kESemanticMask_Default )
{
    for (auto mod = decl->modifiers.first; mod; mod = mod->next)
    {
        auto semantic = mod.As<HLSLSemantic>();
        if (!semantic)
            continue;

        EmitSemantic(context, semantic, mask);
    }
}

static void EmitDeclsInContainer(EmitContext* context, RefPtr<ContainerDecl> container)
{
    for (auto member : container->Members)
    {
        EmitDecl(context, member);
    }
}

static void EmitDeclsInContainerUsingLayout(
    EmitContext*                context,
    RefPtr<ContainerDecl>       container,
    RefPtr<StructTypeLayout>    containerLayout)
{
    for (auto member : container->Members)
    {
        RefPtr<VarLayout> memberLayout;
        if( containerLayout->mapVarToLayout.TryGetValue(member.Ptr(), memberLayout) )
        {
            EmitDeclUsingLayout(context, member, memberLayout);
        }
        else
        {
            // No layout for this decl
            EmitDecl(context, member);
        }
    }
}

static void EmitTypeDefDecl(EmitContext* context, RefPtr<TypeDefDecl> decl)
{
    // TODO(tfoley): check if current compilation target even supports typedefs

    Emit(context, "typedef ");
    EmitType(context, decl->Type, decl->Name.Content);
    Emit(context, ";\n");
}

static void EmitStructDecl(EmitContext* context, RefPtr<StructSyntaxNode> decl)
{
    // Don't emit a declaration that was only generated implicitly, for
    // the purposes of semantic checking.
    if(decl->HasModifier<ImplicitParameterBlockElementTypeModifier>())
        return;

    Emit(context, "struct ");
    Emit(context, decl->Name.Content);
    Emit(context, "\n{\n");

    // TODO(tfoley): Need to hoist members functions, etc. out to global scope
    EmitDeclsInContainer(context, decl);

    Emit(context, "};\n");
}

// Shared emit logic for variable declarations (used for parameters, locals, globals, fields)
static void EmitVarDeclCommon(EmitContext* context, VarDeclBaseRef declRef)
{
    EmitModifiers(context, declRef.GetDecl());

    EmitType(context, declRef.GetType(), declRef.GetName());

    EmitSemantics(context, declRef.GetDecl());

    // TODO(tfoley): technically have to apply substitution here too...
    if (auto initExpr = declRef.GetDecl()->Expr)
    {
        Emit(context, " = ");
        EmitExpr(context, initExpr);
    }
}

// Shared emit logic for variable declarations (used for parameters, locals, globals, fields)
static void EmitVarDeclCommon(EmitContext* context, RefPtr<VarDeclBase> decl)
{
    EmitVarDeclCommon(context, DeclRef(decl.Ptr(), nullptr).As<VarDeclBaseRef>());
}

// Emit a single `regsiter` semantic, as appropriate for a given resource-type-specific layout info
static void emitHLSLRegisterSemantic(
    EmitContext*                    context,
    VarLayout::ResourceInfo const&  info)
{
    if( info.kind == LayoutResourceKind::Uniform )
    {
        size_t offset = info.index;

        // The HLSL `c` register space is logically grouped in 16-byte registers,
        // while we try to traffic in byte offsets. That means we need to pick
        // a register number, based on the starting offset in 16-byte register
        // units, and then a "component" within that register, based on 4-byte
        // offsets from there. We cannot support more fine-grained offsets than that.

        Emit(context, ": packoffset(c");

        // Size of a logical `c` register in bytes
        auto registerSize = 16;

        // Size of each component of a logical `c` register, in bytes
        auto componentSize = 4;

        size_t startRegister = offset / registerSize;
        Emit(context, int(startRegister));

        size_t byteOffsetInRegister = offset % registerSize;

        // If this field doesn't start on an even register boundary,
        // then we need to emit additional information to pick the
        // right component to start from
        if (byteOffsetInRegister != 0)
        {
            // The value had better occupy a whole number of components.
            assert(byteOffsetInRegister % componentSize == 0);

            size_t startComponent = byteOffsetInRegister / componentSize;

            static const char* kComponentNames[] = {"x", "y", "z", "w"};
            Emit(context, ".");
            Emit(context, kComponentNames[startComponent]);
        }
        Emit(context, ")");
    }
    else
    {
        Emit(context, ": register(");
        switch( info.kind )
        {
        case LayoutResourceKind::ConstantBuffer:
            Emit(context, "b");
            break;
        case LayoutResourceKind::ShaderResource:
            Emit(context, "t");
            break;
        case LayoutResourceKind::UnorderedAccess:
            Emit(context, "u");
            break;
        case LayoutResourceKind::SamplerState:
            Emit(context, "s");
            break;
        default:
            assert(!"unexpected");
            break;
        }
        Emit(context, info.index);
        if(info.space)
        {
            Emit(context, ", space");
            Emit(context, info.space);
        }
        Emit(context, ")");
    }
}

// Emit all the `register` semantics that are appropriate for a particular variable layout
static void emitHLSLRegisterSemantics(
    EmitContext*        context,
    RefPtr<VarLayout>   layout)
{
    if (!layout) return;

    switch( context->target )
    {
    default:
        return;

    case CodeGenTarget::HLSL:
        break;
    }

    for( auto rr : layout->resourceInfos )
    {
        emitHLSLRegisterSemantic(context, rr);
    }
}

static void emitHLSLParameterBlockDecl(
    EmitContext*                    context,
    RefPtr<VarDeclBase>             varDecl,
    RefPtr<ParameterBlockType>      parameterBlockType,
    RefPtr<VarLayout>               layout)
{
    // The data type that describes where stuff in the constant buffer should go
    RefPtr<ExpressionType> dataType = parameterBlockType->elementType;

    // We expect/require the data type to be a user-defined `struct` type
    auto declRefType = dataType->As<DeclRefType>();
    assert(declRefType);

    // We expect to always have layout information
    assert(layout);

    // We expect the layout to be for a structured type...
    RefPtr<ParameterBlockTypeLayout> bufferLayout = layout->typeLayout.As<ParameterBlockTypeLayout>();
    assert(bufferLayout);

    RefPtr<StructTypeLayout> structTypeLayout = bufferLayout->elementTypeLayout.As<StructTypeLayout>();
    assert(structTypeLayout);

    if( auto constantBufferType = parameterBlockType->As<ConstantBufferType>() )
    {
        Emit(context, "cbuffer ");
    }
    else if( auto textureBufferType = parameterBlockType->As<TextureBufferType>() )
    {
        Emit(context, "tbuffer ");
    }

    if( auto reflectionNameModifier = varDecl->FindModifier<ParameterBlockReflectionName>() )
    {
        Emit(context, " ");
        Emit(context, reflectionNameModifier->nameToken.Content);
    }

    EmitSemantics(context, varDecl, kESemanticMask_None);

    auto info = layout->FindResourceInfo(LayoutResourceKind::ConstantBuffer);
    assert(info);
    emitHLSLRegisterSemantic(context, *info);

    Emit(context, "\n{\n");
    if (auto structRef = declRefType->declRef.As<StructDeclRef>())
    {
        for (auto field : structRef.GetMembersOfType<FieldDeclRef>())
        {
            EmitVarDeclCommon(context, field);

            RefPtr<VarLayout> fieldLayout;
            structTypeLayout->mapVarToLayout.TryGetValue(field.GetDecl(), fieldLayout);
            assert(fieldLayout);

            // Emit explicit layout annotations for every field
            for( auto rr : fieldLayout->resourceInfos )
            {
                auto kind = rr.kind;

                auto offsetResource = rr;

                if(kind != LayoutResourceKind::Uniform)
                {
                    // Add the base index from the cbuffer into the index of the field
                    //
                    // TODO(tfoley): consider maybe not doing this, since it actually
                    // complicates logic around constant buffers...

                    // If the member of the cbuffer uses a resource, it had better
                    // appear as part of the cubffer layout as well.
                    auto cbufferResource = layout->FindResourceInfo(kind);
                    assert(cbufferResource);

                    offsetResource.index += cbufferResource->index;
                    offsetResource.space += cbufferResource->space;
                }

                emitHLSLRegisterSemantic(context, offsetResource);
            }

            Emit(context, ";\n");
        }
    }
    Emit(context, "}\n");
}

static void
emitGLSLLayoutQualifier(
    EmitContext*                    context,
    VarLayout::ResourceInfo const&  info)
{
    switch(info.kind)
    {
    case LayoutResourceKind::Uniform:
        Emit(context, "layout(offset = ");
        Emit(context, info.index);
        Emit(context, ")\n");
        break;

    case LayoutResourceKind::VertexInput:
    case LayoutResourceKind::FragmentOutput:
        Emit(context, "layout(location = ");
        Emit(context, info.index);
        Emit(context, ")\n");
        break;

    case LayoutResourceKind::SpecializationConstant:
        Emit(context, "layout(constant_id = ");
        Emit(context, info.index);
        Emit(context, ")\n");
        break;

    case LayoutResourceKind::ConstantBuffer:
    case LayoutResourceKind::ShaderResource:
    case LayoutResourceKind::UnorderedAccess:
    case LayoutResourceKind::SamplerState:
    case LayoutResourceKind::DescriptorTableSlot:
        Emit(context, "layout(binding = ");
        Emit(context, info.index);
        if(info.space)
        {
            Emit(context, ", set = ");
            Emit(context, info.space);
        }
        Emit(context, ")\n");
        break;
    }
}

static void
emitGLSLLayoutQualifiers(
    EmitContext*                    context,
    RefPtr<VarLayout>               layout)
{
    if(!layout) return;

    switch( context->target )
    {
    default:
        return;

    case CodeGenTarget::GLSL:
        break;
    }

    for( auto info : layout->resourceInfos )
    {
        emitGLSLLayoutQualifier(context, info);
    }
}

static void emitGLSLParameterBlockDecl(
    EmitContext*                    context,
    RefPtr<VarDeclBase>             varDecl,
    RefPtr<ParameterBlockType>      parameterBlockType,
    RefPtr<VarLayout>               layout)
{
    // The data type that describes where stuff in the constant buffer should go
    RefPtr<ExpressionType> dataType = parameterBlockType->elementType;

    // We expect/require the data type to be a user-defined `struct` type
    auto declRefType = dataType->As<DeclRefType>();
    assert(declRefType);

    // We expect to always have layout information
    assert(layout);

    // We expect the layout to be for a structured type...
    RefPtr<ParameterBlockTypeLayout> bufferLayout = layout->typeLayout.As<ParameterBlockTypeLayout>();
    assert(bufferLayout);

    RefPtr<StructTypeLayout> structTypeLayout = bufferLayout->elementTypeLayout.As<StructTypeLayout>();
    assert(structTypeLayout);

    // TODO(tfoley): emit GLSL layout info
//    emitHLSLRegisterSemantic(context, *info);

    emitGLSLLayoutQualifiers(context, layout);

    EmitModifiers(context, varDecl);

//    EmitSemantics(context, varDecl, kESemanticMask_None);

    if( auto reflectionNameModifier = varDecl->FindModifier<ParameterBlockReflectionName>() )
    {
        Emit(context, " ");
        Emit(context, reflectionNameModifier->nameToken.Content);
    }

    Emit(context, "\n{\n");
    if (auto structRef = declRefType->declRef.As<StructDeclRef>())
    {
        for (auto field : structRef.GetMembersOfType<FieldDeclRef>())
        {
            RefPtr<VarLayout> fieldLayout;
            structTypeLayout->mapVarToLayout.TryGetValue(field.GetDecl(), fieldLayout);
            assert(fieldLayout);

            // TODO(tfoley): We may want to emit *some* of these,
            // some of the time...
//            emitGLSLLayoutQualifiers(context, fieldLayout);

            EmitVarDeclCommon(context, field);

            Emit(context, ";\n");
        }
    }
    Emit(context, "}");

    if( varDecl->Name.Type != TokenType::Unknown )
    {
        Emit(context, " ");
        Emit(context, varDecl->Name.Content);
    }

    Emit(context, ";\n");
}

static void emitParameterBlockDecl(
    EmitContext*				context,
    RefPtr<VarDeclBase>			varDecl,
    RefPtr<ParameterBlockType>  parameterBlockType,
    RefPtr<VarLayout>           layout)
{
    switch(context->target)
    {
    case CodeGenTarget::HLSL:
        emitHLSLParameterBlockDecl(context, varDecl, parameterBlockType, layout);
        break;

    case CodeGenTarget::GLSL:
        emitGLSLParameterBlockDecl(context, varDecl, parameterBlockType, layout);
        break;

    default:
        assert(!"unexpected");
        break;
    }
}

static void EmitVarDecl(EmitContext* context, RefPtr<VarDeclBase> decl, RefPtr<VarLayout> layout)
{
    // As a special case, a variable using a parameter block type
    // will be translated into a declaration using the more primitive
    // language syntax.
    //
    // TODO(tfoley): Be sure to unwrap arrays here, in the GLSL case.
    //
    // TODO(tfoley): Detect cases where we need to fall back to
    // ordinary variable declaration syntax in HLSL.
    //
    // TODO(tfoley): there might be a better way to detect this, e.g.,
    // with an attribute that gets attached to the variable declaration.
    if (auto parameterBlockType = decl->Type->As<ParameterBlockType>())
    {
        emitParameterBlockDecl(context, decl, parameterBlockType, layout);
        return;
    }

    emitGLSLLayoutQualifiers(context, layout);

    EmitVarDeclCommon(context, decl);

    emitHLSLRegisterSemantics(context, layout);

    Emit(context, ";\n");
}

static void EmitParamDecl(EmitContext* context, RefPtr<ParameterSyntaxNode> decl)
{
    EmitVarDeclCommon(context, decl);
}

static void EmitFuncDecl(EmitContext* context, RefPtr<FunctionSyntaxNode> decl)
{
    EmitModifiers(context, decl);

    // TODO: if a function returns an array type, or something similar that
    // isn't allowed by declarator syntax and/or language rules, we could
    // hypothetically wrap things in a `typedef` and work around it.

    EmitType(context, decl->ReturnType, decl->Name.Content);

    Emit(context, "(");
    bool first = true;
    for (auto paramDecl : decl->GetMembersOfType<ParameterSyntaxNode>())
    {
        if (!first) Emit(context, ", ");
        EmitParamDecl(context, paramDecl);
        first = false;
    }
    Emit(context, ")");

    EmitSemantics(context, decl);

    if (auto bodyStmt = decl->Body)
    {
        EmitBlockStmt(context, bodyStmt);
    }
    else
    {
        Emit(context, ";\n");
    }
}

static void emitGLSLPreprocessorDirective(
    EmitContext*                        context,
    RefPtr<GLSLPreprocessorDirective>   directive)
{
    switch(context->target)
    {
    // Don't emit this stuff unless we are targetting GLSL
    default:
        return;

    case CodeGenTarget::GLSL:
        break;
    }

    if( auto versionDirective = directive.As<GLSLVersionDirective>() )
    {
        // TODO(tfoley): Emit an appropriate `#line` directive...

        Emit(context, "#version ");
        Emit(context, versionDirective->versionNumberToken.Content);
        if(versionDirective->glslProfileToken.Type != TokenType::Unknown)
        {
            Emit(context, " ");
            Emit(context, versionDirective->glslProfileToken.Content);
        }
        Emit(context, "\n");
    }
    else if( auto extensionDirective = directive.As<GLSLExtensionDirective>() )
    {
        // TODO(tfoley): Emit an appropriate `#line` directive...

        Emit(context, "#extension ");
        Emit(context, extensionDirective->extensionNameToken.Content);
        Emit(context, " : ");
        Emit(context, extensionDirective->dispositionToken.Content);
        Emit(context, "\n");
    }
    // TODO: handle other cases...
    else
    {
    }

    // TODO(tfoley): We really need to emit a `#version` directive unconditionally,
    // and we might need to emit `#extension` during cross-compilation, if we
    // are making use of extended functionality...
}

static void EmitProgram(
    EmitContext*                context,
    RefPtr<ProgramSyntaxNode>   program,
    RefPtr<ProgramLayout>       programLayout)
{
    // There may be global-scope modifiers that we should emit now
    for(auto modifier : program->GetModifiersOfType<GLSLPreprocessorDirective>())
    {
        emitGLSLPreprocessorDirective(context, modifier);
    }

    switch(context->target)
    {
    case CodeGenTarget::GLSL:
        {
            Emit(context, "#extension GL_GOOGLE_cpp_style_line_directive : require\n");
        }
        break;

    default:
        break;
    }


    // Layout information for the global scope is either an ordinary
    // `struct` in the common case, or a constant buffer in the case
    // where there were global-scope uniforms.
    auto globalScopeLayout = programLayout->globalScopeLayout;
    if( auto globalStructLayout = globalScopeLayout.As<StructTypeLayout>() )
    {
        // The `struct` case is easy enough to handle: we just
        // emit all the declarations directly, using their layout
        // information as a guideline.
        EmitDeclsInContainerUsingLayout(context, program, globalStructLayout);
    }
    else if(auto globalConstantBufferLayout = globalScopeLayout.As<ParameterBlockTypeLayout>())
    {
        // TODO: the `cbuffer` case really needs to be emitted very
        // carefully, but that is beyond the scope of what a simple rewriter
        // can easily do (without semantic analysis, etc.).
        //
        // The crux of the problem is that we need to collect all the
        // global-scope uniforms (but not declarations that don't involve
        // uniform storage...) and put them in a single `cbuffer` declaration,
        // so that we can give it an explicit location. The fields in that
        // declaration might use various type declarations, so we'd really
        // need to emit all the type declarations first, and that involves
        // some large scale reorderings.
        //
        // For now we will punt and just emit the declarations normally,
        // and hope that the global-scope block (`$Globals`) gets auto-assigned
        // the same location that we manually asigned it.

        auto elementTypeLayout = globalConstantBufferLayout->elementTypeLayout;
        auto elementTypeStructLayout = elementTypeLayout.As<StructTypeLayout>();

        // We expect all constant buffers to contain `struct` types for now
        assert(elementTypeStructLayout);

        EmitDeclsInContainerUsingLayout(
            context,
            program,
            elementTypeStructLayout);
    }
    else
    {
        assert(!"unexpected");
    }
}

static void EmitDeclImpl(EmitContext* context, RefPtr<Decl> decl, RefPtr<VarLayout> layout)
{
    // Don't emit code for declarations that came from the stdlib.
    //
    // TODO(tfoley): We probably need to relax this eventually,
    // since different targets might have different sets of builtins.
    if (decl->HasModifier<FromStdLibModifier>())
        return;

    if (auto typeDefDecl = decl.As<TypeDefDecl>())
    {
        EmitTypeDefDecl(context, typeDefDecl);
        return;
    }
    else if (auto structDecl = decl.As<StructSyntaxNode>())
    {
        EmitStructDecl(context, structDecl);
        return;
    }
    else if (auto varDecl = decl.As<VarDeclBase>())
    {
        EmitVarDecl(context, varDecl, layout);
        return;
    }
    else if (auto funcDecl = decl.As<FunctionSyntaxNode>())
    {
        EmitFuncDecl(context, funcDecl);
        return;
    }
    else if (auto genericDecl = decl.As<GenericDecl>())
    {
        // Don't emit generic decls directly; we will only
        // ever emit particular instantiations of them.
        return;
    }
	else if (auto classDecl = decl.As<ClassSyntaxNode>())
	{
		return;
	}
    else if( auto emptyDecl = decl.As<EmptyDecl>() )
    {
        EmitModifiers(context, emptyDecl);
        Emit(context, ";\n");
        return;
    }
    throw "unimplemented";
}

static void EmitDecl(EmitContext* context, RefPtr<Decl> decl)
{
    EmitDeclImpl(context, decl, nullptr);
}

static void EmitDeclUsingLayout(EmitContext* context, RefPtr<Decl> decl, RefPtr<VarLayout> layout)
{
    EmitDeclImpl(context, decl, layout);
}

static void EmitDecl(EmitContext* context, RefPtr<DeclBase> declBase)
{
    if( auto decl = declBase.As<Decl>() )
    {
        EmitDecl(context, decl);
    }
    else if(auto declGroup = declBase.As<DeclGroup>())
    {
        for(auto d : declGroup->decls)
            EmitDecl(context, d);
    }
    else
    {
        throw "unimplemented";
    }
}

String emitProgram(
    ProgramSyntaxNode*  program,
    ProgramLayout*      programLayout,
    CodeGenTarget       target)
{
    // TODO(tfoley): only emit symbols on-demand, as needed by a particular entry point

    EmitContext context;
    context.target = target;

    EmitProgram(&context, program, programLayout);

    String code = context.sb.ProduceString();

    return code;

#if 0
    // HACK(tfoley): Invoke the D3D HLSL compiler on the result, to validate it

#ifdef _WIN32
    {
        HMODULE d3dCompiler = LoadLibraryA("d3dcompiler_47");
        assert(d3dCompiler);

        pD3DCompile D3DCompile_ = (pD3DCompile)GetProcAddress(d3dCompiler, "D3DCompile");
        assert(D3DCompile_);

        ID3DBlob* codeBlob;
        ID3DBlob* diagnosticsBlob;
        HRESULT hr = D3DCompile_(
            code.begin(),
            code.Length(),
            "spire",
            nullptr,
            nullptr,
            "main",
            "ps_5_0",
            0,
            0,
            &codeBlob,
            &diagnosticsBlob);
        if (codeBlob) codeBlob->Release();
        if (diagnosticsBlob)
        {
            String diagnostics = (char const*) diagnosticsBlob->GetBufferPointer();
            fprintf(stderr, "%s", diagnostics.begin());
            OutputDebugStringA(diagnostics.begin());
            diagnosticsBlob->Release();
        }
        if (FAILED(hr))
        {
            int f = 9;
        }
    }

    #include <d3dcompiler.h>
#endif
#endif

}


}} // Spire::Compiler
