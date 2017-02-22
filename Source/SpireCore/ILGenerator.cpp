#include "Syntax.h"
#include "ScopeDictionary.h"
#include "CodeWriter.h"
#include "Naming.h"

namespace Spire
{
    namespace Compiler
    {
        class ILGenerator : public SyntaxVisitor
        {
        public:
            ILProgram * program = nullptr;
            ILGenerator(ILProgram * result, DiagnosticSink * sink)
                : SyntaxVisitor(sink)
            {
                program = result;
            }
        private:
            Dictionary<DeclRef, RefPtr<ILStructType>> structTypes;
            Dictionary<DeclRef, ILFunction*> functions;
            ScopeDictionary<String, ILOperand*> variables;
            CodeWriter codeWriter;
        private:
            RefPtr<ILStructType> TranslateStructType(AggTypeDecl* structDecl)
            {
                RefPtr<ILStructType> ilStructType;

                if (structTypes.TryGetValue(structDecl, ilStructType))
                {
                    return ilStructType;
                }

                ilStructType = new ILStructType();
                ilStructType->TypeName = structDecl->Name.Content;

                for (auto field : structDecl->GetMembersOfType<Variable>())
                {
                    ILStructType::ILStructField ilField;
                    ilField.FieldName = field->Name.Content;
                    ilField.Type = TranslateExpressionType(field->Type.Ptr());
                    ilStructType->Members.Add(ilField);
                }

                structTypes.Add(DeclRef(structDecl, nullptr), ilStructType);
                return ilStructType;
            }

            int GetIntVal(RefPtr<IntVal> val)
            {
                if (auto constantVal = val.As<ConstantIntVal>())
                {
                    return constantVal->value;
                }
                assert(!"unexpected");
                return 0;
            }

            RefPtr<ILType> TranslateExpressionType(ExpressionType * type)
            {
                if (auto basicType = type->AsBasicType())
                {
                    auto base = new ILBasicType();
                    base->Type = (ILBaseType)basicType->BaseType;
                    return base;
                }
                else if (auto vecType = type->AsVectorType())
                {
                    auto elementType = vecType->elementType->AsBasicType();
                    int elementCount = GetIntVal(vecType->elementCount);
                    assert(elementType);
                    return new ILVectorType((ILBaseType)elementType->BaseType, elementCount);
                }
                else if (auto matType = type->AsMatrixType())
                {
                    auto elementType = matType->elementType->AsBasicType();
                    int rowCount = GetIntVal(matType->rowCount);
                    int colCount = GetIntVal(matType->colCount);
                    assert(elementType);
                    return new ILMatrixType((ILBaseType)elementType->BaseType, rowCount, colCount);
                }
                else if (auto texType = type->As<TextureType>())
                {
                    return new ILTextureType(TranslateExpressionType(texType->elementType.Ptr()),
                        (ILTextureShape)texType->GetBaseShape(),
                        texType->isMultisample(),
                        texType->IsArray(),
                        texType->isShadow());
                }
                else if (auto cbufferType = type->As<ConstantBufferType>())
                {
                    auto ilType = new ILPointerLikeType(ILPointerLikeTypeName::ConstantBuffer, TranslateExpressionType(cbufferType->elementType.Ptr()));
                    return ilType;
                }
                else if (auto declRefType = type->AsDeclRefType())
                {
                    auto decl = declRefType->declRef.decl;
                    if (auto structDecl = dynamic_cast<StructSyntaxNode*>(decl))
                    {
                        return TranslateStructType(structDecl);
                    }
                    else
                    {
                        throw NotImplementedException("decl type");
                    }
                }
                else if (auto arrType = type->AsArrayType())
                {
                    auto nArrType = new ILArrayType();
                    nArrType->BaseType = TranslateExpressionType(arrType->BaseType.Ptr());
                    nArrType->ArrayLength = arrType->ArrayLength ? GetIntVal(arrType->ArrayLength) : 0;
                    return nArrType;
                }
                throw NotImplementedException("decl type");
            }

            RefPtr<ILType> TranslateExpressionType(const RefPtr<ExpressionType> & type)
            {
                return TranslateExpressionType(type.Ptr());
            }

            ParameterQualifier GetParamDirectionQualifier(ParameterSyntaxNode* paramDecl)
            {
                if (paramDecl->HasModifier<InOutModifier>())
                    return ParameterQualifier::InOut;
                else if (paramDecl->HasModifier<OutModifier>())
                    return ParameterQualifier::Out;
                else
                    return ParameterQualifier::In;
            }
            
            FetchArgInstruction * thisArg = nullptr;
            DeclRef thisDeclRef;
            void GenerateFunctionHeader(FunctionSyntaxNode * f, ILStructType * thisType)
            {
                RefPtr<ILFunction> func = new ILFunction();
                StringBuilder internalName;
                if (thisType)
                    internalName << thisType->TypeName << "@";
                internalName << f->Name.Content;
                for (auto & para : f->GetParameters())
                {
                    internalName << "@" << para->Type.type->ToString();
                }
                f->InternalName = internalName.ProduceString();
                program->Functions.Add(f->InternalName, func);
                func->Name = thisType->TypeName + "@" + f->InternalName;
                func->ReturnType = TranslateExpressionType(f->ReturnType);
                
                functions[DeclRef(f, nullptr)] = func.Ptr();
            }
            void GenerateFunction(FunctionSyntaxNode * f, ILStructType * thisType)
            {
                RefPtr<ILFunction> func = functions[DeclRef(f, nullptr)]();
                variables.PushScope();
                codeWriter.PushNode();
                int id = 0;
                if (thisType)
                {
                    thisArg = codeWriter.FetchArg(thisType, ++id, ParameterQualifier::InOut);
                    func->Parameters.Add("this", thisArg);
                    thisArg->Name = "sv_this";
                    variables.Add("this", thisArg);
                }
                for (auto &param : f->GetParameters())
                {
                    auto op = codeWriter.FetchArg(TranslateExpressionType(param->Type.Ptr()), ++id, GetParamDirectionQualifier(param.Ptr()));
                    func->Parameters.Add(param->Name.Content, op);
                    op->Name = EscapeCodeName(String("p_") + param->Name.Content);
                    variables.Add(param->Name.Content, op);
                }
                f->Body->Accept(this);
                func->Code = codeWriter.PopNode();
                variables.PopScope();
                thisArg = nullptr;
            }
            void GenerateMemberFunctionHeader(ClassSyntaxNode * node)
            {
                thisDeclRef = DeclRef(node, nullptr);
                auto thisType = structTypes[thisDeclRef]();
                for (auto && f : node->GetMembersOfType<FunctionSyntaxNode>())
                {
                    GenerateFunctionHeader(f.Ptr(), thisType.Ptr());
                }
                thisDeclRef = DeclRef();
            }
            void GenerateMemberFunction(ClassSyntaxNode * node)
            {
                thisDeclRef = DeclRef(node, nullptr);
                auto thisType = structTypes[thisDeclRef]();
                for (auto && f : node->GetMembersOfType<FunctionSyntaxNode>())
                {
                    GenerateFunction(f.Ptr(), thisType.Ptr());
                }
                thisDeclRef = DeclRef();
            }
        public:
            virtual RefPtr<ProgramSyntaxNode> VisitProgram(ProgramSyntaxNode * prog) override
            {
                for (auto&& s : prog->GetStructs())
                {
                    if (s->HasModifier<IntrinsicModifier>() || s->HasModifier<FromStdLibModifier>())
                        continue;
                    s->Accept(this);
                }
                auto classes = prog->GetMembersOfType<ClassSyntaxNode>();
                for (auto&& c : classes)
                {
                    TranslateStructType(c.Ptr());
                }
                for (auto&& c : classes)
                {
                    GenerateMemberFunctionHeader(c.Ptr());
                }
                variables.PushScope();
                for (auto&& v : prog->GetMembersOfType<Variable>())
                {
                    if (v->HasModifier<IntrinsicModifier>() || v->HasModifier<FromStdLibModifier>())
                        continue;
                    v->Accept(this);
                }
                for (auto&& f : prog->GetFunctions())
                {
                    if (f->HasModifier<IntrinsicModifier>() || f->HasModifier<FromStdLibModifier>())
                        continue;
                    GenerateFunctionHeader(f.Ptr(), nullptr);
                }

                for (auto&& c : classes)
                {
                    GenerateMemberFunction(c.Ptr());
                }
                for (auto&& f : prog->GetFunctions())
                {
                    if (f->HasModifier<IntrinsicModifier>() || f->HasModifier<FromStdLibModifier>())
                        continue;
                    GenerateFunction(f.Ptr(), nullptr);
                }
                variables.PopScope();
                return prog;
            }
            virtual RefPtr<StructSyntaxNode> VisitStruct(StructSyntaxNode * st) override
            {
                RefPtr<ILStructType> structType = TranslateStructType(st);
                program->Structs.Add(structType);
                return st;
            }
            virtual RefPtr<FunctionSyntaxNode> VisitFunction(FunctionSyntaxNode* function) override
            {
                if (function->IsExtern())
                    return function;
                GenerateFunction(function, nullptr);
                return function;
            }

        public:
            // functions for emiting code body (statements and expressions)
            ILOperand * exprStack = nullptr;
            ILOperand * returnRegister = nullptr;
            void PushStack(ILOperand * op)
            {
                assert(exprStack == nullptr);
                exprStack = op;
            }
            ILOperand * PopStack()
            {
                auto rs = exprStack;
                exprStack = nullptr;
                return rs;
            }
            virtual RefPtr<StatementSyntaxNode> VisitBlockStatement(BlockStatementSyntaxNode* stmt) override
            {
                variables.PushScope();
                for (auto & subStmt : stmt->Statements)
                    subStmt->Accept(this);
                variables.PopScope();
                return stmt;
            }
            virtual RefPtr<StatementSyntaxNode> VisitWhileStatement(WhileStatementSyntaxNode* stmt) override
            {
                RefPtr<WhileInstruction> instr = new WhileInstruction();
                variables.PushScope();
                codeWriter.PushNode();
                stmt->Predicate->Accept(this);
                codeWriter.Insert(new ReturnInstruction(PopStack()));
                instr->ConditionCode = codeWriter.PopNode();
                codeWriter.PushNode();
                stmt->Statement->Accept(this);
                instr->BodyCode = codeWriter.PopNode();
                codeWriter.Insert(instr.Release());
                variables.PopScope();
                return stmt;
            }
            virtual RefPtr<StatementSyntaxNode> VisitDoWhileStatement(DoWhileStatementSyntaxNode* stmt) override
            {
                RefPtr<DoInstruction> instr = new DoInstruction();
                variables.PushScope();
                codeWriter.PushNode();
                stmt->Predicate->Accept(this);
                codeWriter.Insert(new ReturnInstruction(PopStack()));
                instr->ConditionCode = codeWriter.PopNode();
                codeWriter.PushNode();
                stmt->Statement->Accept(this);
                instr->BodyCode = codeWriter.PopNode();
                codeWriter.Insert(instr.Release());
                variables.PopScope();
                return stmt;
            }
            virtual RefPtr<StatementSyntaxNode> VisitForStatement(ForStatementSyntaxNode* stmt) override
            {
                RefPtr<ForInstruction> instr = new ForInstruction();
                variables.PushScope();
                if (auto initStmt = stmt->InitialStatement.Ptr())
                {
                    // TODO(tfoley): any of this push-pop malarky needed here?
                    initStmt->Accept(this);
                }
                if (stmt->PredicateExpression)
                {
                    codeWriter.PushNode();
                    stmt->PredicateExpression->Accept(this);
                    PopStack();
                    instr->ConditionCode = codeWriter.PopNode();
                }

                if (stmt->SideEffectExpression)
                {
                    codeWriter.PushNode();
                    stmt->SideEffectExpression->Accept(this);
                    PopStack();
                    instr->SideEffectCode = codeWriter.PopNode();
                }

                codeWriter.PushNode();
                stmt->Statement->Accept(this);
                instr->BodyCode = codeWriter.PopNode();
                codeWriter.Insert(instr.Release());
                variables.PopScope();
                return stmt;
            }
            virtual RefPtr<StatementSyntaxNode> VisitIfStatement(IfStatementSyntaxNode* stmt) override
            {
                RefPtr<IfInstruction> instr = new IfInstruction();
                variables.PushScope();
                stmt->Predicate->Accept(this);
                instr->Operand = PopStack();
                codeWriter.PushNode();
                stmt->PositiveStatement->Accept(this);
                instr->TrueCode = codeWriter.PopNode();
                if (stmt->NegativeStatement)
                {
                    codeWriter.PushNode();
                    stmt->NegativeStatement->Accept(this);
                    instr->FalseCode = codeWriter.PopNode();
                }
                codeWriter.Insert(instr.Release());
                variables.PopScope();
                return stmt;
            }
            virtual RefPtr<StatementSyntaxNode> VisitReturnStatement(ReturnStatementSyntaxNode* stmt) override
            {
                returnRegister = nullptr;
                if (stmt->Expression)
                {
                    stmt->Expression->Accept(this);
                    returnRegister = PopStack();
                }
                codeWriter.Insert(new ReturnInstruction(returnRegister));
                return stmt;
            }
            virtual RefPtr<StatementSyntaxNode> VisitBreakStatement(BreakStatementSyntaxNode* stmt) override
            {
                codeWriter.Insert(new BreakInstruction());
                return stmt;
            }
            virtual RefPtr<StatementSyntaxNode> VisitContinueStatement(ContinueStatementSyntaxNode* stmt) override
            {
                codeWriter.Insert(new ContinueInstruction());
                return stmt;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitSelectExpression(SelectExpressionSyntaxNode * expr) override
            {
                expr->SelectorExpr->Accept(this);
                auto predOp = PopStack();
                expr->Expr0->Accept(this);
                auto v0 = PopStack();
                expr->Expr1->Accept(this);
                auto v1 = PopStack();
                PushStack(codeWriter.Select(predOp, v0, v1));
                return expr;
            }
            ILOperand * EnsureBoolType(ILOperand * op, RefPtr<ExpressionType> type)
            {
                if (!type->Equals(ExpressionType::Bool.Ptr()))
                {
                    auto cmpeq = new CmpneqInstruction();
                    cmpeq->Operands[0] = op;
                    cmpeq->Operands[1] = program->ConstantPool->CreateConstant(0);
                    cmpeq->Type = new ILBasicType(ILBaseType::Int);
                    codeWriter.Insert(cmpeq);
                    return cmpeq;
                }
                else
                    return op;
            }
            virtual RefPtr<StatementSyntaxNode> VisitDiscardStatement(DiscardStatementSyntaxNode * stmt) override
            {
                codeWriter.Discard();
                return stmt;
            }
            AllocVarInstruction * AllocVar(String name, ExpressionType * etype, CodePosition pos)
            {
                AllocVarInstruction * varOp = 0;
                RefPtr<ILType> type = TranslateExpressionType(etype);
                assert(type);
                if (codeWriter.GetCurrentNode())
                    varOp = codeWriter.AllocVar(type);
                else
                {
                    auto gvar = new ILGlobalVariable(type);
                    gvar->IsConst = false;
                    varOp = gvar;
                    program->GlobalVars[name] = gvar;
                }
                varOp->Name = name;
                varOp->Position = pos;
                return varOp;
            }

            RefPtr<Variable> VisitDeclrVariable(Variable* varDecl)
            {
                AllocVarInstruction * varOp = AllocVar(EscapeCodeName(varDecl->Name.Content), varDecl->Type.Ptr(), varDecl->Position);
                variables.Add(varDecl->Name.Content, varOp);
                if (varDecl->Expr)
                {
                    varDecl->Expr->Accept(this);
                    Assign(varOp, PopStack());
                }
                return varDecl;
            }

            virtual RefPtr<StatementSyntaxNode> VisitExpressionStatement(ExpressionStatementSyntaxNode* stmt) override
            {
                stmt->Expression->Accept(this);
                PopStack();
                return stmt;
            }
            void Assign(ILOperand * left, ILOperand * right)
            {
                if (auto add = dynamic_cast<AddInstruction*>(left))
                {
                    auto baseOp = add->Operands[0].Ptr();
                    codeWriter.Update(baseOp, add->Operands[1].Ptr(), right);
                    add->Erase();
                }
                else if (auto swizzle = dynamic_cast<SwizzleInstruction*>(left))
                {
                    auto baseOp = swizzle->Operand.Ptr();
                    int index = 0;
                    for (int i = 0; i < swizzle->SwizzleString.Length(); i++)
                    {
                        switch (swizzle->SwizzleString[i])
                        {
                        case 'r':
                        case 'x':
                            index = 0;
                            break;
                        case 'g':
                        case 'y':
                            index = 1;
                            break;
                        case 'b':
                        case 'z':
                            index = 2;
                            break;
                        case 'a':
                        case 'w':
                            index = 3;
                            break;
                        }
                        codeWriter.Update(baseOp, program->ConstantPool->CreateConstant(index),
                            codeWriter.Retrieve(right, program->ConstantPool->CreateConstant(i)));
                    }
                    swizzle->Erase();
                }
                else
                    codeWriter.Store(left, right);
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitBinaryExpression(BinaryExpressionSyntaxNode* expr) override
            {
                expr->RightExpression->Accept(this);
                auto right = PopStack();
                if (expr->Operator == Operator::Assign)
                {
                    expr->LeftExpression->Access = ExpressionAccess::Write;
                    expr->LeftExpression->Accept(this);
                    auto left = PopStack();
                    Assign(left, right);
                    PushStack(left);
                }
                else
                {
                    expr->LeftExpression->Access = ExpressionAccess::Read;
                    expr->LeftExpression->Accept(this);
                    auto left = PopStack();
                    BinaryInstruction * rs = 0;
                    switch (expr->Operator)
                    {
                    case Operator::Add:
                    case Operator::AddAssign:
                        rs = new AddInstruction();
                        break;
                    case Operator::Sub:
                    case Operator::SubAssign:
                        rs = new SubInstruction();
                        break;
                    case Operator::Mul:
                    case Operator::MulAssign:
                        rs = new MulInstruction();
                        break;
                    case Operator::Mod:
                    case Operator::ModAssign:
                        rs = new ModInstruction();
                        break;
                    case Operator::Div:
                    case Operator::DivAssign:
                        rs = new DivInstruction();
                        break;
                    case Operator::And:
                        rs = new AndInstruction();
                        break;
                    case Operator::Or:
                        rs = new OrInstruction();
                        break;
                    case Operator::BitAnd:
                    case Operator::AndAssign:
                        rs = new BitAndInstruction();
                        break;
                    case Operator::BitOr:
                    case Operator::OrAssign:
                        rs = new BitOrInstruction();
                        break;
                    case Operator::BitXor:
                    case Operator::XorAssign:
                        rs = new BitXorInstruction();
                        break;
                    case Operator::Lsh:
                    case Operator::LshAssign:
                        rs = new ShlInstruction();
                        break;
                    case Operator::Rsh:
                    case Operator::RshAssign:
                        rs = new ShrInstruction();
                        break;
                    case Operator::Eql:
                        rs = new CmpeqlInstruction();
                        break;
                    case Operator::Neq:
                        rs = new CmpneqInstruction();
                        break;
                    case Operator::Greater:
                        rs = new CmpgtInstruction();
                        break;
                    case Operator::Geq:
                        rs = new CmpgeInstruction();
                        break;
                    case Operator::Leq:
                        rs = new CmpleInstruction();
                        break;
                    case Operator::Less:
                        rs = new CmpltInstruction();
                        break;
                    default:
                        throw NotImplementedException("Code gen not implemented for this operator.");
                    }
                    rs->Operands.SetSize(2);
                    rs->Operands[0] = left;
                    rs->Operands[1] = right;
                    rs->Type = TranslateExpressionType(expr->Type);
                    codeWriter.Insert(rs);
                    switch (expr->Operator)
                    {
                    case Operator::AddAssign:
                    case Operator::SubAssign:
                    case Operator::MulAssign:
                    case Operator::DivAssign:
                    case Operator::ModAssign:
                    case Operator::LshAssign:
                    case Operator::RshAssign:
                    case Operator::AndAssign:
                    case Operator::OrAssign:
                    case Operator::XorAssign:
                    {
                        expr->LeftExpression->Access = ExpressionAccess::Write;
                        expr->LeftExpression->Accept(this);
                        auto target = PopStack();
                        Assign(target, rs);
                        break;
                    }
                    default:
                        break;
                    }
                    PushStack(rs);
                }
                return expr;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitConstantExpression(ConstantExpressionSyntaxNode* expr) override
            {
                ILConstOperand * op;
                if (expr->ConstType == ConstantExpressionSyntaxNode::ConstantType::Float)
                {
                    op = program->ConstantPool->CreateConstant(expr->FloatValue);
                }
                else if (expr->ConstType == ConstantExpressionSyntaxNode::ConstantType::Bool)
                {
                    op = program->ConstantPool->CreateConstant(expr->IntValue != 0);
                }
                else
                {
                    op = program->ConstantPool->CreateConstant(expr->IntValue);
                }
                PushStack(op);
                return expr;
            }
            void GenerateIndexExpression(ILOperand * base, ILOperand * idx, bool read)
            {
                if (read)
                {
                    auto ldInstr = codeWriter.Retrieve(base, idx);
                    ldInstr->Attribute = base->Attribute;
                    PushStack(ldInstr);
                }
                else
                {
                    PushStack(codeWriter.Add(base, idx));
                }
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitIndexExpression(IndexExpressionSyntaxNode* expr) override
            {
                expr->BaseExpression->Access = expr->Access;
                expr->BaseExpression->Accept(this);
                auto base = PopStack();
                expr->IndexExpression->Access = ExpressionAccess::Read;
                expr->IndexExpression->Accept(this);
                auto idx = PopStack();
                GenerateIndexExpression(base, idx,
                    expr->Access == ExpressionAccess::Read);
                return expr;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitSwizzleExpression(SwizzleExpr * expr) override
            {
                RefPtr<Object> refObj;
                expr->base->Access = expr->Access;
                expr->base->Accept(this);
                auto base = PopStack();
                StringBuilder swizzleStr;
                for (int i = 0; i < expr->elementCount; i++)
                    swizzleStr << ('x' + i);
                auto rs = new SwizzleInstruction();
                rs->Type = TranslateExpressionType(expr->Type.Ptr());
                rs->SwizzleString = swizzleStr.ToString();
                rs->Operand = base;
                codeWriter.Insert(rs);
                PushStack(rs);
                return expr;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitMemberExpression(MemberExpressionSyntaxNode * expr) override
            {
                RefPtr<Object> refObj;
                expr->BaseExpression->Access = expr->Access;
                expr->BaseExpression->Accept(this);
                auto base = PopStack();
                if (auto declRefType = expr->BaseExpression->Type->AsDeclRefType())
                {
                    if (auto structDecl = declRefType->declRef.As<StructDeclRef>())
                    {
                        int id = structDecl.GetDecl()->FindFieldIndex(expr->MemberName);
                        GenerateIndexExpression(base, program->ConstantPool->CreateConstant(id),
                            expr->Access == ExpressionAccess::Read);
                    }
                    else
                        throw NotImplementedException("member expression codegen");
                }
                else
                    throw NotImplementedException("member expression codegen");
               
                return expr;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitInvokeExpression(InvokeExpressionSyntaxNode* expr) override
            {
                List<ILOperand*> args;
                bool hasSideEffect = false;
                for (auto arg : expr->Arguments)
                {
                    arg->Accept(this);
                    args.Add(PopStack());
                }
                if (auto funcType = expr->FunctionExpr->Type.Ptr()->As<FuncType>())
                {
                    auto instr = new CallInstruction(args.Count());
                    ILFunction * func = nullptr;
                    if (functions.TryGetValue(funcType->declRef, func))
                    {
                        auto rsType = funcType->declRef.GetResultType();
                        instr->Type = TranslateExpressionType(rsType);
                        instr->Function = func->Name;
                        instr->HasSideEffect = true;
                    }
                    // ad-hoc processing for ctor calls
                    else if (auto ctor = dynamic_cast<ConstructorDecl*>(funcType->declRef.GetDecl()))
                    {
                        RefPtr<ExpressionType> exprType = DeclRefType::Create(DeclRef(ctor->ParentDecl, funcType->declRef.substitutions));
                        auto rsType = TranslateExpressionType(exprType);
                        instr->Type = rsType;
                        instr->Function = "__init";
                    }
                    for (int i = 0; i < args.Count(); i++)
                        instr->Arguments[i] = args[i];
                    instr->Type = TranslateExpressionType(expr->Type);
                    codeWriter.Insert(instr);
                    PushStack(instr);
                }
                else
                    throw InvalidProgramException();
                return expr;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitTypeCastExpression(TypeCastExpressionSyntaxNode * expr) override
            {
                expr->Expression->Accept(this);
                auto base = PopStack();
                if (expr->Expression->Type->Equals(expr->Type))
                {
                    PushStack(base);
                }
                else if (expr->Expression->Type->Equals(ExpressionType::Float) &&
                    expr->Type->Equals(ExpressionType::Int))
                {
                    auto instr = new Float2IntInstruction(base);
                    codeWriter.Insert(instr);
                    PushStack(instr);
                }
                else if (expr->Expression->Type->Equals(ExpressionType::Int) &&
                    expr->Type->Equals(ExpressionType::Float))
                {
                    auto instr = new Int2FloatInstruction(base);
                    codeWriter.Insert(instr);
                    PushStack(instr);
                }
                else
                {
                    getSink()->diagnose(expr, Diagnostics::invalidTypeCast, expr->Expression->Type, expr->Type);
                }
                return expr;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitUnaryExpression(UnaryExpressionSyntaxNode* expr) override
            {
                if (expr->Operator == Operator::PostDec || expr->Operator == Operator::PostInc
                    || expr->Operator == Operator::PreDec || expr->Operator == Operator::PreInc)
                {
                    expr->Expression->Access = ExpressionAccess::Read;
                    expr->Expression->Accept(this);
                    auto base = PopStack();
                    BinaryInstruction * instr;
                    if (expr->Operator == Operator::PostDec)
                        instr = new SubInstruction();
                    else
                        instr = new AddInstruction();
                    instr->Operands.SetSize(2);
                    instr->Operands[0] = base;
                    if (expr->Type->Equals(ExpressionType::Float))
                        instr->Operands[1] = program->ConstantPool->CreateConstant(1.0f);
                    else
                        instr->Operands[1] = program->ConstantPool->CreateConstant(1);
                    instr->Type = TranslateExpressionType(expr->Type);
                    codeWriter.Insert(instr);

                    expr->Expression->Access = ExpressionAccess::Write;
                    expr->Expression->Accept(this);
                    auto dest = PopStack();
                    auto store = new StoreInstruction(dest, instr);
                    codeWriter.Insert(store);
                    PushStack(base);
                }
                else if (expr->Operator == Operator::PreDec || expr->Operator == Operator::PreInc)
                {
                    expr->Expression->Access = ExpressionAccess::Read;
                    expr->Expression->Accept(this);
                    auto base = PopStack();
                    BinaryInstruction * instr;
                    if (expr->Operator == Operator::PostDec)
                        instr = new SubInstruction();
                    else
                        instr = new AddInstruction();
                    instr->Operands.SetSize(2);
                    instr->Operands[0] = base;
                    if (expr->Type->Equals(ExpressionType::Float))
                        instr->Operands[1] = program->ConstantPool->CreateConstant(1.0f);
                    else
                        instr->Operands[1] = program->ConstantPool->CreateConstant(1);
                    instr->Type = TranslateExpressionType(expr->Type);
                    codeWriter.Insert(instr);

                    expr->Expression->Access = ExpressionAccess::Write;
                    expr->Expression->Accept(this);
                    auto dest = PopStack();
                    auto store = new StoreInstruction(dest, instr);
                    codeWriter.Insert(store);
                    PushStack(instr);
                }
                else
                {
                    expr->Expression->Accept(this);
                    auto base = PopStack();
                    auto genUnaryInstr = [&](ILOperand * input)
                    {
                        UnaryInstruction * rs = 0;
                        switch (expr->Operator)
                        {
                        case Operator::Not:
                            input = EnsureBoolType(input, expr->Expression->Type);
                            rs = new NotInstruction();
                            break;
                        case Operator::Neg:
                            rs = new NegInstruction();
                            break;
                        case Operator::BitNot:
                            rs = new BitNotInstruction();
                            break;
                        default:
                            throw NotImplementedException("Code gen is not implemented for this operator.");
                        }
                        rs->Operand = input;
                        rs->Type = input->Type;
                        codeWriter.Insert(rs);
                        return rs;
                    };
                    PushStack(genUnaryInstr(base));
                }
                return expr;
            }
            bool GenerateVarRef(String name, ExpressionAccess access)
            {
                ILOperand * var = 0;
                String srcName = name;
                if (!variables.TryGetValue(srcName, var))
                {
                    if (thisDeclRef)
                    {
                        int id = ((AggTypeDecl*)thisDeclRef.GetDecl())->FindFieldIndex(name);
                        GenerateIndexExpression(thisArg, program->ConstantPool->CreateConstant(id), access);
                        return true;
                    }
                    return false;
                }
                if (access == ExpressionAccess::Read)
                {
                    PushStack(var);
                }
                else
                {
                    PushStack(var);
                }
                return true;
            }
            virtual RefPtr<ExpressionSyntaxNode> VisitVarExpression(VarExpressionSyntaxNode* expr) override
            {
                RefPtr<Object> refObj;
                if (!GenerateVarRef(expr->Variable, expr->Access))
                {
                    throw InvalidProgramException("identifier is neither a variable nor a recognized component.");
                }
                return expr;
            }
        };

        SyntaxVisitor * CreateILCodeGenerator(DiagnosticSink * err, ILProgram * program)
        {
            return new ILGenerator(program, err);
        }
    }
}