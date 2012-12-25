//===--- SemaCLI.cpp - Semantic Analysis for C++/CLI-----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for C++/CLI language constructs.
//
//===----------------------------------------------------------------------===//

#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaCLI.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Scope.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclCLI.h"
#include "clang/AST/TypeCLI.h"
#include "clang/AST/CXXInheritance.h"
#include "llvm/ADT/StringExtras.h"
#include "TypeLocBuilder.h"

#using <System.dll>
using namespace System;
using namespace System::IO;
using namespace System::Collections::Generic;
using namespace System::Runtime::InteropServices;

#using <Mono.Cecil.dll>
using namespace Mono::Cecil;

#include "CLIInterop.h"
namespace clang {
using namespace sema;
using namespace clix;

static IdentifierInfo *getIdentifier(Sema &S, StringRef Name) {
  IdentifierTable& IdentTable = S.getPreprocessor().getIdentifierTable();
  return &IdentTable.get(Name);
}

static IdentifierInfo *getIdentifier(Sema &S, System::String ^Name) {
  std::string IdentName = marshalString<E_UTF8>(Name);
  return getIdentifier(S, IdentName);
}

static NamespaceDecl *findCreateNamespace(Sema &S, DeclContext *DC,
                                          StringRef Namespace) {
  IdentifierInfo *II = getIdentifier(S, Namespace);
  DeclContextLookupResult Res = DC->lookup(DeclarationName(II));

  for (; Res.first != Res.second; ++Res.first) {
    if (NamespaceDecl *ND = dyn_cast<NamespaceDecl>(*Res.first)) {
      return ND;
    }
  }

  NamespaceDecl *NS = NamespaceDecl::Create(S.getASTContext(), DC,
    /*IsInline=*/false, SourceLocation(), SourceLocation(), II, 0);
  DC->addDecl(NS);
  return NS;
}

static NamespaceDecl *findCreateNamespaces(Sema &S, StringRef Namespace) {
  TranslationUnitDecl* TU = S.getASTContext().getTranslationUnitDecl();
  DeclContext* DC = TU->getPrimaryContext();

  llvm::SmallVector<StringRef, 4> Namespaces;
  llvm::SplitString(Namespace, Namespaces, ".");

  NamespaceDecl* NS = 0;
  for (unsigned i = 0; i < Namespaces.size(); ++i) {
    NS = findCreateNamespace(S, DC, Namespaces[i]);
    assert(NS && "Expected a valid namespace");
    DC = NS->getPrimaryContext();
  }

  return NS;
}

static NamespaceDecl *findCreateNamespaces(Sema &S, TypeReference ^Type) {
  std::string Namespace = marshalString<E_UTF8>(Type->Namespace);
  return findCreateNamespaces(S, Namespace);
}

static NamespaceDecl* findCreateNamespaces(Sema &S, TypeDefinition ^Type) {
  std::string Namespace = marshalString<E_UTF8>(Type->Namespace);
  return findCreateNamespaces(S, Namespace);
}

static AccessSpecifier convertMethodAccess(MethodDefinition ^Method) {
  if (Method->IsPublic) return AS_public;
  else if (Method->IsAssembly) return AS_internal;
  else if (Method->IsFamily) return AS_protected;
  else if (Method->IsFamilyOrAssembly) return AS_protected_public;
  else if (Method->IsFamilyAndAssembly) return AS_protected_private;
  else return AS_private;
}

static AccessSpecifier convertFieldAccess(FieldDefinition ^Field) {
  if (Field->IsPublic) return AS_public;
  else if (Field->IsAssembly) return AS_internal;
  else if (Field->IsFamily) return AS_protected;
  else if (Field->IsFamilyOrAssembly) return AS_protected_public;
  else if (Field->IsFamilyAndAssembly) return AS_protected_private;
  else return AS_private;
}

static AccessSpecifier convertPropertyAccess(PropertyDefinition ^Prop) {
  return AS_public;
}

static bool findCreateType(Sema &S, TypeReference ^TypeRef, QualType &Type,
                           CXXRecordDecl *MethodRecord = 0);

typedef Mono::Collections::Generic::Collection<Mono::Cecil::CustomAttribute^>
  CustomAttributeCollection;

static void createDeclAttributes(Sema &S, CustomAttributeCollection ^Attributes,
                                 Decl *D);

static CXXRecordDecl *findCreateClassDecl(Sema &S, TypeDefinition ^TypeDef);

static bool hasCLIParamsAttribute(Sema &S, MethodDefinition ^Method) {
  for each (ParameterDefinition ^Param in Method->Parameters) {
    for each (CustomAttribute ^Attr in Param->CustomAttributes) {
      CXXRecordDecl *AttrClass =
        findCreateClassDecl(S, Attr->AttributeType->Resolve());
      if (AttrClass == S.getCLIContext()->ParamArrayAttribute)
        return true;
    }
  }
  return false;
}

static DeclarationName getMethodDeclName(Sema &S, MethodDefinition ^Method,
                                         CXXRecordDecl *RD) {
  ASTContext &C = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  std::string MethodName = marshalString<E_UTF8>(Method->Name);
  IdentifierInfo &II = IdentTable.get(MethodName);

  TagDecl *D = RD;

  if (RD->isCLIRecord() && RD->getCLIData()->isGeneric()) {
    /*Ty = S.getASTContext().getInjectedClassNameType(RD, */
    //D = RD->getDescribedClassTemplate();
  }

  DeclarationName DN;
  if (Method->IsConstructor) {
    CanQualType Ty = C.getCanonicalType(C.getTagDeclType(RD));
    DN = C.DeclarationNames.getCXXConstructorName(Ty);
  } else {
    DN = C.DeclarationNames.getIdentifier(&II);
  }

  return DN;
}

static CXXMethodDecl *findCreateMethod(Sema &S, MethodDefinition ^Method,
                                      CXXRecordDecl *RD,
                                      bool AddToDecl = true) {
  ASTContext &C = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  if (!RD) {
    RD = findCreateClassDecl(S, Method->DeclaringType);
    assert (RD && "Expected a valid record decl");
  }

  DeclarationName DN = getMethodDeclName(S, Method, RD);
  
  unsigned MetadataToken = Method->MetadataToken.ToUInt32();
  DeclContextLookupResult Res = RD->lookup(DN);
  for (; Res.first != Res.second; ++Res.first) {
    Decl *D = *Res.first;
    if (CXXMethodDecl *MD = dyn_cast<CXXMethodDecl>(D)) {
      CLIMethodData *CLIData = MD->getCLIData();
      if (CLIData->MetadataToken == MetadataToken)
        return MD;
    }
  }

  QualType RT;
  findCreateType(S, Method->ReturnType, RT, RD);
  if (RT.isNull())
    return 0;
  //assert(!RT.isNull() && "Expected a valid method return type");

  llvm::SmallVector<QualType, 4> ParamTypes;
  for each (ParameterDefinition ^Param in Method->Parameters) {
    QualType ParamType;
    if (!findCreateType(S, Param->ParameterType, ParamType, RD))
      return nullptr;
    
    if (ParamType.isNull())
      return nullptr;
    
    ParamTypes.push_back(ParamType);
  }

  FunctionProtoType::ExtProtoInfo Info;

  if (Method->CallingConvention == MethodCallingConvention::VarArg) {
    Info.Variadic = true;
  }

  QualType FT = C.getFunctionType(RT, ParamTypes.data(), ParamTypes.size(),
    Info);

  TypeSourceInfo *TSI = C.getTrivialTypeSourceInfo(FT);
  FunctionProtoTypeLoc FTL = cast<FunctionProtoTypeLoc>(TSI->getTypeLoc());
  DeclarationNameInfo DNI(DN, SourceLocation());

  CXXMethodDecl *MD = 0;
  if (Method->IsStatic && Method->IsConstructor) {
    return 0;
  } else if (Method->IsConstructor) {
    MD = CXXConstructorDecl::Create(C, RD, SourceLocation(), DNI, FT, TSI,
      /*IsExplicit=*/true, /*IsInline=*/false, /*IsImplicit=*/false,
      /*IsConstexpr=*/false);
  } else {
    MD = CXXMethodDecl::Create(C, RD, SourceLocation(), DNI, FT, TSI,
      /*IsStatic=*/Method->IsStatic, SC_None, /*IsInline=*/false,
      /*IsConstexpr=*/false, SourceLocation());
  }

  CLIMethodData *CLIData = new (C) CLIMethodData();
  CLIData->MetadataToken = MetadataToken;
  CLIData->FullName = marshalString<E_UTF8>(Method->FullName);

  MD->setCLIData(CLIData);
  MD->setAccess(convertMethodAccess(Method));

  llvm::SmallVector<ParmVarDecl*, 4> ParamDecls;
  unsigned paramIndex = 0;
  for each (ParameterDefinition^ Param in Method->Parameters) {
    std::string ParamName = marshalString<E_UTF8>(Param->Name);
    IdentifierInfo& PII = IdentTable.get(ParamName);

    QualType ParamType;
    findCreateType(S, Param->ParameterType, ParamType, RD);

    ParmVarDecl *PVD = ParmVarDecl::Create(C, MD, SourceLocation(),
      SourceLocation(), &PII, ParamType,
      C.getTrivialTypeSourceInfo(ParamType),
      SC_Auto, SC_Auto, 0);
    
    assert(PVD && "Expected a valid parameter decl");
    PVD->setScopeInfo(0, paramIndex);
    FTL.setArg(paramIndex++, PVD);

    createDeclAttributes(S, Param->CustomAttributes, PVD);

    ParamDecls.push_back(PVD);
  }

  createDeclAttributes(S, Method->CustomAttributes, MD);

  assert(ParamDecls.size() == Method->Parameters->Count);
  MD->setParams(ParamDecls);

  if (AddToDecl)
    RD->addDecl(MD);

  return MD;
}

static DeclaratorDecl *createField(Sema &S, FieldDefinition ^Field,
                                   CXXRecordDecl *RD) {
  ASTContext &C = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  std::string FieldName = marshalString<E_UTF8>(Field->Name);
  IdentifierInfo &II = IdentTable.get(FieldName);

  QualType FT;
  findCreateType(S, Field->FieldType, FT, RD);

  if (FT.isNull()) {
    std::string Name = marshalString<E_UTF8>(Field->FieldType->Name);
    return 0;
  }

  TypeSourceInfo *TSI = C.getTrivialTypeSourceInfo(FT);
  DeclaratorDecl *DD = 0;

  if (Field->IsStatic) {
    DD = VarDecl::Create(C, RD, SourceLocation(), SourceLocation(), &II,
      FT, TSI, SC_Static, SC_Static);
  } else {
    DD = FieldDecl::Create(C, RD, SourceLocation(), SourceLocation(), &II,
      FT, TSI, 0, 0, ICIS_NoInit);
  }

  assert(DD && "Expected a valid declarator decl");
  DD->setAccess(convertFieldAccess(Field));

  return DD;
}

static CLIRecordType GetCLIRecordType(TypeDefinition ^Type) {
  assert(Type->IsClass);
  if (Type->IsInterface)
    return CLI_RT_InterfaceType;
  else if (Type->IsValueType)
    return CLI_RT_ValueType;
  return CLI_RT_ReferenceType;
}

static CXXRecordDecl * findCreateClassDecl(Sema &S, TypeReference ^TypeRef);

static CLIDefinitionData *GetCLIRecordData(Sema &S, TypeDefinition^ TypeDef) {
  CLIDefinitionData *CD = new (S.getASTContext()) CLIDefinitionData();
  CD->AssemblyName = marshalString<E_UTF8>(TypeDef->Module->Assembly->Name->Name);
  CD->FullName = marshalString<E_UTF8>(TypeDef->FullName);
  CD->Type = CLI_RT_ReferenceType;

  if (TypeDef->IsInterface)
    CD->Type = CLI_RT_InterfaceType;
  else if (TypeDef->IsValueType)
    CD->Type = CLI_RT_ValueType;

  return CD;
}

static ClassTemplateDecl *GetCLIClassTemplate(Sema &S, TypeDefinition^ TypeDef,
                                              CXXRecordDecl *RD) {
  assert(TypeDef->HasGenericParameters);

  ASTContext &C = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  std::string Name = marshalString<E_UTF8>(TypeDef->Name);
  IdentifierInfo &II = IdentTable.get(Name);

  CLIGenericData *Data = new (C) CLIGenericData();
  llvm::SmallVector<TemplateTypeParmDecl *, 4> Params;

  for each(GenericParameter ^Param in TypeDef->GenericParameters) {
    CLIGenericParameter GP;
    GP.Name = marshalString<E_UTF8>(Param->Name);
    GP.Flags = 0;
    Data->Parameters.push_back(GP);

    if (Param->Type == GenericParameterType::Type) {
      TemplateTypeParmDecl *TP = TemplateTypeParmDecl::Create(C,
        C.getTranslationUnitDecl(), SourceLocation(), SourceLocation(),
        0, 0, &IdentTable.get(GP.Name), false, false);

      Params.push_back(TP);
    } else if (Param->Type == GenericParameterType::Method) {
      llvm_unreachable("Method template parameters not supported yet");
    }
  }
  
  RD->getCLIData()->setGenericData(Data);

  TemplateParameterList *TPL = TemplateParameterList::Create(C,
    RD->getLocation(), SourceLocation(),
    reinterpret_cast<NamedDecl**>(Params.data()), Params.size(),
    SourceLocation());

  NamespaceDecl *NS = findCreateNamespaces(S, TypeDef);
  ClassTemplateDecl *TD = ClassTemplateDecl::Create(C, NS, SourceLocation(),
    &II, TPL, RD, /*PrevDecl=*/0);
  RD->setDescribedClassTemplate(TD);

  RD->setTypeForDecl(0);
  QualType T = TD->getInjectedClassNameSpecialization();
  T = C.getInjectedClassNameType(RD, T);

  return TD;
}

static CLIRecordDecl * createClass(Sema &S, TypeDefinition^ TypeDef) {
  ASTContext &C = S.getASTContext();
  std::string Name = marshalString<E_UTF8>(TypeDef->Name);

  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();
  IdentifierInfo &II = IdentTable.get(Name);

  NamespaceDecl *NS = findCreateNamespaces(S, TypeDef);
  if (!NS) return nullptr;

  CLIRecordDecl *RD = CLIRecordDecl::Create(C, TTK_RefClass, NS, SourceLocation(),
    SourceLocation(), &II, 0);

  RD->setCLIData(GetCLIRecordData(S, TypeDef));
  RD->startDefinition();

  if (TypeDef->HasGenericParameters) {
    ClassTemplateDecl *TD = GetCLIClassTemplate(S, TypeDef, RD);
    NS->getPrimaryContext()->addDecl(TD);
  } else {
    NS->getPrimaryContext()->addDecl(RD);
  }

  return RD;
}

static CLICustomAttribute* createAttribute(Sema &S, CustomAttribute ^AttrDef) {
  ASTContext &C = S.getASTContext();
  
  CXXRecordDecl *AttrClass = findCreateClassDecl(S, AttrDef->AttributeType);
  if (!AttrClass) return 0;

  CXXMethodDecl *AttrCtor = findCreateMethod(S, AttrDef->Constructor->Resolve(),
    AttrClass);

  CLICustomAttribute *Attr = new(C) CLICustomAttribute(SourceRange(), C,
    AttrClass, AttrCtor);

  if (AttrDef->HasConstructorArguments) {
    for each (CustomAttributeArgument Arg in AttrDef->ConstructorArguments) {
      CLICustomAttribute::Argument A;
      //A.Expression = Arg.Type;
      Attr->Arguments.push_back(A);
    }
  }

  if (AttrDef->HasFields) {
    for each (CustomAttributeNamedArgument Arg in AttrDef->Fields) {
      CLICustomAttribute::Argument A;
      A.Name = marshalString<E_UTF8>(Arg.Name);
      //A.Expression = Arg.Type;
      Attr->Arguments.push_back(A);
    }
  }

  if (AttrDef->HasProperties) {
    for each (CustomAttributeNamedArgument Arg in AttrDef->Properties) {
      CLICustomAttribute::Argument A;
      A.Name = marshalString<E_UTF8>(Arg.Name);
      //A.Expression = Arg.Type;
      Attr->Arguments.push_back(A);
    }
  }

  return Attr;
}

static void createDeclAttributes(Sema &S, CustomAttributeCollection ^Attributes,
                                 Decl *D) {
  for each (CustomAttribute ^Attr in Attributes) {
    CLICustomAttribute *CLIAttr = createAttribute(S, Attr);
    D->addAttr(CLIAttr);
  }
}

static void createClassBases(Sema &S, TypeDefinition ^TypeDef,
                             CXXRecordDecl *RD) {
  ASTContext &C = S.getASTContext();
  if (TypeDef->BaseType != nullptr) {
    if (CLIRecordDecl *BRD = findCreateClassDecl(S, TypeDef->BaseType)) {
      QualType BaseType = C.getTypeDeclType(BRD);
      CXXBaseSpecifier *Base = new (C) CXXBaseSpecifier(SourceRange(), false,
        false, AS_public, C.getTrivialTypeSourceInfo(BaseType), SourceLocation());
      RD->setBases(&Base, 1);
    }
  }

  for each (TypeReference ^Interface in TypeDef->Interfaces) {
    if (CLIRecordDecl *BRD = findCreateClassDecl(S, Interface)) {
      QualType BaseType = C.getTypeDeclType(BRD);
      CXXBaseSpecifier *Base = new (C) CXXBaseSpecifier(SourceRange(), false,
        false, AS_public, C.getTrivialTypeSourceInfo(BaseType), SourceLocation());
      RD->getCLIData()->Interfaces.push_back(Base);
    }
  }
}

static void createClassMethods(Sema &S, TypeDefinition ^TypeDef,
                               CXXRecordDecl *RD) {
  for each (MethodDefinition ^Method in TypeDef->Methods) {
    if (String::IsNullOrWhiteSpace(Method->Name))
      continue;

    if (Method->IsGetter || Method->IsSetter)
      continue;

    FunctionDecl *FD = findCreateMethod(S, Method, RD);
  }
}

static void createClassFields(Sema &S, TypeDefinition ^TypeDef,
                              CXXRecordDecl *RD) {
  for each (FieldDefinition ^Field in TypeDef->Fields) {
    if (String::IsNullOrWhiteSpace(Field->Name))
      continue;

    if (DeclaratorDecl *DD = createField(S, Field, RD))
      RD->addDecl(DD);
  }
}

static void createClassProperties(Sema &S, TypeDefinition ^TypeDef,
                                  CXXRecordDecl *RD) {
  ASTContext &C = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  for each (PropertyDefinition ^PropDef in TypeDef->Properties) {
    if (String::IsNullOrWhiteSpace(PropDef->Name))
      continue;

    std::string Name = marshalString<E_UTF8>(PropDef->Name);
    IdentifierInfo &II = IdentTable.get(Name);
    DeclarationName DN = C.DeclarationNames.getIdentifier(&II);

    QualType PropTy;
    if (!findCreateType(S, PropDef->PropertyType, PropTy, RD))
      continue;

    CLIPropertyDecl *PD = CLIPropertyDecl::Create(S.getASTContext(),
      RD->getPrimaryContext(), DN, PropTy);

    if (PropDef->HasParameters) {
      for each (ParameterDefinition ^Param in PropDef->Parameters) {
        QualType IndexTy;
        findCreateType(S, Param->ParameterType, IndexTy, RD);
        PD->IndexerTypes.push_back(IndexTy);
      }
    }

    PD->setAccess(convertPropertyAccess(PropDef));

    if (PropDef->GetMethod)
      PD->GetMethod = findCreateMethod(S, PropDef->GetMethod, RD,
                                       /*AddToDecl=*/false);

    if (PropDef->SetMethod)
      PD->SetMethod = findCreateMethod(S, PropDef->SetMethod, RD,
                                       /*AddToDecl=*/false);

    createDeclAttributes(S, PropDef->CustomAttributes, PD);
    RD->addDecl(PD);
  }
}

static void createClassImplicitOperator(Sema &S, TypeDefinition ^TypeDef,
                                        CXXRecordDecl *RD,
                                        OverloadedOperatorKind Op) {
  ASTContext &C = S.getASTContext();

  QualType ClassTy = C.getHandleType(C.getRecordType(RD));
  llvm::SmallVector<QualType, 1> ParamTypes;
  ParamTypes.push_back(ClassTy);

  QualType ReturnTy = C.BoolTy;

  FunctionProtoType::ExtProtoInfo Info;
  QualType FunctionTy = C.getFunctionType(ReturnTy, ParamTypes.data(),
    ParamTypes.size(), Info);

  TypeSourceInfo *TSI = C.getTrivialTypeSourceInfo(FunctionTy);
  FunctionProtoTypeLoc FTL = cast<FunctionProtoTypeLoc>(TSI->getTypeLoc());

  DeclarationName DN = C.DeclarationNames.getCXXOperatorName(Op);
  DeclarationNameInfo DNI(DN, SourceLocation());

  CXXMethodDecl *MD = CXXMethodDecl::Create(C, RD, SourceLocation(),
    DNI, FunctionTy, TSI, /*IsStatic=*/false, SC_Auto, /*IsInline=*/false,
    /*IsConstexpr=*/false, SourceLocation());

  MD->setImplicit(true);

  llvm::SmallVector<StringRef, 1> ParamNames;
  ParamNames.push_back("rhs");

  llvm::SmallVector<ParmVarDecl*, 2> ParamDecls;
  for (unsigned i = 0; i < ParamNames.size(); ++i) {
    StringRef ParamName = ParamNames[i];
    IdentifierInfo &PII = S.getPreprocessor().getIdentifierTable().
      get(ParamName);

    QualType ParamType = ClassTy;
    ParmVarDecl *PVD = ParmVarDecl::Create(C, MD, SourceLocation(),
      SourceLocation(), &PII, ParamType,
      C.getTrivialTypeSourceInfo(ParamType), SC_Auto, SC_Auto, 0);
    
    assert(PVD && "Expected a valid parameter decl");
    PVD->setScopeInfo(0, i);
    FTL.setArg(i++, PVD);

    ParamDecls.push_back(PVD);
  }

  MD->setParams(ParamDecls);

  RD->addDecl(MD);
}

static void createClassImplicitOperators(Sema &S, TypeDefinition ^TypeDef,
                                         CXXRecordDecl *RD) {
  // 15.8.1 Handle equality operators
  // Every ref class type and value class type C implicitly provides the
  // following predefined equality operators:
  //
  //    bool operator ==(C^ x, C^ y); 
  //    bool operator !=(C^ x, C^ y);

  createClassImplicitOperator(S, TypeDef, RD, OO_EqualEqual);
  createClassImplicitOperator(S, TypeDef, RD, OO_ExclaimEqual);
}

static void createClassDecls(Sema &S, TypeDefinition ^TypeDef, CXXRecordDecl *RD) {
  createClassBases(S, TypeDef, RD);
  createClassMethods(S, TypeDef, RD);
  createClassFields(S, TypeDef, RD);
  createClassProperties(S, TypeDef, RD);
  createClassImplicitOperators(S, TypeDef, RD);

  RD->completeDefinition();
}

static CXXRecordDecl * findCreateClassDecl(Sema &S, TypeDefinition ^TypeDef) {
  if (!TypeDef) return 0;

  NamespaceDecl *NS = findCreateNamespaces(S, TypeDef);
  if (!NS) return 0;

  IdentifierInfo *II = getIdentifier(S, TypeDef->Name);
  DeclContextLookupResult Res = NS->lookup(II);

  for (; Res.first != Res.second; ++Res.first) {
    Decl *D = *Res.first;
    if (CLIRecordDecl *RD = dyn_cast<CLIRecordDecl>(D)) {
      return RD;
    } else if (ClassTemplateDecl *CTD = dyn_cast<ClassTemplateDecl>(D)) {
      CXXRecordDecl *TD = CTD->getTemplatedDecl();
      if (TD->isCLIRecord())
        return TD;
    }
  }

  // If we do not find the type, then it has not been found yet and we need
  // to create it explicitly.

  CXXRecordDecl *RD = createClass(S, TypeDef);
  createClassDecls(S, TypeDef, RD);

  return RD;
}

static CXXRecordDecl * findCreateClassDecl(Sema &S, TypeReference ^TypeRef) {
  TypeDefinition ^TypeDef = TypeRef->Resolve();
  return findCreateClassDecl(S, TypeDef);
}

static bool convertPrimitiveType(Sema &S, TypeReference ^TypeRef, QualType &Type) {
  ASTContext& C = S.getASTContext();
  int Token = TypeRef->MetadataToken.ToInt32();

  CLIPrimitiveTypes &P = S.getCLIContext()->Types;

  if (Token == P.Void.Token) {
    Type = C.VoidTy;
    return true;
  } else if (Token == P.Boolean.Token) {
    Type = C.BoolTy;
    return true;
  } else if (Token == P.Char.Token) {
    Type = C.WCharTy;
    return true;
  } else if (Token == P.Byte.Token) {
    Type = C.UnsignedCharTy;
    return true;
  } else if (Token == P.SByte.Token) {
    // FIXME: Check for modopt IsSignUnspecifiedByte
    Type = C.CharTy;
    return true;
  } else if (Token == P.Int16.Token) {
    Type = C.ShortTy;
    return true;
  } else if (Token == P.UInt16.Token) {
    Type = C.UnsignedShortTy;
    return true;
  } else if (Token == P.Int32.Token) {
    // FIXME: Check for modopt IsLong
    Type = C.IntTy;
    return true;
  } else if (Token == P.UInt32.Token) {
    // FIXME: Check for modopt IsLong
    Type = C.UnsignedIntTy;
    return true;
  } else if (Token == P.Int64.Token) {
    Type = C.LongLongTy;
    return true;
  } else if (Token == P.UInt64.Token) {
    Type = C.UnsignedLongLongTy;
    return true;
  } else if (Token == P.Single.Token) {
    Type = C.FloatTy;
    return true;
  } else if (Token == P.Double.Token) {
    // FIXME: Check for modopt IsLong
    Type = C.DoubleTy;
    return true;
  } else if (Token == P.String.Token) {
    Type = P.String.Ty;
    return true;
  } else if (Token == P.IntPtr.Token) {
    Type = P.IntPtr.Ty;
    return true;
  } else if (Token == P.UIntPtr.Token) {
    Type = P.UIntPtr.Ty;
    return true;
  } else 
  return false;
}

static void createClassGenericParameters(Sema &S, CXXRecordDecl *RD,
                                         GenericInstanceType ^Type) {
  for each(GenericParameter ^Param in Type->GenericParameters) {
    std::string Name = marshalString<E_UTF8>(Param->Name);
    //Console::WriteLine("{0} {1}", Param->FullName, Param->Name);
    continue;
  }

  for each(TypeReference ^Arg in Type->GenericArguments) {
    std::string Name = marshalString<E_UTF8>(Arg->Name);
    //Console::WriteLine("{0} {1}", Arg->FullName, Arg->Name);
    continue;
  }

  // Demangle the CLI generic name.
  //std::string Name = marshalString<E_UTF8>(Type->Name);
  //Name = Name.substr(0, Name.find("`"));

}

static bool findCreateType(Sema &S, TypeReference ^TypeRef, QualType &Type,
                           CXXRecordDecl *MethodRecord) {
  if (convertPrimitiveType(S, TypeRef, Type))
    return true;

  ASTContext &Context = S.getASTContext();

  if (TypeRef->IsArray) {
    Mono::Cecil::ArrayType ^Arr = safe_cast<Mono::Cecil::ArrayType^>(TypeRef);
    QualType ElementType;
    if (!findCreateType(S, Arr->ElementType, ElementType, MethodRecord))
      return false;
    CXXRecordDecl *RD = findCreateClassDecl(S, Arr->ElementType);
    Type = Context.getHandleType(Context.getCLIArrayType(ElementType,
      Arr->Rank, S.getCLIContext()->Types.Array.Decl));
    return true;
  } else if (TypeRef->IsGenericParameter) {
    assert(MethodRecord && "Expected a valid C++/CLI record");

    ClassTemplateDecl *TD = MethodRecord->getDescribedClassTemplate();
    if (!TD) {
      Type = Context.VoidTy;
      return false;
    }

    assert(TD && "Expected a valid class template");
    std::string Name = marshalString<E_UTF8>(TypeRef->Name);

    TemplateParameterList *TPL = TD->getTemplateParameters();
    for (unsigned I = 0, E = TPL->size(); I != E; ++I) {
      NamedDecl *TP = TPL->getParam(I);
      
      TemplateTypeParmDecl *TT = dyn_cast<TemplateTypeParmDecl>(TP);
      assert(TT && "Expected a template type parameter");
      
      if (TP->getName() == Name) {
        Type = Context.getTemplateTypeParmType(0, I, 0, TT);
        return true;
      }
    }
  } else {
    if (CXXRecordDecl *RD = findCreateClassDecl(S, TypeRef)) {
      if (TypeRef->IsGenericInstance) {
        GenericInstanceType ^Instance = safe_cast<GenericInstanceType^>(TypeRef);
        createClassGenericParameters(S, RD, Instance);
        Type = Context.getHandleType(Context.getRecordType(RD));
        return true;
      }
      Type = Context.getHandleType(Context.getRecordType(RD));
      return true;
    }
  }

  //llvm_unreachable("Unhandled managed type");
  Type = Context.VoidTy;
  return false;
}

static void initializeCLIType(Sema &S, CLIPrimitiveType &P, CLITypeKind Kind,
                              TypeDefinition ^Type) {
  P.Token = Type->MetadataToken.ToInt32();
  P.Decl = createClass(S, Type);
  P.Decl->getCLIData()->Kind = Kind;

  QualType RT = S.getASTContext().getRecordType(P.Decl);

  if (Type->IsValueType)
    P.Ty = RT;
  else
    P.Ty = S.getASTContext().getHandleType(RT);
}

static void initializeCLIClass(Sema &S, CLIPrimitiveType &P,
                               TypeDefinition ^Type) {
  createClassDecls(S, Type, P.Decl);
}

static void initializeCLITypes(Sema &S, AssemblyDefinition^ Assembly) {
  ModuleDefinition ^Module = Assembly->MainModule;
  CLIPrimitiveTypes &P = S.getCLIContext()->Types;

  // Get the metadata tokens for each type so we can look it up later.
#define CLI_TYPE(X) \
  initializeCLIType(S, P.X, CLI_TK_None, Module->GetType("System."#X));
#define CLI_PRITIMIVE_TYPE(X) \
  initializeCLIType(S, P.X, CLI_TK_##X, Module->GetType("System."#X));
#include "clang/AST/CLITypes.def"
#undef CLI_TYPE
#undef CLI_PRITIMIVE_TYPE

#define CLI_TYPE(X) \
  initializeCLIClass(S, P.X, Module->GetType("System."#X));
#include "clang/AST/CLITypes.def"
#undef CLI_TYPE

  CLISemaContext &Ctx = *S.getCLIContext();

  Ctx.DefaultMemberAttribute = findCreateClassDecl(S,
    Module->GetType("System.Reflection.DefaultMemberAttribute"));

  Ctx.ParamArrayAttribute = findCreateClassDecl(S,
    Module->GetType("System.ParamArrayAttribute"));

  Ctx.IEnumerable = findCreateClassDecl(S,
    Module->GetType("System.Collections.IEnumerable"));

  Ctx.GenericIEnumerable = findCreateClassDecl(S,
    Module->GetType("System.Collections.Generic.IEnumerable`1"));
}

static NamespaceDecl *initializeNamespaceDecl(Sema &S) {
  ASTContext &ASTContext = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();
  TranslationUnitDecl *TU = ASTContext.getTranslationUnitDecl();

  NamespaceDecl *Namespace = NamespaceDecl::Create(ASTContext,
    TU->getPrimaryContext(), /*IsInline=*/false, SourceLocation(),
    SourceLocation(), &IdentTable.get("cli"), 0);
  Namespace->setImplicit(true);
  TU->addDecl(Namespace);

  return Namespace;
}

static ClassTemplateDecl *initializeCLIArrayDecl(Sema &S,
                                                 NamespaceDecl *Namespace) {
  ASTContext &ASTContext = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  CXXRecordDecl *RD = CXXRecordDecl::Create(ASTContext, TTK_RefClass,
    Namespace->getPrimaryContext(), SourceLocation(), SourceLocation(),
    &IdentTable.get("array"), 0, false);

  RD->startDefinition();
  RD->completeDefinition();

  // Set System::Array as the base class.
  QualType BaseType = ASTContext.getTypeDeclType(S.getCLIContext()->
    Types.Array.Decl);
  CXXBaseSpecifier *Base = new (ASTContext) CXXBaseSpecifier(SourceRange(),
    false, false, AS_public, ASTContext.getTrivialTypeSourceInfo(BaseType),
    SourceLocation());
  RD->setBases(&Base, 1);

  TemplateTypeParmDecl *TT = TemplateTypeParmDecl::Create(ASTContext,
    RD->getPrimaryContext(), SourceLocation(), SourceLocation(),
    0, 0, &IdentTable.get("T"), /*Typename=*/true, /*ParameterPack=*/false);

  NonTypeTemplateParmDecl *NT = NonTypeTemplateParmDecl::Create(ASTContext,
    RD->getPrimaryContext(), SourceLocation(), SourceLocation(),
    0, 0, &IdentTable.get("Rank"), ASTContext.UnsignedIntTy, false,
    ASTContext.getTrivialTypeSourceInfo(ASTContext.UnsignedIntTy));
  NT->setDefaultArgument(IntegerLiteral::Create(ASTContext,
      llvm::APInt(32, 1), ASTContext.UnsignedIntTy, SourceLocation()), false);

  NamedDecl *TemplateDecls[] = { TT, NT };
  TemplateParameterList *TPL = TemplateParameterList::Create(ASTContext,
    SourceLocation(), SourceLocation(), TemplateDecls, 2, SourceLocation());

  ClassTemplateDecl *ClassTemplate = 
    ClassTemplateDecl::Create(ASTContext, Namespace->getPrimaryContext(),
      SourceLocation(), DeclarationName(&IdentTable.get("array")), TPL,
      RD, 0);

  Namespace->addDecl(ClassTemplate);

  return ClassTemplate;
}

static ClassTemplateDecl *initializeInteriorPtrDecl(Sema &S,
                                                    NamespaceDecl *Namespace) {
  ASTContext &ASTContext = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  CXXRecordDecl *RD = CXXRecordDecl::Create(ASTContext, TTK_Class,
    Namespace->getPrimaryContext(), SourceLocation(), SourceLocation(),
    &IdentTable.get("interior_ptr"), 0, false);

  TemplateTypeParmDecl *TT = TemplateTypeParmDecl::Create(ASTContext,
    RD->getPrimaryContext(), SourceLocation(), SourceLocation(),
    0, 0, &IdentTable.get("T"), /*Typename=*/true, false);

  NamedDecl *TemplateDecls[] = { TT };
  TemplateParameterList *TPL = TemplateParameterList::Create(ASTContext,
    SourceLocation(), SourceLocation(), TemplateDecls, 1, SourceLocation());

  ClassTemplateDecl *ClassTemplate = 
    ClassTemplateDecl::Create(ASTContext, Namespace->getPrimaryContext(),
      SourceLocation(), DeclarationName(&IdentTable.get("interior_ptr")), TPL,
      RD, 0);
  Namespace->addDecl(ClassTemplate);

  return ClassTemplate;
}

static ClassTemplateDecl *initializePinPtrDecl(Sema &S,
                                               NamespaceDecl *Namespace) {
  ASTContext &ASTContext = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  CXXRecordDecl *RD = CXXRecordDecl::Create(ASTContext, TTK_Class,
    Namespace->getPrimaryContext(), SourceLocation(), SourceLocation(),
    &IdentTable.get("pin_ptr"), 0, false);

  TemplateTypeParmDecl *TT = TemplateTypeParmDecl::Create(ASTContext,
    RD->getPrimaryContext(), SourceLocation(), SourceLocation(),
    0, 0, &IdentTable.get("T"), /*Typename=*/true, false);

  NamedDecl *TemplateDecls[] = { TT };
  TemplateParameterList *TPL = TemplateParameterList::Create(ASTContext,
    SourceLocation(), SourceLocation(), TemplateDecls, 1, SourceLocation());

  ClassTemplateDecl *ClassTemplate = 
    ClassTemplateDecl::Create(ASTContext, Namespace->getPrimaryContext(),
      SourceLocation(), DeclarationName(&IdentTable.get("pin_ptr")), TPL,
      RD, 0);
  Namespace->addDecl(ClassTemplate);

  return ClassTemplate;
}

static ClassTemplateDecl *initializeSafeCastDecl(Sema &S,
                                               NamespaceDecl *Namespace) {
  ASTContext &ASTContext = S.getASTContext();
  IdentifierTable &IdentTable = S.getPreprocessor().getIdentifierTable();

  CXXRecordDecl *RD = CXXRecordDecl::Create(ASTContext, TTK_Class,
    Namespace->getPrimaryContext(), SourceLocation(), SourceLocation(),
    &IdentTable.get("safe_cast"), 0, false);

  TemplateTypeParmDecl *TT = TemplateTypeParmDecl::Create(ASTContext,
    RD->getPrimaryContext(), SourceLocation(), SourceLocation(),
    0, 0, &IdentTable.get("T"), /*Typename=*/true, false);

  NamedDecl *TemplateDecls[] = { TT };
  TemplateParameterList *TPL = TemplateParameterList::Create(ASTContext,
    SourceLocation(), SourceLocation(), TemplateDecls, 1, SourceLocation());

  ClassTemplateDecl *ClassTemplate = 
    ClassTemplateDecl::Create(ASTContext, Namespace->getPrimaryContext(),
      SourceLocation(), DeclarationName(&IdentTable.get("safe_cast")), TPL,
      RD, 0);
  Namespace->addDecl(ClassTemplate);

  return ClassTemplate;
}

static void initializeCLINamespace(Sema &S) {
  CLISemaContext *Ctx = S.getCLIContext();

  assert(!Ctx->CLINamespace && "Expected unitialized CLI namespace");
  NamespaceDecl *Namespace = initializeNamespaceDecl(S);
  Ctx->CLINamespace = Namespace;

  // C++/CLI provides a couple of declarations in the ::cli namespace.
  //  array, interior_ptr, pin_ptr, or safe_cast

  assert(!Ctx->Array && "Expected unitialized CLI array decl");
  Ctx->Array = initializeCLIArrayDecl(S, Namespace);

  assert(!Ctx->InteriorPtr && "Expected unitialized CLI interior_ptr decl");
  Ctx->InteriorPtr = initializeInteriorPtrDecl(S, Namespace);

  assert(!Ctx->PinPtr && "Expected unitialized CLI pin_ptr decl");
  Ctx->InteriorPtr = initializePinPtrDecl(S, Namespace);

  assert(!Ctx->SafeCast && "Expected unitialized CLI safe_cast decl");
  Ctx->SafeCast = initializeSafeCastDecl(S, Namespace);
  
  ASTContext &ASTContext = S.getASTContext();
  TranslationUnitDecl *TU = ASTContext.getTranslationUnitDecl();
  UsingDirectiveDecl *UD = UsingDirectiveDecl::Create(ASTContext,
    TU->getPrimaryContext(), SourceLocation(), SourceLocation(),
    NestedNameSpecifierLoc(), SourceLocation(), Namespace,
    Namespace->getParent());
  TU->addDecl(UD);
}

void Sema::LoadManagedAssembly(FileID FID) {
  SourceManager &SourceMgr = getSourceManager();
  
  const llvm::MemoryBuffer *Buffer = SourceMgr.getBuffer(FID);
  assert(Buffer && "Expected a valid buffer from file ID");

  void *Buf = (void*) Buffer->getBufferStart();
  size_t Size = Buffer->getBufferSize();

  array<Byte> ^Data = gcnew array<Byte>(Size);
  Marshal::Copy(IntPtr(Buf), Data, 0, Int32(Size)); 

  AssemblyDefinition ^Assembly = AssemblyDefinition::ReadAssembly(
    gcnew MemoryStream(Data));

  assert(CLIContext && "Expected an initialized CLI context");
  
  if (Assembly->Name->HasPublicKey) {
    String^ Token = BitConverter::ToString(Assembly->Name->PublicKeyToken);
    Token = Token->Replace("-", String::Empty)->ToLower();
    // Compare the assembly's public key token with that of mscorlib.
    // This token is the same in both Mono and Microsoft CLR.
    if (Token->Equals("b77a5c561934e089")
        && Assembly->Name->Name->Equals("mscorlib"))
      initializeCLITypes(*this, Assembly);

    if (CLIContext->CLINamespace == 0)
      initializeCLINamespace(*this);
  }

  for each (ModuleDefinition ^Module in Assembly->Modules) {
    for each (TypeDefinition ^TypeDef in Module->GetTypes()) {
      QualType Type;
      findCreateType(*this, TypeDef, Type);
    }
  }
}

/// C++/CLI conversion functions
static const CLIRecordDecl * getCLIRecordDeclForHandleType(QualType T) {
  if (const HandleType *PT = T->getAs<HandleType>())
    if (const RecordType *RT = PT->getPointeeType()->getAs<RecordType>())
      return dyn_cast<CLIRecordDecl>(RT->getDecl());
  return 0;
}

#pragma unmanaged

/// Helper function to determine whether this is C++/CLI string literal
/// conversion to a System::String^. (C++/CLI 14.2.5 String literal conversions)
static bool isCLIStringLiteralConversion(Expr *From, QualType ToType) {
  // Look inside the implicit cast, if it exists.
  if (ImplicitCastExpr *Cast = dyn_cast<ImplicitCastExpr>(From))
    From = Cast->getSubExpr();

  StringLiteral *StrLit = dyn_cast<StringLiteral>(From->IgnoreParens());
  if (!StrLit) return false;

  const CLIRecordDecl * RD = getCLIRecordDeclForHandleType(ToType);
  if (!RD) return false;

  switch (StrLit->getKind()) {
    case StringLiteral::UTF8:
    case StringLiteral::UTF16:
    case StringLiteral::UTF32:
    case StringLiteral::Ascii:
    case StringLiteral::Wide:
      return true;
  }

  return false;
}

static QualType ConvertBuiltinTypeToCLIPrimitiveType(Sema &S,
                                        const BuiltinType *Builtin) {
  CLISemaContext *Ctx = S.getCLIContext();
  CLIPrimitiveTypes &Tys = Ctx->Types;
  
  // 8.2.1 Fundamental types and the CLI
  switch (Builtin->getKind()) {
  case BuiltinType::Bool: return Tys.Boolean.Ty;
  case BuiltinType::SChar: return Tys.SByte.Ty;
  case BuiltinType::UChar: return Tys.Byte.Ty;
  case BuiltinType::Char_S: return Tys.SByte.Ty;
  case BuiltinType::Char_U: return Tys.SByte.Ty;
  case BuiltinType::Short: return Tys.Int16.Ty;
  case BuiltinType::UShort: return Tys.UInt16.Ty;
  case BuiltinType::Int: return Tys.Int32.Ty;
  case BuiltinType::UInt: return Tys.UInt32.Ty;
  case BuiltinType::Long: return Tys.Int32.Ty;
  case BuiltinType::ULong: return Tys.UInt32.Ty;
  case BuiltinType::LongLong: return Tys.Int64.Ty;
  case BuiltinType::ULongLong: return Tys.UInt64.Ty;
  case BuiltinType::Float: return Tys.Single.Ty;
  case BuiltinType::Double: return Tys.Double.Ty;
  case BuiltinType::LongDouble: return Tys.Double.Ty;
  case BuiltinType::WChar_S: return Tys.Char.Ty;
  case BuiltinType::WChar_U: return Tys.Char.Ty;
  default: return QualType();
  }

  return QualType();
}

/// IsBoxingConversion - Determines whether the conversion from
/// FromType to ToType is a boxing conversion (C++/CLI 14.2.6).
bool IsBoxingConversion(Sema &S, QualType FromType, QualType ToType,
                        QualType &ConvertedType) {
  // "A boxing conversion involves the creation of a new object on the
  // CLI heap. A boxing conversion shall be applied only to instances of
  // value types, with the exception of pointers. For any given value
  // type V, the conversion results in a V^."
  if (!ToType->isHandleType())
    return false;

  CXXRecordDecl *RD = ToType->getPointeeType()->getAsCXXRecordDecl();
  if (!RD | !RD->isCLIRecord())
    return false;

  // Value Types: Fundamental Type, Enum, Pointer (not considered for
  // boxing purposes), Value Class
  if (const BuiltinType *FromBuiltin = FromType->getAs<BuiltinType>()) {
    QualType Ty = ConvertBuiltinTypeToCLIPrimitiveType(S, FromBuiltin);
    if (Ty.isNull())
      return false;
    ConvertedType = Ty;
    // Value types are not initialized as handles
    ConvertedType = S.getASTContext().getHandleType(ConvertedType);
  } else if (const RecordType *FromRecord = FromType->getAs<RecordType>()) {
    const CXXRecordDecl *FRD = FromRecord->getAsCXXRecordDecl();
    if (!FRD->isCLIRecord())
      return false;
    if (FRD->getCLIData()->Type != CLI_RT_ValueType)
      return false;
    CXXBasePaths P;
    if (!S.Context.hasSameType(ToType->getPointeeType(), FromType) &&
        !FRD->isDerivedFrom(RD, P))
      return false;
    ConvertedType = ToType;
  } else if (const EnumType *FromEnum = FromType->getAs<EnumType>()) {
    llvm_unreachable("Enum boxing conversions not implemented yet");
  } else {
    return false;
  }

  assert(!ConvertedType.isNull());
  if (RD == S.getCLIContext()->Types.Object.Decl)
    ConvertedType = S.getCLIContext()->Types.Object.Ty;

  return true;
}

// Used in SemaExprCXX.cpp
QualType GetBoxingConversionValueType(Sema &S, QualType FromType) {
  if (const BuiltinType *FromBuiltin = FromType->getAs<BuiltinType>()) {
    QualType Ty = ConvertBuiltinTypeToCLIPrimitiveType(S, FromBuiltin);
    assert(!Ty.isNull());
    return Ty;
  } else if (const RecordType *FromRecord = FromType->getAs<RecordType>()) {
    return FromType;
  //} else if (const EnumType *FromEnum = FromType->getAs<EnumType>()) {
  } else {
    assert(0 && "Expected a valid fundamental type");
  }

  return QualType();
}

/// CheckHandleConversion - Check the handle conversion from the
/// expression From to the type ToType. This routine checks for
/// ambiguous or inaccessible derived-to-base handle
/// conversions for which IsHandleConversion has already returned
/// true. It returns true and produces a diagnostic if there was an
/// error, or returns false otherwise.
bool Sema::CheckHandleConversion(Expr *From, QualType ToType,
                                 CastKind &Kind,
                                 CXXCastPath &BasePath,
                                 bool IgnoreBaseAccess) {
  QualType FromType = From->getType();

  if (FromType->isHandleType()) {
    // The conversion was successful.
    // FIXME: Check for accessibility
    Kind = CK_CLI_DerivedToBaseHandle;
    return false;
  }

  // We shouldn't fall into this case unless it's valid for other
  // reasons.
  if (From->isNullPointerConstant(Context, Expr::NPC_ValueDependentIsNull)) {
    Kind = CK_CLI_NullToHandle;
    return false;
  }

  return true;
}

} // end namespace clang