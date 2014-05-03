//===--- DeclCLI.cpp - C++/CLI Declaration AST Node Implementation -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the C++/CLI related Decl classes.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCLI.h"
#include "clang/Sema/SemaCLI.h"
#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace clang;

CLIMethodData::CLIMethodData()
    : MetadataToken(0)
    , ReturnTemplateParamIndex(0)
    , ParameterArrayOriginalOverload(0) {
}

CLIMethodData::CLIMethodData(CLIMethodData& MD)
    : MetadataToken(MD.MetadataToken)
    , FullName(MD.FullName)
    , ReturnTemplateParamIndex(MD.ReturnTemplateParamIndex)
    , ParameterArrayOriginalOverload(MD.ParameterArrayOriginalOverload) {
}

bool CXXMethodDecl::hasCLIParametersArray(Sema& S) const {
  if (!isCLIMethod())
    return false;
  QualType CLIParamsType;
  return HasCLIParamArrayAttribute(S, this, CLIParamsType);
}

CLIDefinitionData *CXXRecordDecl::getCLIData() const {
  return CLIData;
}

void CXXRecordDecl::setCLIData(CLIDefinitionData *Data) {
  assert(!CLIData && "Expected an invalid CLI class data");
  CLIData = Data;
}

bool CXXRecordDecl::isCLIRecord() const {
  return CLIData != 0;
}

void CLIPropertyDecl::anchor() { }

CLIPropertyDecl::CLIPropertyDecl(DeclContext *DC, DeclarationName DN,
                                 QualType Ty)
  : ValueDecl(CLIProperty, DC, SourceLocation(), DN, Ty),
    GetMethod(0), SetMethod(0), Field(0) {
}

CLIPropertyDecl *CLIPropertyDecl::Create(ASTContext &C, DeclContext *DC,
                                         DeclarationName DN, QualType Ty) {
  return new (C,DC) CLIPropertyDecl(DC, DN, Ty);
}

CLIPropertyDecl *CLIPropertyDecl::CreateDeserialized(ASTContext &C, unsigned ID) {
  //void *Mem = AllocateDeserializedDecl(C, ID, sizeof(CLIPropertyDecl));
  return new (C,ID) CLIPropertyDecl(0, DeclarationName(), QualType());
}

void CLIEventDecl::anchor() { }

CLIEventDecl::CLIEventDecl(DeclContext *DC, DeclarationName DN,
                                 QualType Ty)
  : ValueDecl(CLIEvent, DC, SourceLocation(), DN, Ty){
}

CLIEventDecl *CLIEventDecl::Create(ASTContext &C, DeclContext *DC,
                                         DeclarationName DN, QualType Ty) {
  return new (C,DC) CLIEventDecl(DC, DN, Ty);
}

CLIEventDecl *CLIEventDecl::CreateDeserialized(ASTContext &C, unsigned ID) {
  //void *Mem = AllocateDeserializedDecl(C, ID, sizeof(CLIPropertyDecl));
  return new (C,ID) CLIEventDecl(0, DeclarationName(), QualType());
}

CLICustomAttribute *CLICustomAttribute::clone(ASTContext &C) const {
  return new (C) CLICustomAttribute(SourceRange(), C, Class, Ctor);
}

void CLICustomAttribute::printPretty(llvm::raw_ostream &OS,
                                     const PrintingPolicy &Policy) const {
}