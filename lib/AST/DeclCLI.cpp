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
#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace clang;

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
  return new (C) CLIPropertyDecl(DC, DN, Ty);
}

CLIPropertyDecl *CLIPropertyDecl::CreateDeserialized(ASTContext &C, unsigned ID) {
  void *Mem = AllocateDeserializedDecl(C, ID, sizeof(CLIPropertyDecl));
  return new (Mem) CLIPropertyDecl(0, DeclarationName(), QualType());
}
