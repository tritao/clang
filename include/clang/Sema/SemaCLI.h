//===--- SemaCLI.h - Semantic Analysis for C++/CLI--------*----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file declares helpers for semantic analysis for C++/CLI language
//  constructs.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_SEMA_SEMACLI_H
#define LLVM_CLANG_SEMA_SEMACLI_H

#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclCLI.h"
#include "clang/AST/Type.h"

namespace clang {

struct CLIPrimitiveType {
  int Token;
  clang::QualType Ty;
  clang::CXXRecordDecl *Decl;
};

struct CLIPrimitiveTypes {
#define CLI_TYPE(X) \
  CLIPrimitiveType X;
#include "clang/AST/CLITypes.def"
#undef CLI_TYPE
};

class CLISemaContext {
public:
  CLISemaContext() : CLINamespace(0), Array(0), InteriorPtr(0),
    PinPtr(0), SafeCast(0), ParamArrayAttribute(0),
    IEnumerable(0), GenericIEnumerable(0) {
  }

  CLIPrimitiveTypes Types;
  NamespaceDecl *CLINamespace;
  NamedDecl *Array;
  NamedDecl *InteriorPtr;
  NamedDecl *PinPtr;
  NamedDecl *SafeCast;
  CXXRecordDecl *DefaultMemberAttribute;
  CXXRecordDecl *ParamArrayAttribute;
  CXXRecordDecl *IEnumerable;
  CXXRecordDecl *GenericIEnumerable;
};

bool HasCLIParamArrayAttribute(Sema &S, FunctionDecl* Fn, QualType& Type);

}  // end namespace clang

#endif