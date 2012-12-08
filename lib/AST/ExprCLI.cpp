//===--- ExprCLI.cpp - (C++/CLI) Expression AST Node Implementation --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Expr class declared in ExprCLI.h
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/IdentifierTable.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclCLI.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/ExprCLI.h"
#include "clang/AST/TypeLoc.h"
using namespace clang;

// CLIGCNewExpr
CLIGCNewExpr::CLIGCNewExpr(ASTContext &C,
             CXXNewExpr::InitializationStyle initializationStyle,
             Expr *initializer, QualType ty, TypeSourceInfo *allocatedTypeInfo,
             SourceLocation startLoc, SourceRange directInitRange)
  : Expr(CLIGCNewExprClass, ty, VK_RValue, OK_Ordinary,
         ty->isDependentType(), ty->isDependentType(),
         ty->isInstantiationDependentType(),
         ty->containsUnexpandedParameterPack()),
    Initializer(0), AllocatedTypeInfo(allocatedTypeInfo),
    StartLoc(startLoc), DirectInitRange(directInitRange) {
  assert((initializer != 0 || initializationStyle == CXXNewExpr::NoInit) &&
         "Only NoInit can have no initializer.");
  StoredInitializationStyle = initializer ? initializationStyle + 1 : 0;

  if (initializer) {
    //if (initializer->isInstantiationDependent())
    //  ExprBits.InstantiationDependent = true;

    //if (initializer->containsUnexpandedParameterPack())
    //  ExprBits.ContainsUnexpandedParameterPack = true;

    Initializer = initializer;
  }
}

SourceLocation CLIGCNewExpr::getEndLoc() const {
  switch (getInitializationStyle()) {
  case CXXNewExpr::NoInit:
    return AllocatedTypeInfo->getTypeLoc().getEndLoc();
  case CXXNewExpr::CallInit:
    return DirectInitRange.getEnd();
  case CXXNewExpr::ListInit:
    return getInitializer()->getSourceRange().getEnd();
  }
  llvm_unreachable("bogus initialization style");
}

// CLIValueClassInitExpr
SourceRange CLIValueClassInitExpr::getSourceRange() const {
  SourceLocation Start;
  if (TypeInfo)
    Start = TypeInfo->getTypeLoc().getBeginLoc();
  return SourceRange(Start, TypeInfo->getTypeLoc().getEndLoc());
}


