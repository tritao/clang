//===-- ExprCLI.h - Classes for representing C++/CLI expressions ------=====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the C++/CLI Expr subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_EXPRCLI_H
#define LLVM_CLANG_AST_EXPRCLI_H

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCLI.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Basic/ExpressionTraits.h"
#include "clang/Basic/TypeTraits.h"
#include "llvm/Support/Compiler.h"

namespace clang {

/// @brief Represents a C++/CLI gcnew-expression for memory allocation
// and constructor calls, e.g: "new CLIGCNewExpr(foo)".
class CLIGCNewExpr : public Expr {
  // Contains an optional initialization expression.
  Stmt *Initializer;

  /// \brief The allocated type-source information, as written in the source.
  TypeSourceInfo *AllocatedTypeInfo;

  /// \brief Location of the first token.
  SourceLocation StartLoc;

  /// \brief Source-range of a paren-delimited initializer.
  SourceRange DirectInitRange;

  // What kind of initializer do we have? Could be none, parens, or braces.
  // In storage, we distinguish between "none, and no initializer expr", and
  // "none, but an implicit initializer expr".
  unsigned StoredInitializationStyle : 2;

  friend class ASTStmtReader;
  friend class ASTStmtWriter;
public:

  CLIGCNewExpr(ASTContext &C, CXXNewExpr::InitializationStyle initStyle,
             Expr *initializer, QualType ty, TypeSourceInfo *AllocatedTypeInfo,
             SourceLocation startLoc, SourceRange directInitRange);

  explicit CLIGCNewExpr(EmptyShell Shell)
    : Expr(CLIGCNewExprClass, Shell), Initializer(0) { }

  QualType getAllocatedType() const {
    assert(getType()->isHandleType());
    return getType()->getAs<HandleType>()->getPointeeType();
  }

  TypeSourceInfo *getAllocatedTypeSourceInfo() const {
    return AllocatedTypeInfo;
  }

  /// \brief Whether this gcnew-expression has any initializer at all.
  bool hasInitializer() const { return StoredInitializationStyle > 0; }

  /// \brief The kind of initializer this new-expression has.
  CXXNewExpr::InitializationStyle getInitializationStyle() const {
    if (StoredInitializationStyle == 0)
      return CXXNewExpr::NoInit;
    return static_cast<CXXNewExpr::InitializationStyle>(
        StoredInitializationStyle-1);
  }

  /// \brief The initializer of this new-expression.
  Expr *getInitializer() {
    return cast<Expr>(Initializer);
  }
  const Expr *getInitializer() const {
    return cast<Expr>(Initializer);
  }

  /// \brief Returns the CXXConstructExpr from this gcnew-expression, or NULL.
  const CXXConstructExpr* getConstructExpr() {
    return dyn_cast_or_null<CXXConstructExpr>(getInitializer());
  }

  SourceLocation getStartLoc() const { return StartLoc; }
  SourceLocation getEndLoc() const;

  SourceRange getDirectInitRange() const { return DirectInitRange; }

  SourceRange getSourceRange() const LLVM_READONLY {
    return SourceRange(getStartLoc(), getEndLoc());
  }

  // Iterators
  child_range children() { return child_range(&Initializer, &Initializer+1); }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CLIGCNewExprClass;
  }
  static bool classof(const CLIGCNewExpr *) { return true; }
};

enum CLIValueClassInitKind {
  CLI_VCIK_ZeroInit,
  CLI_VCIK_CopyInit,
};

/// @brief A C++/CLI expression which represents zero or copy initialization
/// of value classes.
class CLIValueClassInitExpr : public Expr {
  CLIValueClassInitKind InitKind;
  Expr *InitExpr; // When InitKind == CLI_VCIK_CopyInit
  TypeSourceInfo *TypeInfo;

  friend class ASTStmtReader;

public:
  /// \brief Create an expression which represents zero or copy initialization
  /// of value classes.
  CLIValueClassInitExpr(QualType Type,
                        TypeSourceInfo *TypeInfo,
                        CLIValueClassInitKind InitKind,
                        Expr *InitExpr) :
    Expr(CLIValueClassInitExprClass, Type, VK_RValue, OK_Ordinary,
         false, false, Type->isInstantiationDependentType(), false),
    TypeInfo(TypeInfo), InitKind(InitKind), InitExpr(InitExpr) {}

  explicit CLIValueClassInitExpr(EmptyShell Shell)
    : Expr(CLIValueClassInitExprClass, Shell) { }

  TypeSourceInfo *getTypeSourceInfo() const {
    return TypeInfo;
  }

  CLIValueClassInitKind getInitKind() const { return InitKind; }

  Expr * getInitExpr() const { return InitExpr; }

  SourceRange getSourceRange() const LLVM_READONLY;

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CLIValueClassInitExprClass;
  }
  static bool classof(const CXXScalarValueInitExpr *) { return true; }

  // Iterators
  child_range children() { return child_range(); }
};

/// CLIPropertyRefExpr - A dot-syntax expression to access a CLI property.
class CLIPropertyRefExpr : public Expr {
private:
  CLIPropertyDecl *Property;
  DeclarationNameInfo PropertyNameInfo;

  Expr *Base;

  /// Indexing arguments used if property indexing.
  SmallVector<Expr *, 2> Args;

  bool IsIndexer : 1;

  bool IsArrow : 1;

  friend class ASTStmtReader;

public:
  /// \brief Create an expression which represents property reference/index.
  CLIPropertyRefExpr(CLIPropertyDecl *Decl,
                     DeclarationNameInfo NameInfo,
                     Expr *Base,
                     bool IsArrow,
                     QualType Type,
                     ExprValueKind VK,
                     ExprObjectKind OK) :
    Expr(CLIPropertyRefExprClass, Type, VK, OK, false, false,
      Type->isInstantiationDependentType(), false), Property(Decl),
      PropertyNameInfo(NameInfo), Base(Base), Args(0),
      IsIndexer(false), IsArrow(IsArrow) {
    assert(Type->isSpecificPlaceholderType(BuiltinType::PseudoObject));
  }

  /// \brief Create an expression which represents property reference/index.
  CLIPropertyRefExpr(CLIPropertyDecl *Decl,
                     Expr *Base,
                     SmallVector<Expr *, 2> Args,
                     QualType Type,
                     ExprValueKind VK,
                     ExprObjectKind OK) :
    Expr(CLIPropertyRefExprClass, Type, VK, OK, false, false,
      Type->isInstantiationDependentType(), false), Property(Decl),
      Base(Base), Args(Args), IsIndexer(true), IsArrow(false) {
    assert(Type->isSpecificPlaceholderType(BuiltinType::PseudoObject));
  }

  explicit CLIPropertyRefExpr(EmptyShell Shell)
    : Expr(CLIPropertyRefExprClass, Shell) { }

  CLIPropertyDecl *getProperty() const { return Property; }
  DeclarationNameInfo getPropertyNameInfo() const { return PropertyNameInfo; }

  Expr *getBase() const { return Base; }
  bool isArrow() const { return IsArrow; }

  const SmallVector<Expr *, 2> &getArgs() const { return Args; }
  void setArgs(SmallVector<Expr *, 2> &Args) { Args = Args; }

  bool isPropertyIndexing() const { return IsIndexer; }

  SourceRange getSourceRange() const LLVM_READONLY {
    return SourceRange();
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CLIPropertyRefExprClass;
  }
  static bool classof(const CLIPropertyRefExpr *) { return true; }

  // Iterators
  child_range children() { return child_range(); }
};

} // end namespace clang

#endif