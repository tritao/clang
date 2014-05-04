//===--- TypeCLI.h - C++/CLI Language Type Representation -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the C++/CLI Type interface and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_TYPECLI_H
#define LLVM_CLANG_AST_TYPECLI_H

//#include "clang/AST/Type.h"
//
//namespace clang {

enum CLITypeKind {
  CLI_TK_None,
#define CLI_TYPE(X) \
    CLI_TK_##X,
#include "clang/AST/CLITypes.def"
#undef CLI_TYPE
};

/// CLIArrayType - This class represents the canonical version of C++/CLI
/// arrays with a specified type and rank.
class CLIArrayType : public RecordType {
  /// ElementType - The element type of the array.
  QualType ElementType;
  /// Rank of the array.
  unsigned Rank;

protected:
  CLIArrayType(QualType ET, unsigned Rank, const RecordDecl *D)
    : RecordType(CLIArray, const_cast<RecordDecl *>(D)),
      ElementType(ET),
      Rank(Rank) {}
  friend class ASTContext;  // ASTContext creates these.

public:
  QualType getElementType() const { return ElementType; }
  unsigned getRank() const { return Rank; }

  bool isSugared() const { return false; }
  QualType desugar() const { return QualType(this, 0); }

  static bool classof(const Type *T) { return T->getTypeClass() == CLIArray; }
  static bool classof(const CLIArrayType *) { return true; }
};

//}  // end namespace clang

#endif