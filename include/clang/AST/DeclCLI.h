//===-- DeclCLI.h - Classes for representing C++/CLI declarations ------=====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the C++/CLI Decl subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_DECLCLI_H
#define LLVM_CLANG_AST_DECLCLI_H

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Redeclarable.h"
#include "clang/AST/DeclarationName.h"

namespace clang {

enum CLIPropertyKind {
  CLI_PK_Auto,
  CLI_PK_Getter,
  CLI_PK_Setter,
};

/// CLIPropertyDecl - Represent a C++/CLI property.
class CLIPropertyDecl : public ValueDecl {
  virtual void anchor() LLVM_OVERRIDE;

  CLIPropertyDecl(DeclContext *DC, DeclarationName DN, QualType Ty);

public:

  CXXMethodDecl *GetMethod; // Declaration of getter instance method
  CXXMethodDecl *SetMethod; // Declaration of setter instance method
  FieldDecl* Field;
  SmallVector<QualType, 2> IndexerTypes;

  bool isIndexer() const { return !IndexerTypes.empty(); }

  static CLIPropertyDecl *Create(ASTContext &C, DeclContext *DC,
    DeclarationName DN, QualType Ty);

  static CLIPropertyDecl *CreateDeserialized(ASTContext &C, unsigned ID);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classof(const CLIPropertyDecl *D) { return true; }
  static bool classofKind(Kind K) { return K == CLIProperty; }

  friend class ASTDeclReader;
  friend class ASTDeclWriter;
};

} // end namespace clang

#endif