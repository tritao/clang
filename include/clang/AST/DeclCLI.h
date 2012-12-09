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

typedef CXXRecordDecl CLIRecordDecl;

enum CLIGenericConstraintFlags {
  // ILasm notation in comments
  CLI_GCK_Covariant     = 0x01, // +
  CLI_GCK_Contravariant = 0x02, // -
  CLI_GCK_ReferenceTypeConstraint        = 0x04, // class
  CLI_GCK_NotNullableValueTypeConstraint = 0x08, // valuetype
  CLI_GCK_DefaultConstructorConstraint   = 0x10, // .ctor
};

struct CLIGenericParameter {
  std::string Name;
  unsigned char Flags; // CLIGenericConstraintFlags
};

enum CLIRecordType {
  CLI_RT_ValueType,
  CLI_RT_ReferenceType,
  CLI_RT_InterfaceType
};

class CLIGenericData {
public:

  llvm::SmallVector<CLIGenericParameter, 4> Parameters;
};

class CLIDefinitionData {
public:
  CLIDefinitionData() : Kind(CLI_TK_None), GenericData(0) { }

  /// Full name of the assembly.
  std::string FullName;

  /// Name of the assembly DLL.
  std::string AssemblyName;

  /// IR mangled name.
  std::string IRName;

  /// CLI record type.
  CLIRecordType Type;

  /// CLI type kind.
  CLITypeKind Kind;

  /// CLI generic definition data.
  CLIGenericData *GenericData;

  /// Gets the CLI generic definition data.
  CLIGenericData *getGenericData() const { return GenericData; }

  /// Sets the CLI generic definition data.
  void setGenericData(CLIGenericData *D) { GenericData = D; }

  /// Checks if record has CLI generic definition data.
  bool isGeneric() { return getGenericData() != 0; }

  /// Interfaces of this record.
  typedef SmallVector<CXXBaseSpecifier *, 2> InterfacesVector;
  InterfacesVector Interfaces;

  /// Sets the interfaces of this struct or class.
  void setInterfaces(InterfacesVector &);

  /// Retrieves the number of interfaces of this class.
  unsigned getNumInterfaces() const { return Interfaces.size(); }
};

} // end namespace clang

#endif