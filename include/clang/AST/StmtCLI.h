//===-- StmtCLI.h - Classes for representing C++/CLI statements ------=====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the C++/CLI Stmt subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_STMTCLI_H
#define LLVM_CLANG_AST_STMTCLI_H

#include "clang/AST/Stmt.h"
#include "llvm/Support/Compiler.h"

namespace clang {

/// CLIForEachStmt - This represents C++/CLI for each statement,
/// represented as 'for each ( in )'.
class CLIForEachStmt : public Stmt {
  enum { COLLECTION, BODY, END };
  Stmt *SubExprs[END];
  VarDecl *LoopVar;
  SourceLocation ForLoc;
  SourceLocation EachLoc;
  SourceLocation InLoc;
  SourceLocation RParenLoc;

public:
  CLIForEachStmt( VarDecl *LoopVar,
                  Stmt *Collection, Stmt *Body,
                  SourceLocation FL, SourceLocation EL, SourceLocation IL,
                  SourceLocation RPL);
  CLIForEachStmt(EmptyShell Empty) : Stmt(CLIForEachStmtClass, Empty) { }

  VarDecl *getLoopVariable();
  const VarDecl *getLoopVariable() const;
  
  Stmt *getCollection() { return SubExprs[COLLECTION]; }
  const Stmt *getCollection() const { return SubExprs[COLLECTION]; }
  void setCollection(Stmt *S) { SubExprs[COLLECTION] = S; }

  Stmt *getBody() { return SubExprs[BODY]; }
  const Stmt *getBody() const { return SubExprs[BODY]; }
  void setBody(Stmt *S) { SubExprs[BODY] = S; }

  SourceLocation getForLoc() const { return ForLoc; }
  void setForLoc(SourceLocation Loc) { ForLoc = Loc; }
  SourceLocation getEachLoc() const { return EachLoc; }
  void setEachLoc(SourceLocation Loc) { EachLoc = Loc; }
  SourceLocation getInLoc() const { return InLoc; }
  void setInLoc(SourceLocation Loc) { InLoc = Loc; }
  SourceLocation getRParenLoc() const { return RParenLoc; }
  void setRParenLoc(SourceLocation Loc) { RParenLoc = Loc; }

  SourceLocation getLocStart() const LLVM_READONLY { return ForLoc; }
  SourceLocation getLocEnd() const LLVM_READONLY {
    return SubExprs[BODY]->getLocEnd();
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == CLIForEachStmtClass;
  }

  // Iterators
  child_range children() {
    return child_range(&SubExprs[0], &SubExprs[END]);
  }
};

} // end namespace clang

#endif