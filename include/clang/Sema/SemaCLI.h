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
#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaDiagnostic.h"

namespace clang {

struct CLIPrimitiveType {
  int Token;
  int Hash;
  clang::QualType Ty;
  clang::CXXRecordDecl *Decl;
};

struct CLIPrimitiveTypes {
#define CLI_TYPE(X) \
  CLIPrimitiveType X;
#include "clang/AST/CLITypes.def"
#undef CLI_TYPE
};

class CLICecilContext;

class CLISemaContext {
public:
  CLISemaContext() : CLINamespace(0), Array(0), InteriorPtr(0),
    PinPtr(0), SafeCast(0), ParamArrayAttribute(0),
    IEnumerable(0), GenericIEnumerable(0), CecilContext(nullptr) {
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

  CLICecilContext *CecilContext;
};

bool HasCLIParamArrayAttribute(Sema &S, const FunctionDecl* Fn,
                               QualType& Type);

class CLIArrayConvertDiagnoser : public Sema::ICEConvertDiagnoser {
    Expr *Index;
      
public:
    CLIArrayConvertDiagnoser(Expr *ArraySize)
    : ICEConvertDiagnoser(false, false, false), Index(Index) { }
      
    virtual Sema::SemaDiagnosticBuilder diagnoseNotInt(Sema &S,
                                                       SourceLocation Loc,
                                                       QualType T) {
      return S.Diag(Loc, diag::err_array_size_not_integral)
               << S.getLangOpts().CPlusPlus11 << T;
    }
      
    virtual Sema::SemaDiagnosticBuilder diagnoseIncomplete(Sema &S,
                                                           SourceLocation Loc,
                                                           QualType T) {
      return S.Diag(Loc, diag::err_array_size_incomplete_type)
               << T << Index->getSourceRange();
    }
      
    virtual Sema::SemaDiagnosticBuilder diagnoseExplicitConv(Sema &S,
                                                             SourceLocation Loc,
                                                             QualType T,
                                                             QualType ConvTy) {
      return S.Diag(Loc, diag::err_array_size_explicit_conversion)
               << T << ConvTy;
    }
      
    virtual Sema::SemaDiagnosticBuilder noteExplicitConv(Sema &S,
                                                         CXXConversionDecl *Conv,
                                                         QualType ConvTy) {
      return S.Diag(Conv->getLocation(), diag::note_array_size_conversion)
               << ConvTy->isEnumeralType() << ConvTy;
    }
      
    virtual Sema::SemaDiagnosticBuilder diagnoseAmbiguous(Sema &S,
                                                          SourceLocation Loc,
                                                          QualType T) {
      return S.Diag(Loc, diag::err_array_size_ambiguous_conversion) << T;
    }
      
    virtual Sema::SemaDiagnosticBuilder noteAmbiguous(Sema &S,
                                                      CXXConversionDecl *Conv,
                                                      QualType ConvTy) {
      return S.Diag(Conv->getLocation(), diag::note_array_size_conversion)
               << ConvTy->isEnumeralType() << ConvTy;
    }
      
    virtual Sema::SemaDiagnosticBuilder diagnoseConversion(Sema &S,
                                                           SourceLocation Loc,
                                                           QualType T,
                                                           QualType ConvTy) {
      return S.Diag(Loc,
                    S.getLangOpts().CPlusPlus11
                    ? diag::warn_cxx98_compat_array_size_conversion
                    : diag::ext_array_size_conversion)
               << T << ConvTy->isEnumeralType() << ConvTy;
    }
};

}  // end namespace clang

#endif