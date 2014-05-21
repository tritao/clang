set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_library(clangSema
  AnalysisBasedWarnings.cpp
  AttributeList.cpp
  CodeCompleteConsumer.cpp
  DeclSpec.cpp
  DelayedDiagnostic.cpp
  IdentifierResolver.cpp
  JumpDiagnostics.cpp
  MultiplexExternalSemaSource.cpp
  Scope.cpp
  ScopeInfo.cpp
  Sema.cpp
  SemaAccess.cpp
  SemaAttr.cpp
  SemaCLI.cpp
  SemaCXXScopeSpec.cpp
  SemaCast.cpp
  SemaChecking.cpp
  SemaCodeComplete.cpp
  SemaConsumer.cpp
  SemaDecl.cpp
  SemaDeclAttr.cpp
  SemaDeclCXX.cpp
  SemaDeclObjC.cpp
  SemaExceptionSpec.cpp
  SemaExpr.cpp
  SemaExprCXX.cpp
  SemaExprMember.cpp
  SemaExprObjC.cpp
  SemaFixItUtils.cpp
  SemaInit.cpp
  SemaLambda.cpp
  SemaLookup.cpp
  SemaObjCProperty.cpp
  SemaOpenMP.cpp
  SemaOverload.cpp
  SemaPseudoObject.cpp
  SemaStmt.cpp
  SemaStmtAsm.cpp
  SemaStmtAttr.cpp
  SemaTemplate.cpp
  SemaTemplateDeduction.cpp
  SemaTemplateInstantiate.cpp
  SemaTemplateInstantiateDecl.cpp
  SemaTemplateVariadic.cpp
  SemaType.cpp
  TypeLocBuilder.cpp

  LINK_LIBS
  clangAST
  clangAnalysis
  clangBasic
  clangEdit
  clangLex
  )


# VS flags for CLI compiler,
if (MSVC)
  # # remove /RTC1 (runtime checks)
  set(CMAKE_CXX_FLAGS_DEBUG "/D_DEBUG /MDd /Zi /Ob0 /Od")
  set(CMAKE_C_FLAGS_DEBUG "/D_DEBUG /MDd /Zi /Ob0 /Od")
  
  # add runtime checks for all files except SemaCLI
  set_source_files_properties(
    AnalysisBasedWarnings.cpp AttributeList.cpp CodeCompleteConsumer.cpp 
    DeclSpec.cpp DelayedDiagnostic.cpp IdentifierResolver.cpp JumpDiagnostics.cpp
    MultiplexExternalSemaSource.cpp    Scope.cpp ScopeInfo.cpp Sema.cpp SemaAccess.cpp
    SemaAttr.cpp SemaCXXScopeSpec.cpp SemaCast.cpp SemaChecking.cpp SemaCodeComplete.cpp
    SemaConsumer.cpp SemaDecl.cpp SemaDeclAttr.cpp SemaDeclCXX.cpp SemaDeclObjC.cpp
    SemaExceptionSpec.cpp SemaExpr.cpp SemaExprCXX.cpp SemaExprMember.cpp SemaExprObjC.cpp
    SemaFixItUtils.cpp SemaInit.cpp SemaLambda.cpp SemaLookup.cpp SemaObjCProperty.cpp
    SemaOpenMP.cpp SemaOverload.cpp SemaPseudoObject.cpp SemaStmt.cpp SemaStmtAsm.cpp
    SemaStmtAttr.cpp SemaTemplate.cpp SemaTemplateDeduction.cpp SemaTemplateInstantiate.cpp
    SemaTemplateInstantiateDecl.cpp SemaTemplateVariadic.cpp SemaType.cpp TypeLocBuilder.cpp
       PROPERTIES COMPILE_FLAGS "/RTC1")
  
  # add necessary flags to SemaCLI, /CLR, /GR, and Cecil dep
  set_source_files_properties(SemaCLI.cpp PROPERTIES COMPILE_FLAGS "/clr /AI\"${CLANG_SOURCE_DIR}/deps/cecil/\" /GR")

endif()

