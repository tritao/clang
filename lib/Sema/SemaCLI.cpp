//===--- SemaCLI.cpp - Semantic Analysis for C++/CLI-----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for C++/CLI language constructs.
//
//===----------------------------------------------------------------------===//

#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaCLI.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Scope.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclCLI.h"
#include "clang/AST/TypeCLI.h"
#include "clang/AST/CXXInheritance.h"
#include "llvm/ADT/StringExtras.h"
#include "TypeLocBuilder.h"

#using <System.dll>
using namespace System;
using namespace System::IO;
using namespace System::Collections::Generic;
using namespace System::Runtime::InteropServices;

#using <Mono.Cecil.dll>
using namespace Mono::Cecil;

#include "CLIInterop.h"
namespace clang {
using namespace sema;
using namespace clix;


} // end namespace clang