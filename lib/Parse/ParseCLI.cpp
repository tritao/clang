//===--- ParseCLI.cpp - C++/CLI Parsing -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the C++/CLI portions of the Parser interface.
//
//===----------------------------------------------------------------------===//

#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "RAIIObjectsForParser.h"
#include "clang/AST/DeclCLI.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/PrettyDeclStackTrace.h"
#include "clang/Sema/Scope.h"
#include "llvm/ADT/SmallVector.h"
using namespace clang;


/// \brief Parse a C++/CLI attribute-target.
///
/// attribute-target:
///     assembly
///     class
///     constructor
///     delegate
///     enum
///     event
///     field
///     interface
///     method
///     parameter
///     property
///     returnvalue
///     struct
/// 
static bool ParseCLIAttributeTarget(Parser &P, const Token &Tok,
                                    CLIAttributeTarget& Target) {
  switch(Tok.getKind()) {
  case tok::kw_class:
    Target = CLI_AT_class;
    return true;
  case tok::kw_enum:
    Target = CLI_AT_enum;
    return true;
  case tok::kw_struct:
    Target = CLI_AT_struct;
    return true;
  case tok::identifier: {
    IdentifierInfo *Id = Tok.getIdentifierInfo();
    if (Id == P.getCLIContextKeyword(Parser::cli_assembly))
      Target = CLI_AT_assembly;
    else if (Id == P.getCLIContextKeyword(Parser::cli_constructor))
      Target = CLI_AT_constructor;
    else if (Id == P.getCLIContextKeyword(Parser::cli_delegate))
      Target = CLI_AT_delegate;
    else if (Id == P.getCLIContextKeyword(Parser::cli_event))
      Target = CLI_AT_event;
    else if (Id == P.getCLIContextKeyword(Parser::cli_field))
      Target = CLI_AT_field;
    else if (Id == P.getCLIContextKeyword(Parser::cli_interface))
      Target = CLI_AT_interface;
    else if (Id == P.getCLIContextKeyword(Parser::cli_method))
      Target = CLI_AT_method;
    else if (Id == P.getCLIContextKeyword(Parser::cli_parameter))
      Target = CLI_AT_parameter;
    else if (Id == P.getCLIContextKeyword(Parser::cli_property))
      Target = CLI_AT_property;
    else if (Id == P.getCLIContextKeyword(Parser::cli_returnvalue))
      Target = CLI_AT_returnvalue;
    else
      return false;
    return true;
  }
  default:
    return false;
  }
}

/// \brief Parse a C++/CLI attribute specification.
/// 
/// attribute-section: 
///     '[' attribute-target-specifier[opt] attribute-list ']'
///
/// attribute-target-specifier:
///     attribute-target ':'
///
/// attribute-list:
///     attribute
///     attribute-list ',' attribute
///
/// attribute:
///     attribute-name attribute-arguments[opt]
///
/// attribute-name:
///     type-name 
///
/// attribute-arguments:
///     '(' positional-argument-list[opt] ')'
///     '(' positional-argument-list ',' named-argument-list ')'
///     '(' named-argument-list ')'
///
///  positional-argument-list:
///      positional-argument
///      positional-argument-list ',' positional-argument
///
/// positional-argument:
///     attribute-argument-expression
///
/// named-argument-list:
///     named-argument
///     named-argument-list ',' named-argument
///
/// named-argument:
///     identifier '=' attribute-argument-expression
///
/// attribute-argument-expression:
///     assignment-expression
///
void Parser::ParseCLIAttribute(ParsedAttributes &Attrs) {
  CLIAttributeTarget AttrTarget = CLI_AT_none;
  SourceLocation TargetLoc;

  ExprVector AttrArgs;
  if (NextToken().is(tok::colon)) {
    if (!ParseCLIAttributeTarget(*this, Tok, AttrTarget)) {
      Diag(Tok, Diags.getCustomDiagID(DiagnosticsEngine::Error,
        "unknown attribute target"));
      goto Exit;
    }

    TargetLoc = ConsumeToken();
    assert(Tok.is(tok::colon));
    ConsumeToken();
  }

  StringRef AttrName = Tok.getIdentifierInfo()->getName();
  ConsumeToken();

  if (Tok.is(tok::l_paren)) {
    BalancedDelimiterTracker T(*this, tok::l_paren);
    T.consumeOpen();

    if (Tok.isNot(tok::r_paren)) {
      CommaLocsTy CommaLocs;
      if (ParseExpressionList(AttrArgs, CommaLocs)) {
        goto Exit;
      }
    }
    T.consumeClose();
  }

#if 0
  Scope *S = getCurScope();
  for(auto it = S->decl_begin(); it != S->decl_end(); ++it) {
    auto D = *it;
    Decl::Kind K = D->getKind();
    D->getAccess();
  }
#endif

  ActionResult<CLICustomAttribute*> AttrRes = Actions.ActOnCLIAttribute(
    getCurScope(), AttrTarget, TargetLoc, AttrName, AttrArgs);

Exit:
  SkipUntil(tok::r_square, /*StopAtSemi=*/true, /*DontConsume=*/true);
}
