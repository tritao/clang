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

Parser::CLIContextSensitiveKeywords Parser::ConvertTokenToCLITagKeyword(
                                                       const Token& Tok) {
  if (IdentifierInfo *II = Tok.getIdentifierInfo()) {
    if (II == CLIContextKeywords[cli_ref])
      return cli_ref;
    else if (II == CLIContextKeywords[cli_value])
      return cli_value;
    else if (II == CLIContextKeywords[cli_interface])
      return cli_interface;
  }
  return cli_NumKeywords;
}

/// \brief Parse a C++/CX aggregate keyword.
bool Parser::ParseAggregateClassKeywords(const Token& Tok1, const Token& Tok2,
                                         Token& Res) { 
  CLIContextSensitiveKeywords TagKw = ConvertTokenToCLITagKeyword(Tok1);
  if (TagKw == cli_NumKeywords) return false;

  // If we have a context sensitive keyword, check the next token to see
  // if it is a valid C++/CX tag type specifier combination.

  tok::TokenKind Kind = tok::unknown;
  tok::TokenKind NextKind = Tok2.getKind();

  switch(TagKw) {
  case cli_ref:
    if (NextKind == tok::kw_class)
      Kind = tok::kw_ref_class;
    else if(NextKind == tok::kw_struct)
      Kind = tok::kw_ref_struct;
    break;
  case cli_value:
    if (NextKind == tok::kw_class)
      Kind = tok::kw_value_class;
    else if(NextKind == tok::kw_struct)
      Kind = tok::kw_value_struct;
    break;
  case cli_interface:
    if (NextKind == tok::kw_class)
      Kind = tok::kw_interface_class;
    else if(NextKind == tok::kw_struct)
      Kind = tok::kw_interface_struct;
    break;
  default: break;
  }

  if (Kind == tok::unknown)
    return false;

  SourceManager &S = getPreprocessor().getSourceManager();
  const unsigned TokLen = S.getFileOffset(Tok2.getLastLoc())
      - S.getFileOffset(Tok1.getLocation());
  
  // Initialize a new token consisting of the aggregate keywords.
  Res.startToken();
  Res.setKind(Kind);
  Res.setLocation(Tok1.getLocation());
  Res.setLength(TokLen);

  return true;
}

/// \brief Parse a C++/CX tag visibility keyword.
bool Parser::ParseTagVisibility(AccessSpecifier& Visibility,
                                SourceLocation& Loc) {
  Visibility = AS_private;

  // Try to parse the top level visibility of the class.
  Loc = Tok.getLocation();
  AccessSpecifier Acc = getAccessSpecifierIfPresent();
  
  if (Acc == AS_none)
    return false;

  if (Acc == AS_public || Acc == AS_private)
    Visibility = Acc;
  else
    Diag(diag::err_cx_invalid_class_access_specifier)
                        << GetAccessSpecifierName(Acc)
                        << FixItHint::CreateReplacement(Loc, "private");
  return true;
}

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

/// \brief Determine whether the given token is a C++/CLI virt-specifier.
///
///       virt-specifier:
///         abstract
///         new
///         sealed
///         override
VirtSpecifiers::Specifier Parser::isCLIVirtSpecifier(const Token &Tok) const {
  if (Tok.is(tok::identifier)) {
    IdentifierInfo *II = Tok.getIdentifierInfo();

    if (II == CLIContextKeywords[cli_abstract])
      return VirtSpecifiers::VS_Abstract;
    if (II == CLIContextKeywords[cli_sealed])
      return VirtSpecifiers::VS_Sealed;
    else if (II == CLIContextKeywords[cli_override])
      return VirtSpecifiers::VS_Override;
  } else if (Tok.is(tok::kw_new)) {
      return VirtSpecifiers::VS_New;
  }

  return VirtSpecifiers::VS_None;
}