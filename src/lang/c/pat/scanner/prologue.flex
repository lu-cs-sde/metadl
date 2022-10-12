/* C tokens */
/* Adapted from SuperC/src/superc/parser/lexer.l */

package lang.c.pat.ast; // The generated scanner will belong to the package lang.ast

import lang.c.pat.ast.PatLangParserSEP.Terminals; // The terminals are implicitly defined in the parser
import lang.c.pat.ast.PatLangParserSEP.SyntaxError;
%%

// define the signature for the generated scanner
%public
%final
%class PatLangScanner
%extends beaver.Scanner
