/* C tokens */
/* Adapted from SuperC/src/superc/parser/lexer.l */

package lang.c.obj.ast; // The generated scanner will belong to the package lang.ast

import lang.c.obj.ast.ObjLangParserSEP.Terminals; // The terminals are implicitly defined in the parser
import lang.c.obj.ast.ObjLangParserSEP.SyntaxError;
%%

// define the signature for the generated scanner
%public
%final
%class ObjLangScanner
%extends beaver.Scanner
