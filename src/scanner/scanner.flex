package lang.ast; // The generated scanner will belong to the package lang.ast

import lang.ast.LangParser.Terminals; // The terminals are implicitly defined in the parser
import lang.ast.LangParser.SyntaxError;

%%

// define the signature for the generated scanner
%public
%final
%class LangScanner
%extends beaver.Scanner

// the interface between the scanner and the parser is the nextToken() method
%type beaver.Symbol 
%function nextToken 

// store line and column information in the tokens
%line
%column

// this code will be inlined in the body of the generated scanner class
%{
  private beaver.Symbol sym(short id) {
    return new beaver.Symbol(id, yyline + 1, yycolumn + 1, yylength(), yytext());
  }
%}

// macros
WhiteSpace = [ ] | \t | \f | \n | \r | \/\/[^\n<<EOF>>]*
VAR_ID = [a-z][a-zA-Z0-9]*
PRED_ID = [A-Z][a-zA-Z0-9]*
Numeral = [0-9]+
String  = \"[^\"]*\"

%%

// discard whitespace information
{WhiteSpace}  { }

// token definitions
"("        {  return  sym(Terminals.LPARA);      }
")"        {  return  sym(Terminals.RPARA);      }
":-"       {  return  sym(Terminals.IMPLIED_BY); }
"."        {  return  sym(Terminals.DOT);        }
","        {  return  sym(Terminals.COMMA);      }
"EDB"      {  return  sym(Terminals.EDB);        }
"NOT"      {  return  sym(Terminals.NOT);        }
{Numeral}  {  return  sym(Terminals.NUMERAL);    }
{VAR_ID}   {  return  sym(Terminals.VAR_ID);     }
{PRED_ID}  {  return  sym(Terminals.PRED_ID);    }
{String}   {
                // Remove Quotes from Matched String.
                String text = yytext();
                String data = text.substring(1, text.length() - 1);
                return new beaver.Symbol(Terminals.STRING, yyline + 1, yycolumn + 1, yylength() - 2, data); 
           }
<<EOF>>    {  return  sym(Terminals.EOF);        }

/* error fallback */
[^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
