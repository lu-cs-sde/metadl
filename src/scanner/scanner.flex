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
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]
Comment        = "#" {InputCharacter}* {LineTerminator}?
VAR_ID = "$"?[a-z][a-zA-Z0-9_]*
PRED_ID = [A-Z][a-zA-Z0-9_]*
PRED_REF = '{PRED_ID}
Pattern = "<:" ~":>"
Numeral = [0-9]+
String  = \"[^\"]*\"
%%

// discard whitespace information
{WhiteSpace}  { }
{Comment}     { }

// token definitions
"("        {  return  sym(Terminals.LPARA);          }
")"        {  return  sym(Terminals.RPARA);          }
"["        {  return  sym(Terminals.LBRACK);         }
"]"        {  return  sym(Terminals.RBRACK);         }
"{"        {  return  sym(Terminals.LBRACE);         }
"}"        {  return  sym(Terminals.RBRACE);         }
":-"       {  return  sym(Terminals.IMPLIED_BY);     }
"."        {  return  sym(Terminals.DOT);            }
","        {  return  sym(Terminals.COMMA);          }
"+"        {  return  sym(Terminals.ADD);            }
"-"        {  return  sym(Terminals.SUB);            }
"*"        {  return  sym(Terminals.MUL);            }
"/"        {  return  sym(Terminals.DIV);            }
"EQ"       {  return  sym(Terminals.EQ);             }
"NEQ"      {  return  sym(Terminals.NEQ);            }
"LT"       {  return  sym(Terminals.LT);             }
"LTE"      {  return  sym(Terminals.LTE);            }
"GT"       {  return  sym(Terminals.GT);             }
"GTE"      {  return  sym(Terminals.GTE);            }
"EDB"      {  return  sym(Terminals.EDB);            }
"OUTPUT"   {  return  sym(Terminals.OUTPUT);         }
"NOT"      {  return  sym(Terminals.NOT);            }
"BIND"     {  return  sym(Terminals.BIND);           }
"IMPORT"   {  return  sym(Terminals.IMPORT);         }
"analyze"  {  return  sym(Terminals.ANALYZE);        }
{Numeral}  {  return  sym(Terminals.NUMERAL);        }
{VAR_ID}   {  return  sym(Terminals.VAR_ID);         }
{PRED_ID}  {  return  sym(Terminals.PRED_ID);        }
{PRED_REF} {
                String text = yytext();
                String data = text.substring(1, text.length());
                return new beaver.Symbol(Terminals.PRED_REF, yyline + 1, yycolumn + 1, yylength() - 1, data);
           }

{Pattern}  {
                // Remove Quotes from Matched String.
                String text = yytext();
                String data = text.substring(2, text.length() - 2);
                return new beaver.Symbol(Terminals.PATTERN, yyline + 1, yycolumn + 1, yylength() - 3, data);
           }

{String}   {
                // Remove Quotes from Matched String.
                String text = yytext();
                String data = text.substring(1, text.length() - 1);
                return new beaver.Symbol(Terminals.STRING, yyline + 1, yycolumn + 1, yylength() - 2, data);
           }
<<EOF>>    {  return  sym(Terminals.EOF);        }

/* error fallback */
[^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
