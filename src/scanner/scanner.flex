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
ID = [a-zA-Z][a-zA-Z0-9]*
Numeral = [0-9]+ ("." [0-9]+)?

%%

// discard whitespace information
{WhiteSpace}  { }

// token definitions
"("        {  return  sym(Terminals.LPARA);      }
")"        {  return  sym(Terminals.RPARA);      }
"{"        {  return  sym(Terminals.LBRACE);     }
"}"        {  return  sym(Terminals.RBRACE);     }
"="        {  return  sym(Terminals.EQ);         }
"+"        {  return  sym(Terminals.PLUS);       }
"-"        {  return  sym(Terminals.SUB);        }
"*"        {  return  sym(Terminals.MUL);        }
"/"        {  return  sym(Terminals.DIV);        }
"%"        {  return  sym(Terminals.MOD);        }
"<"        {  return  sym(Terminals.LT);         }
"<="       {  return  sym(Terminals.LTEQ);       }
">"        {  return  sym(Terminals.GT);         }
">="       {  return  sym(Terminals.GTEQ);       }
"=="       {  return  sym(Terminals.LOEQ);       }
"!="       {  return  sym(Terminals.LONOTEQ);    }
";"        {  return  sym(Terminals.SEMICOLON);  }
","        {  return  sym(Terminals.COMMA);      }
"true"     {  return  sym(Terminals.TRUE);       }
"false"    {  return  sym(Terminals.FALSE);      }
"while"    {  return  sym(Terminals.WHILE);      }
"return"   {  return  sym(Terminals.RETURN);     }
"if"       {  return  sym(Terminals.IF);         }
"else"     {  return  sym(Terminals.ELSE);       }
{ID}       {  return  sym(Terminals.ID);         }
{Numeral}  {  return  sym(Terminals.NUMERAL);    }
<<EOF>>    {  return  sym(Terminals.EOF);        }

/* error fallback */
[^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
