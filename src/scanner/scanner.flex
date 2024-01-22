package lang.ast; // The generated scanner will belong to the package lang.ast

import lang.ast.LangParser.Terminals; // The terminals are implicitly defined in the parser
import lang.ast.LangParser.SyntaxError;

%%

%state PATTERN

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
  StringBuffer strbuf = new StringBuffer(128);
  int sub_line;
  int sub_column;

  /** String literal start position. */
  int stringStartLine, stringStartColumn;

  private beaver.Symbol sym(short id) {
    return new beaver.Symbol(id, yyline + 1, yycolumn + 1, len(), str());
  }

  private beaver.Symbol sym(short id, String value) {
    return new beaver.Symbol(id, yyline + 1, yycolumn + 1, len(), value);
  }

  private beaver.Symbol sym(short id, String value, int start_line, int start_column, int len) {
    return new beaver.Symbol(id, start_line, start_column, len, value);
  }

  private String str() { return yytext(); }
  private int len() { return yylength(); }

  private void error(String msg) throws beaver.Scanner.Exception {
    throw new beaver.Scanner.Exception(yyline + 1, yycolumn + 1, msg);
  }

  private void patternStart() {
    yybegin(PATTERN);
    stringStartLine = yyline + 1;
    stringStartColumn = yycolumn + 1;
    strbuf.setLength(0);
  }

  private beaver.Symbol patternEnd() {
    yybegin(YYINITIAL);
    String text = strbuf.toString();
    int length = text.length() + 2;
    return new beaver.Symbol(Terminals.PATTERN, stringStartLine, stringStartColumn, length, text);
  }
%}

// macros
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]
Comment        = "#" {InputCharacter}* {LineTerminator}? | "/*" ~ "*/" | "//" {InputCharacter}* {LineTerminator}?
MetaVarPrefix = "$" | "`"
VAR_ID = {MetaVarPrefix}?[a-z][a-zA-Z0-9_]*
PRED_ID = [A-Z][a-zA-Z0-9_]*
PRED_REF = '{PRED_ID}
Numeral = [0-9]+
String  = \"[^\"]*\"
%%


<YYINITIAL> {
// discard whitespace information
{WhiteSpace}  { }
{Comment}     { }

// token definitions
"<:"       {  patternStart();                        }
"=="       {  return  sym(Terminals.IEQEQ);          }
"="        {  return  sym(Terminals.IEQ);            }
"!="       {  return  sym(Terminals.INEQ);           }
"("        {  return  sym(Terminals.LPARA);          }
")"        {  return  sym(Terminals.RPARA);          }
":-"       {  return  sym(Terminals.IMPLIED_BY);     }
"."        {  return  sym(Terminals.DOT);            }
","        {  return  sym(Terminals.COMMA);          }
"+"        {  return  sym(Terminals.ADD);            }
"-"        {  return  sym(Terminals.SUB);            }
"*"        {  return  sym(Terminals.MUL);            }
"/"        {  return  sym(Terminals.DIV);            }
"<"        {  return  sym(Terminals.ILT);            }
">"        {  return  sym(Terminals.IGT);            }
">="       {  return  sym(Terminals.IGTE);           }
"<="       {  return  sym(Terminals.ILTE);           }
"@"        {  return  sym(Terminals.AT);             }
"!"        {  return  sym(Terminals.EXCL);           }
"EQ"       {  return  sym(Terminals.EQ);             }
"NEQ"      {  return  sym(Terminals.NEQ);            }
"LT"       {  return  sym(Terminals.LT);             }
"LTE"      {  return  sym(Terminals.LTE);            }
"GT"       {  return  sym(Terminals.GT);             }
"GTE"      {  return  sym(Terminals.GTE);            }
"MATCH"    {  return  sym(Terminals.MATCH);          }
"NOT"      {  return  sym(Terminals.NOT);            }
"BIND"     {  return  sym(Terminals.BIND);           }
"ANCESTOR" {  return  sym(Terminals.ANCESTOR);       }
"_"        {  return  sym(Terminals.WILDCARD);       }
"undef"    {  return  sym(Terminals.UNDEF);          }
"inline"   {  return  sym(Terminals.INLINE);         }
{Numeral}  {  return  sym(Terminals.NUMERAL);        }
{VAR_ID}   {  return  sym(Terminals.VAR_ID);         }
{PRED_ID}  {  return  sym(Terminals.PRED_ID);        }
{PRED_REF} {
                String text = yytext();
                String data = text.substring(1, text.length());
                return new beaver.Symbol(Terminals.PRED_REF, yyline + 1, yycolumn + 1, yylength() - 1, data);
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
}

<PATTERN> {
  "\\:>"   { strbuf.append(":>"); }
  ":>"    { return patternEnd(); }
  {InputCharacter}  { strbuf.append(str()); }
  {LineTerminator}  { strbuf.append(str()); }
}
