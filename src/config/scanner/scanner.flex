package lang.ast.config; // The generated scanner will belong to the package lang.ast

import lang.ast.config.ConfigParser.Terminals; // The terminals are implicitly defined in the parser
import lang.ast.config.ConfigParser.SyntaxError;

%%

// define the signature for the generated scanner
%public
%final
%class ConfigScanner 
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
ID = [a-zA-Z0-9\?_]+
Numeral = [0-9]+
String  = \"[^\"]*\"
FilePath = [^\s(::)]+

%%

// discard whitespace information
{WhiteSpace}  { }

// token definitions

"-OUT"          {  return  sym(Terminals.OUTDIR);        }
"-FACTS"        {  return  sym(Terminals.FACTSDIR);      }
"-SOUFFLEOUT"   {  return  sym(Terminals.SOUFFLEOUT);    }
"pretty"        {  return  sym(Terminals.PRETTYPRINT); }
"eval"          {  return  sym(Terminals.EVAL); }
"typeprog"      {  return  sym(Terminals.TYPE_PROG); }
"::"            {  return  sym(Terminals.SEP);           }
"bottomupnaive" {  return  sym(Terminals.BOTTOMUPNAIVE); }
"souffle"       {  return  sym(Terminals.SOUFFLE);       }
{FilePath}      {  return  sym(Terminals.FILEPATH);      }
<<EOF>>         {  return  sym(Terminals.EOF);           }

/* error fallback */
[^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
