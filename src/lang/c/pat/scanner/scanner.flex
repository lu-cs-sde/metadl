/* C tokens */
/* Adapted from SuperC/src/superc/parser/lexer.l */

package lang.c.pat.ast; // The generated scanner will belong to the package lang.ast

import lang.c.pat.ast.PatLangParserSEP.Terminals; // The terminals are implicitly defined in the parser
import lang.c.pat.ast.PatLangParserSEP.SyntaxError;
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

%state COMMENT

// macros
identifier=[a-zA-Z_][0-9a-zA-Z_]*

/* Define floating point number constants. */
exponent_part=([eE]|[pP])[-+]?[0-9]+
fractional_constant=([0-9]*"."[0-9]+)|([0-9]+".")
floating_constant=(({fractional_constant}{exponent_part}?)|([0-9]+{exponent_part}))[FfLl]?

/* Define a catch-all for preprocessor numbers.  These may not
necessarily be valid C numeric constants, but they are valid
preprocessor numbers. */
preprocessing_number="."?[0-9]+([0-9a-zA-Z._]|{exponent_part})*

/* Define numeric constants. */
integer_suffix_opt=([uU]?[lL]?)|([lL][uU])|([uU]("ll"|"LL"))|(("ll"|"LL")[uU]|"LL"|"ll"|"UU"|"uu")
integer_constant=[1-9][0-9]*{integer_suffix_opt}
octal_constant="0"[0-7]*{integer_suffix_opt}
hex_constant="0"[xX][0-9a-fA-F]+{integer_suffix_opt}

/* Define escape sequences. */
simple_escape=[abefnrtv\'\"?\\@]
octal_escape=[0-7]{1,3}
hex_escape="x"[0-9a-fA-F]+
escape_sequence=[\\]({simple_escape}|{octal_escape}|{hex_escape})

/* Define character and string literals.  A single character in
literals is a normal character (without newlines or quotes), an escape
sequence, or a line continuation.  The continuation is preserved to
preserve layout.  */
c_char=[^\'\\\n]|{escape_sequence}|\\\n
s_char=[^\"\\\n]|{escape_sequence}|\\\n


// Define whitespace characters.
h_tab=[\011]
form_feed=[\014]
v_tab=[\013]
c_return=[\015]
horizontal_white=[ ]|{h_tab}
newline=\r|\n|\r\n
//comment=[/][*]([^*/]|[^*][/]|[*][^/])*[*][/]
linecomment="//"[^\r\n]*{newline}
continuation="\\\n"
whitespace=({horizontal_white})+|({v_tab}|{c_return})+|{continuation}

%%

// discard whitespace information
<YYINITIAL> {
// JFLex increments its line counter for form-feeds, but GCC does not.
{form_feed}
{ yyline--; return new Layout(yytext(), false); }

{whitespace}+
{ return new Layout(yytext(), false); }

{whitespace}*({newline}|{linecomment})+{whitespace}*
{ return new Layout(yytext(), true); }


LANGUAGE(AUTO,"auto")
LANGUAGE(BREAK,"break")
LANGUAGE(CASE,"case")
LANGUAGE(CHAR,"char")
LANGUAGE(CONST,"const")
LANGUAGE(CONTINUE,"continue")
LANGUAGE(DEFAULT,"default")
LANGUAGE(DO,"do")
LANGUAGE(DOUBLE,"double")
LANGUAGE(ELSE,"else")
LANGUAGE(ENUM,"enum")
LANGUAGE(EXTERN,"extern")
LANGUAGE(FLOAT,"float")
LANGUAGE(FOR,"for")
LANGUAGE(GOTO,"goto")
LANGUAGE(IF,"if")
LANGUAGE(INT,"int")
LANGUAGE(LONG,"long")
LANGUAGE(REGISTER,"register")
LANGUAGE(RETURN,"return")
LANGUAGE(SHORT,"short")
LANGUAGE(SIGNED,"signed")
LANGUAGE(SIZEOF,"sizeof")
LANGUAGE(STATIC,"static")
LANGUAGE(STRUCT,"struct")
LANGUAGE(SWITCH,"switch")
LANGUAGE(TYPEDEF,"typedef")
LANGUAGE(UNION,"union")
LANGUAGE(UNSIGNED,"unsigned")
LANGUAGE(VOID,"void")
LANGUAGE(VOLATILE,"volatile")
LANGUAGE(WHILE,"while")

//C99 keywords used by the default gnu89 dialect
LANGUAGE(_BOOL,"_Bool")
LANGUAGE(_COMPLEX,"_Complex")
LANGUAGE(INLINE,"inline")

// LANGUAGE(RESTRICT,"restrict") // A C99 keyword not used in gnu89.

//GCC
LANGUAGE(__ALIGNOF,"__alignof")
LANGUAGE(__ALIGNOF__,"__alignof__")
LANGUAGE(ASM,"asm")
LANGUAGE(__ASM,"__asm")
LANGUAGE(__ASM__,"__asm__")
LANGUAGE(__ATTRIBUTE,"__attribute")
LANGUAGE(__ATTRIBUTE__,"__attribute__")
LANGUAGE(__BUILTIN_OFFSETOF,"__builtin_offsetof")
LANGUAGE(__BUILTIN_TYPES_COMPATIBLE_P,"__builtin_types_compatible_p")
LANGUAGE(__BUILTIN_VA_ARG,"__builtin_va_arg")
LANGUAGE(__BUILTIN_VA_LIST,"__builtin_va_list")
LANGUAGE(__COMPLEX__,"__complex__")
LANGUAGE(__CONST,"__const")
LANGUAGE(__CONST__,"__const__")
LANGUAGE(__EXTENSION__,"__extension__")
LANGUAGE(__INLINE,"__inline")
LANGUAGE(__INLINE__,"__inline__")
LANGUAGE(__LABEL__,"__label__")
LANGUAGE(__RESTRICT,"__restrict")
LANGUAGE(__RESTRICT__,"__restrict__")
LANGUAGE(__SIGNED,"__signed")
LANGUAGE(__SIGNED__,"__signed__")
LANGUAGE(__THREAD,"__thread")
LANGUAGE(TYPEOF,"typeof")
LANGUAGE(__TYPEOF,"__typeof")
LANGUAGE(__TYPEOF__,"__typeof__")
LANGUAGE(__VOLATILE,"__volatile")
LANGUAGE(__VOLATILE__,"__volatile__")
LANGUAGE(__INT128,"__int128")

TEXT(IDENTIFIER,{identifier},true)
PREPROCESSOR_TEXT(INTEGERconstant,NUMBER,{integer_constant},false)
PREPROCESSOR_TEXT(OCTALconstant,NUMBER,{octal_constant},false)
PREPROCESSOR_TEXT(HEXconstant,NUMBER,{hex_constant},false)
PREPROCESSOR_TEXT(FLOATINGconstant,NUMBER,{floating_constant},false)
PREPROCESSOR_TEXT(PPNUM,NUMBER,{preprocessing_number},false)
/*
TEXT(INTEGERconstant,{integer_constant},false)
TEXT(OCTALconstant,{octal_constant},false)
TEXT(HEXconstant,{hex_constant},false)
TEXT(FLOATINGconstant,{floating_constant},false)
TEXT(PPNUM,{preprocessing_number},false)
*/

/* The \' doesn't mesh with the function-like invocation.  Returns an
error that there is an unmatched ' in the parameter-list.  Circumvent
the issue by putting the offending regular expression in a macro.  */

#define CHARACTERconstant_regex "L"?\'{c_char}+\'
#define STRINGliteral_regex "L"?\"{s_char}*\"

TEXT(CHARACTERconstant,CHARACTERconstant_regex,false)
TEXT(STRINGliteral,STRINGliteral_regex,false)

LANGUAGE(ARROW,"->")
LANGUAGE(ICR,"++")
LANGUAGE(DECR,"--")
LANGUAGE(LS,"<<")
LANGUAGE(RS,">>")
LANGUAGE(LE,"<=")
LANGUAGE(GE,">=")
LANGUAGE(EQ,"==")
LANGUAGE(NE,"!=")
LANGUAGE(ANDAND,"&&")
LANGUAGE(OROR,"||")
LANGUAGE(PLUSassign,"+=")
LANGUAGE(MINUSassign,"-=")
LANGUAGE(MULTassign,"*=")
LANGUAGE(DIVassign,"/=")
LANGUAGE(MODassign,"%=")
LANGUAGE(LSassign,"<<=")
LANGUAGE(RSassign,">>=")
LANGUAGE(ANDassign,"&=")
LANGUAGE(ERassign,"^=")
LANGUAGE(ORassign,"|=")

PREPROCESSOR(LPAREN,OPEN_PAREN,"(")
PREPROCESSOR(RPAREN,CLOSE_PAREN,")")
PREPROCESSOR(COMMA,COMMA,",")
PREPROCESSOR(HASH,HASH,"#")
PREPROCESSOR(DHASH,DOUBLE_HASH,"##")
PREPROCESSOR(ELLIPSIS,ELLIPSIS,"...")

LANGUAGE(LBRACE,"{")
LANGUAGE(RBRACE,"}")
LANGUAGE(LBRACK,"[")
LANGUAGE(RBRACK,"]")
LANGUAGE(DOT,".")
LANGUAGE(AND,"&")
LANGUAGE(STAR,"*")
LANGUAGE(PLUS,"+")
LANGUAGE(MINUS,"-")
LANGUAGE(NEGATE,"~")
LANGUAGE(NOT,"!")
LANGUAGE(DIV,"/")
LANGUAGE(MOD,"%")
LANGUAGE(LT,"<")
LANGUAGE(GT,">")
LANGUAGE(XOR,"^")
LANGUAGE(PIPE,"|")
LANGUAGE(QUESTION,"?")
LANGUAGE(COLON,":")
LANGUAGE(SEMICOLON,";")
LANGUAGE(ASSIGN,"=")

LANGUAGE(AT,"@")
LANGUAGE(USD,"$")
LANGUAGE(BACKSLASH, "\\")

// For c++
//LANGUAGE(DOTSTAR,".*")
//LANGUAGE(DCOLON,"::")
//LANGUAGE(ARROWSTAR,"->*")


}
// token definitions
"("        {  return  sym(Terminals.LPARA);          }
")"        {  return  sym(Terminals.RPARA);          }
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
"_"        {  return  sym(Terminals.WILDCARD);       }
{Numeral}  {  return  sym(Terminals.NUMERAL);        }
{VAR_ID}   {  return  sym(Terminals.VAR_ID);         }
{PRED_ID}  {  return  sym(Terminals.PRED_ID);        }
{GAP}      {  return  sym(Terminals.GAP);            }
{METAVAR_ID} {  return sym(Terminals.METAVARID);     }
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
