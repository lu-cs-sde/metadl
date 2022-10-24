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

  private StringBuilder comment;
  private boolean commentnl;

  static class Layout extends beaver.Symbol {
  	 public Layout(String text, boolean newline) {
	 }
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

"alignof" { return sym(Terminals.ALIGNOF); }
"auto"  { return sym(Terminals.AUTO); }
// "break"  { return sym(Terminals.BREAK); }
// "case"  { return sym(Terminals.CASE); }
"char"  { return sym(Terminals.CHAR); }
"const"  { return sym(Terminals.CONST); }
// "continue"  { return sym(Terminals.CONTINUE); }
// "default"  { return sym(Terminals.DEFAULT); }
// "do"  { return sym(Terminals.DO); }
"double"  { return sym(Terminals.DOUBLE); }
// "else"  { return sym(Terminals.ELSE); }
"enum"  { return sym(Terminals.ENUM); }
"extern"  { return sym(Terminals.EXTERN); }
"float"  { return sym(Terminals.FLOAT); }
// "for"  { return sym(Terminals.FOR); }
// "goto"  { return sym(Terminals.GOTO); }
// "if"  { return sym(Terminals.IF); }
"int"  { return sym(Terminals.INT); }
"long"  { return sym(Terminals.LONG); }
"register"  { return sym(Terminals.REGISTER); }
// "return"  { return sym(Terminals.RETURN); }
"short"  { return sym(Terminals.SHORT); }
"signed"  { return sym(Terminals.SIGNED); }
"sizeof"  { return sym(Terminals.SIZEOF); }
"static"  { return sym(Terminals.STATIC); }
"struct"  { return sym(Terminals.STRUCT); }
// "switch"  { return sym(Terminals.SWITCH); }
"typedef"  { return sym(Terminals.TYPEDEF); }
"union"  { return sym(Terminals.UNION); }
"unsigned"  { return sym(Terminals.UNSIGNED); }
"void"  { return sym(Terminals.VOID); }
"volatile"  { return sym(Terminals.VOLATILE); }
// "while"  { return sym(Terminals.WHILE); }

//C99 keywords used by the default gnu89 dialect
"_Bool"  { return sym(Terminals.U_BOOL); }
"_Complex"  { return sym(Terminals.U_COMPLEX); }
"inline"  { return sym(Terminals.INLINE); }

"restrict"  { return sym(Terminals.RESTRICT); }

//GCC
// "__alignof"  { return sym(Terminals.__ALIGNOF); }
// "__alignof__"  { return sym(Terminals.__ALIGNOF__); }
// "asm"  { return sym(Terminals.ASM); }
// "__asm"  { return sym(Terminals.__ASM); }
// "__asm__"  { return sym(Terminals.__ASM__); }
// "__attribute"  { return sym(Terminals.__ATTRIBUTE); }
// "__attribute__"  { return sym(Terminals.__ATTRIBUTE__); }
// "__builtin_offsetof"  { return sym(Terminals.__BUILTIN_OFFSETOF); }
// "__builtin_types_compatible_p"  { return sym(Terminals.__BUILTIN_TYPES_COMPATIBLE_P); }
// "__builtin_va_arg"  { return sym(Terminals.__BUILTIN_VA_ARG); }
// "__builtin_va_list"  { return sym(Terminals.__BUILTIN_VA_LIST); }
// "__complex__"  { return sym(Terminals.__COMPLEX__); }
// "__const"  { return sym(Terminals.__CONST); }
// "__const__"  { return sym(Terminals.__CONST__); }
// "__extension__"  { return sym(Terminals.__EXTENSION__); }
// "__inline"  { return sym(Terminals.__INLINE); }
// "__inline__"  { return sym(Terminals.__INLINE__); }
// "__label__"  { return sym(Terminals.__LABEL__); }
// "__restrict"  { return sym(Terminals.__RESTRICT); }
// "__restrict__"  { return sym(Terminals.__RESTRICT__); }
// "__signed"  { return sym(Terminals.__SIGNED); }
// "__signed__"  { return sym(Terminals.__SIGNED__); }
// "__thread"  { return sym(Terminals.__THREAD); }
// "typeof"  { return sym(Terminals.TYPEOF); }
// "__typeof"  { return sym(Terminals.__TYPEOF); }
// "__typeof__"  { return sym(Terminals.__TYPEOF__); }
// "__volatile"  { return sym(Terminals.__VOLATILE); }
// "__volatile__"  { return sym(Terminals.__VOLATILE__); }
// "__int128"  { return sym(Terminals.__INT128); }

//C11
"_Atomic" { return sym(Terminals.U_ATOMIC); }

{identifier} { return sym(Terminals.IDENTIFIER); }
{integer_constant} { return sym(Terminals.INTEGERConstant); }
// {octal_constant} { return sym(Terminals.OCTALConstant); }
// {hex_constant} { return sym(Terminals.HEXConstant); }
// {floating_constant} { return sym(Terminals.FLOATINGConstant); }
// {preprocessing_number} { return sym(Terminals.PPNUM); } // numeric constants supported only by the preprocessor?

// "L"?\'{c_char}+\' { return sym(Terminals.CHARACTERConstant); }
"L"?\"{s_char}*\" { return sym(Terminals.STRINGLiteral); }

"->"  { return sym(Terminals.ARROW); }
"++"  { return sym(Terminals.ICR); }
"--"  { return sym(Terminals.DECR); }
"<<"  { return sym(Terminals.LS); }
">>"  { return sym(Terminals.RS); }
"<="  { return sym(Terminals.LE); }
">="  { return sym(Terminals.GE); }
"=="  { return sym(Terminals.EQ); }
"!="  { return sym(Terminals.NE); }
"&&"  { return sym(Terminals.ANDAND); }
"||"  { return sym(Terminals.OROR); }
"+="  { return sym(Terminals.PLUSassign); }
"-="  { return sym(Terminals.MINUSassign); }
"*="  { return sym(Terminals.MULTassign); }
"/="  { return sym(Terminals.DIVassign); }
"%="  { return sym(Terminals.MODassign); }
"<<="  { return sym(Terminals.LSassign); }
">>="  { return sym(Terminals.RSassign); }
"&="  { return sym(Terminals.ANDassign); }
"^="  { return sym(Terminals.ERassign); }
"|="  { return sym(Terminals.ORassign); }

"("   { return sym(Terminals.LPAREN); } // preprocessor
")"   { return sym(Terminals.RPAREN); } // preprocessor
","   { return sym(Terminals.COMMA); } // preprocessor
// "#"   { return sym(Terminals.HASH); } // preprocessor
// "##"  { return sym(Terminals.DHASH); } // preprocessor
"..." { return sym(Terminals.ELLIPSIS); } // preprocessor

"{"  { return sym(Terminals.LBRACE); }
"}"  { return sym(Terminals.RBRACE); }
"["  { return sym(Terminals.LBRACK); }
"]"  { return sym(Terminals.RBRACK); }
"."  { return sym(Terminals.DOT); }
"&"  { return sym(Terminals.AND); }
"*"  { return sym(Terminals.STAR); }
"+"  { return sym(Terminals.PLUS); }
"-"  { return sym(Terminals.MINUS); }
"~"  { return sym(Terminals.NEGATE); }
"!"  { return sym(Terminals.NOT); }
"/"  { return sym(Terminals.DIV); }
"%"  { return sym(Terminals.MOD); }
"<"  { return sym(Terminals.LT); }
">"  { return sym(Terminals.GT); }
"^"  { return sym(Terminals.XOR); }
"|"  { return sym(Terminals.PIPE); }
"?"  { return sym(Terminals.QUESTION); }
":"  { return sym(Terminals.COLON); }
";"  { return sym(Terminals.SEMICOLON); }
"="  { return sym(Terminals.ASSIGN); }

// "@"  { return sym(Terminals.AT); }
// "$"  { return sym(Terminals.USD); }
//  "\\"  { return sym(Terminals.BACKSLASH); }

// For c++
//".*"  { return sym(Terminals.DOTSTAR); }
//"::"  { return sym(Terminals.DCOLON); }
//"->*"  { return sym(Terminals.ARROWSTAR); }

"/*" {
yybegin(COMMENT);
comment = new StringBuilder();
comment.append(yytext());
commentnl = false;
}

/* error fallback */
[^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
}

<COMMENT> {
"*/" {
comment.append(yytext()); yybegin(YYINITIAL);
return new Layout(comment.toString(), false);
}
[^*\n]+ {comment.append(yytext());}
"*" {comment.append(yytext());}
\n {comment.append(yytext()); commentnl = true;}
}
