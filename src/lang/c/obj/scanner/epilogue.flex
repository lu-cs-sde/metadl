<YYINITIAL> {
  [^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
}
