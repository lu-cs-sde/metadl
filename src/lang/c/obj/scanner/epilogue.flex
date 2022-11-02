<YYINITIAL> {
  [^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
  <<EOF>>       { return sym(Terminals.EOF); }
}
