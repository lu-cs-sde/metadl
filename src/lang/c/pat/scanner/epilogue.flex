<YYINITIAL> {
  "$"[a-z_][a-zA-Z0-9_]*            { return sym(Terminals.METAVARID); }
  ".."                              { return sym(Terminals.GAP); }
  /* error fallback */
  [^]           { throw new SyntaxError("Illegal character <"+yytext()+">"); }
  <<EOF>>       { return sym(Terminals.EOF); }
}
