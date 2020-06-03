/* 3.7 Comments */
<YYINITIAL> {
  {DocumentationComment} { return sym(Terminals.DOCUMENTATION_COMMENT); }
  {Comment} { }
}

