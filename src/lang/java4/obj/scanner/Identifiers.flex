/* 3.8 Identifiers
 * Located at end of current state due to rule priority disambiguation
 */
<YYINITIAL> {
  {Identifier}  { return sym(Terminals.IDENTIFIER); }
}
