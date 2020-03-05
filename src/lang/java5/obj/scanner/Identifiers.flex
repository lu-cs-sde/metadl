/* 3.8 Identifiers
 * Located at end of current state due to rule priority disambiguation
 */
<YYINITIAL> {
  ([:jletter:]|[\ud800-\udfff])([:jletterdigit:]|[\ud800-\udfff])* { return sym(Terminals.IDENTIFIER); }
}
