/* 3.8 Identifiers
 * Located at end of current state due to rule priority disambiguation
 */
<YYINITIAL> {
  `[:jletter:][:jletterdigit:]* { return new parser.MetaSymbol(Terminals.METAVARID, yyline + 1, yycolumn + 1, len(), str()); }
  ([:jletter:]|[\ud800-\udfff])([:jletterdigit:]|[\ud800-\udfff])* { return sym(Terminals.IDENTIFIER); }
}
