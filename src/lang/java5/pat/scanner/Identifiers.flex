/* 3.8 Identifiers
 * Located at end of current state due to rule priority disambiguation
 */
<YYINITIAL> {
  {MetaIdentifier} { return sym(Terminals.METAVARID); }
  {Identifier}  { return sym(Terminals.IDENTIFIER); }
}
