%{
  StringBuffer strbuf = new StringBuffer(128);
  int sub_line;
  int sub_column;

  /** String literal start position. */
  int stringStartLine, stringStartColumn;

  private Symbol sym(short id) {
    return new Symbol(id, yyline + 1, yycolumn + 1, len(), str());
  }

  private Symbol sym(short id, String value) {
    return new Symbol(id, yyline + 1, yycolumn + 1, len(), value);
  }

  private Symbol sym(short id, String value, int start_line, int start_column, int len) {
    return new Symbol(id, start_line, start_column, len, value);
  }

  private String str() { return yytext(); }
  private int len() { return yylength(); }

  private void error(String msg) throws Scanner.Exception {
    throw new Scanner.Exception(yyline + 1, yycolumn + 1, msg);
  }

  private void stringStart() {
    yybegin(STRING);
    stringStartLine = yyline + 1;
    stringStartColumn = yycolumn + 1;
    strbuf.setLength(0);
  }

  private Symbol stringEnd() {
    yybegin(YYINITIAL);
    String text = strbuf.toString();
    int length = text.length() + 2;
    return new Symbol(Terminals.STRING_LITERAL, stringStartLine, stringStartColumn, length, text);
  }

%}
