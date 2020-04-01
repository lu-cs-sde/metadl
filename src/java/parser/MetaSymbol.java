package parser;

public class MetaSymbol extends beaver.Symbol {
	public MetaSymbol(short id, Object value) {
		super(id, value);
	}

	public MetaSymbol(short id, int start_line, int start_column, int length, Object value) {
		super(id, start_line, start_column, length, value);
	}
}
