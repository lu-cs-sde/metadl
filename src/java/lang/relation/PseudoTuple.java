package lang.relation;

import lang.ast.Constant;
import lang.ast.Term;

public class PseudoTuple {
	public final int size;
	private Term[] tuple;
	

	public PseudoTuple(int size) {
		this.size = size;
		this.tuple = new Term[size];
	}
	
	public boolean instantiatedAt(int i) {
		return !tuple[i].isVariable();
	}
	
	public Term coord(int i) {
		return tuple[i];
	}
	
	public void instantiate(int i, Constant c) {
		tuple[i] = c;
	}
}
