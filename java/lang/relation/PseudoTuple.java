package lang.relation;

import lang.ast.Constant;
import lang.ast.RealLiteral;
import lang.ast.Term;

public class PseudoTuple {
	public final int size;
	private Term[] tuple;

	public PseudoTuple(int size) {
		this.size = size;
		this.tuple = new Term[size];
	}

	public PseudoTuple(RealLiteral fact) {
		this.size = fact.getNumTerms();
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = fact.getTerms(i);
		}
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

	public void collectTuple(StringBuilder sb) {
		if(size == 0) return;
		if(size == 1) {
			sb.append("(").append(tuple[0].string()).append(")");
			return;
		}
		sb.append("(");
		sb.append(tuple[0].string());
		for (int i = 1; i != size; ++i) {
			sb.append(",").append(tuple[i].string());
		}
		sb.append(")");
	}
}
