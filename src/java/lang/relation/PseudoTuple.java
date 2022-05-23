package lang.relation;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.ArrayList;

import lang.ast.Constant;
import lang.ast.List;
import lang.ast.Term;
import lang.ast.Variable;

public class PseudoTuple {
	private int size;
	private Term[] tuple;
	public static final Variable uninitializedVar = new Variable("UNINITIALZIED VARIABLE");

	public PseudoTuple(List<Term> terms) {
		this.size = terms.getNumChild();
		this.tuple = new Term[size];
		for(int i = 0; i != size; ++i) {
			tuple[i] = terms.getChild(i);
		}
	}

	public PseudoTuple(java.util.List<Term> terms) {
		this.size = terms.size();
		this.tuple = new Term[size];
		for(int i = 0; i != size; ++i) {
			tuple[i] = terms.get(i);
		}
	}

	public PseudoTuple(Term ... terms) {
		this.size = terms.length;
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = terms[i];
		}
	}

	public PseudoTuple(PseudoTuple o) {
		this.size = o.size;
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = o.tuple[i];
		}
	}

	public PseudoTuple(int size) {
		this.size = size;
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = uninitializedVar;
		}
	}

	public static PseudoTuple of(Term ... consts) {
		PseudoTuple pt = new PseudoTuple(consts.length);
		for(int i = 0; i != consts.length; ++i) {
			pt.set(i, consts[i]);
		}
		return pt;
	}

	public java.util.List<Term> toList() {
		java.util.List<Term> list = new ArrayList<Term>();
		for(int i = 0; i != size; ++i)
			list.add(tuple[i]);
		return list;
	}

	public void expand() {
		Term[] tmp = new Term[size];
		System.arraycopy(tuple, 0, tmp, 0, size);
		tuple = new Term[size + 1];
		System.arraycopy(tmp, 0, tuple, 0, size);
		tuple[size] = uninitializedVar;
		size = size + 1;
	}

	public int arity() {
		return size;
	}

	public boolean instantiatedAt(int i) {
		return !tuple[i].isVariable();
	}

	public Term coord(int i) {
		return tuple[i];
	}

	public PseudoTuple instantiate(int i, Constant c) {
		tuple[i] = c;
		return this;
	}

	public PseudoTuple set(int i, Term t) {
		tuple[i] = t;
		return this;
	}

	public void collectTuple(StringBuilder sb) {
		if (size == 0)
			return;
		if (size == 1) {
			sb.append("(").append(tuple[0].toString()).append(")");
			return;
		}
		sb.append("(");
		sb.append(tuple[0].toString());
		for (int i = 1; i != size; ++i) {
			sb.append(",").append(tuple[i].toString());
		}
		sb.append(")");
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		collectTuple(sb);
		return sb.toString();
	}

	public String[] toStringArray() {
		String[] strs = new String[size];
		for(int i = 0; i != size; ++i) {
			strs[i] = tuple[i].toString();
		}
		return strs;
	}
}
