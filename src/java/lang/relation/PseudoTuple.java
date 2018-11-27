package lang.relation;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import lang.ast.Atom;
import lang.ast.BinaryExclusiveTermLiteral;
import lang.ast.Constant;
import lang.ast.EDBLiteral;
import lang.ast.Term;
import lang.ast.Variable;

public class PseudoTuple implements Comparable<PseudoTuple> {
	public final int size;
	private Term[] tuple;
	
	public PseudoTuple(TreeSet<Variable> vars) {
		this.size = vars.size();
		this.tuple = new Term[size];
		vars.toArray(this.tuple);
	}
	
	public PseudoTuple(Atom fact) {
		this.size = fact.getNumTerms();
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = fact.getTerms(i);
		}
	}
	
	public PseudoTuple(EDBLiteral edb) {
		this.size = 2;
		this.tuple = new Term[size];
		tuple[0] = edb.getPredicateRef();
		tuple[1] = edb.getFileLoc();
	}
	
	public PseudoTuple(BinaryExclusiveTermLiteral bin) {
		this.size = 2;
		this.tuple = new Term[size];
		tuple[0] = bin.getLeft();
		tuple[1] = bin.getRight();
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
			tuple[i] = new Variable("UNINITIALZIED VARIABLE");
		}
	}
	
	public static PseudoTuple of(Constant ... consts) {
		PseudoTuple pt = new PseudoTuple(consts.length);
		for(int i = 0; i != consts.length; ++i) {
			pt.instantiate(i, consts[i]);
		}
		return pt;
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
	
	public void instantiateAll(Set<Integer> positions, Constant c) {
		positions.forEach(i -> tuple[i] = c);
	}
	
	public PseudoTuple project(Set<Integer> positions) {
		PseudoTuple t = new PseudoTuple(positions.size());
		int i = 0;
		for(Integer j : positions) {
			t.tuple[i++] = tuple[j];
		}
		return t;
	}
	
	public boolean isUniInfo() {
		if(tuple.length <= 1) return true;
		Term first = tuple[0];
		for(int i = 1; i != tuple.length; ++i) {
			if(Term.termComparator.compare(first, tuple[i]) != 0) return false;
		}
		return true;
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

	public boolean isGround() {
		for (int i = 0; i != size; ++i)
			if (!instantiatedAt(i))
				return false;
		return true;
	}
	
	public Set<Integer> unboundPositions() {
		Set<Integer> unbound = new HashSet<Integer>();
		for (int i = 0; i != size; ++i) {
			if(!instantiatedAt(i)) unbound.add(i);
		}
		return unbound;
	}
	
	public TreeSet<Variable> freeVariables() {
		TreeSet<Variable> freeVars = new TreeSet<Variable>(Term.termComparator);
		for(int i = 0; i != size; ++i) {
			Term t = tuple[i];
			if(t.isVariable()) {
				freeVars.add((Variable)t);
			}
		}
		return freeVars;
	}
	
	@Override
	public int compareTo(PseudoTuple arg0) {
		for (int i = 0; i != size; ++i) {
			int res = Term.termComparator.compare(coord(i), arg0.coord(i));
			if (res != 0)
				return res;
		}
		return 0;
	}
	
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof PseudoTuple)) return false;
		return compareTo((PseudoTuple) obj) == 0;
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
