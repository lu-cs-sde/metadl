package lang.relation;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import lang.ast.Constant;
import lang.ast.RealLiteral;
import lang.ast.SuperPredicate;
import lang.ast.Term;
import lang.ast.Variable;

public class PseudoTuple implements Comparable<PseudoTuple> {
	public final int size;
	private Term[] tuple;
	
	public final SuperPredicate sp;
	
	public PseudoTuple(TreeSet<Variable> vars) {
		this.size = vars.size();
		this.sp = null;
		this.tuple = new Term[size];
		vars.toArray(this.tuple);
	}
	
	public PseudoTuple(SuperPredicate sp) {
		this.size = sp.realArity();
		this.tuple = new Term[size];
		this.sp = sp;
	}

	public PseudoTuple(RealLiteral fact) {
		this.size = fact.getNumTerms();
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = fact.getTerms(i);
		}
		this.sp = fact.getPredicate().superpredicate();
	}
	
	public PseudoTuple(PseudoTuple o) {
		this.size = o.size;
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = o.tuple[i];
		}
		this.sp = o.sp;
	}
	
	public PseudoTuple(SuperPredicate sp, int size) {
		this.size = size;
		this.tuple = new Term[size];
		for (int i = 0; i != size; ++i) {
			tuple[i] = new Variable("UNINITIALZIED VARIABLE");
		}
		this.sp = sp;
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

	public void collectTuple(StringBuilder sb) {
		if (size == 0)
			return;
		if (size == 1) {
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

	public boolean isGround() {
		for (int i = 0; i != size; ++i)
			if (!instantiatedAt(i))
				return false;
		return true;
	}
	
	public boolean instantiableAs(PseudoTuple pt) {
		if(size != pt.size) return false;
		TreeMap<Variable, Constant> binding = new TreeMap<>(Term.termComparator);
		for(int i = 0; i != size; ++i) {
			Term t1 = tuple[i];
			Term t2 = pt.tuple[i];
			
			if(t1.isVariable()) {
				Constant c = binding.get((Variable)t1);
				if(c == null) 
					binding.put((Variable)t1, (Constant)t2);
				else if(!c.equals(t2)) 
					return false;
			} else if(!((Constant)t1).equals(t2)) return false;
		}
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
	
	public TreeMap<Variable, Constant> createInstantiationFrom(PseudoTuple pt) {
		assertTrue(pt.isGround());
		assertTrue(instantiableAs(pt));
		TreeMap<Variable, Constant> inst = new TreeMap<Variable, Constant>(Term.termComparator);
		for(int i = 0; i != size; ++i) {
			Term t1 = tuple[i];
			Term t2 = pt.tuple[i];
			if(t1.isVariable()) {
				inst.put((Variable)t1, (Constant)t2);
			}
		}
		return inst;
	}
	
	public void instantiateWith(TreeMap<Variable, Constant> inst) {
		for(int i = 0; i != size; ++i) {
			Term t = tuple[i];
			if(t.isVariable()) {
				Constant c = inst.get((Variable) t);
				if(c != null) {
					instantiate(i, c);
				}
			}
		}
	}
	
	public int compareTuples(PseudoTuple arg0) {
		for (int i = 0; i != size; ++i) {
			int res = Term.termComparator.compare(coord(i), arg0.coord(i));
			if (res != 0)
				return res;
		}
		return 0;
	}

	@Override
	public int compareTo(PseudoTuple arg0) {
		if(sp == null) return compareTuples(arg0);
		int pred_comp = sp.predicateName().compareTo(arg0.sp.predicateName());
		if(pred_comp != 0) return pred_comp;
		return compareTuples(arg0);
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
			strs[i] = tuple[i].string();
		}
		return strs;
	}
}
