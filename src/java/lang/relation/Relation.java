package lang.relation;

import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import lang.ast.Constant;
import lang.ast.FormalPredicate;

public class Relation {
	private FormalPredicate formalPredicate = null;
	private Set<PseudoTuple> relation = new TreeSet<PseudoTuple>();
	private int arity;

	public Relation(FormalPredicate formalPredicate) {
		this.formalPredicate = formalPredicate;
		this.arity = formalPredicate.realArity();
	}

	public Relation(Relation r) {
		this.arity = r.arity;
		this.formalPredicate = r.formalPredicate;
		this.relation = new TreeSet<PseudoTuple>();
		this.relation.addAll(r.relation);
	}
	
	public Relation(int arity) {
		this.arity = arity;
	}
	
	public Set<PseudoTuple> tuples() {
		return relation;
	}
	
	/**
	 * Instantiate with all possible tuples of arity arity form objects objects. 
	 */
	public void allInstantiations(PseudoTuple ps, HashSet<Constant> objects) {
		System.out.println("UNIMPLEMENTED RELATIONS ALLINSTANTIATIONS");
		System.exit(0);
	}

	public int size() {
		return relation.size();
	}

	public int arity() {
		return arity;
	}

	public void addTuple(PseudoTuple pt) {
		relation.add(pt);
	}

	public void collectRelation(StringBuilder sb) {
		sb.append("[" + formalPredicate.predicateName() + "]{");
		relation.forEach(pt -> pt.collectTuple(sb));
		sb.append("}");
	}
	
	public boolean contains(PseudoTuple t) {
		return relation.contains(t);
	}
	
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof Relation)) return false;
		return relation.equals(((Relation) obj).relation);
	}
}
