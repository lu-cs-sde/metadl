package lang.relation;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import lang.ast.Constant;
import lang.ast.SuperPredicate;

public class Relation {
	private SuperPredicate superPredicate;
	private Set<PseudoTuple> relation = new TreeSet<PseudoTuple>();
	private int arity;

	public Relation(SuperPredicate superPredicate, int arity) {
		this.superPredicate = superPredicate;
		this.arity = arity;
	}

	public Relation(Relation r) {
		this.arity = r.arity;
		this.superPredicate = r.superPredicate;
		this.relation = new TreeSet<PseudoTuple>();
		this.relation.addAll(r.relation);
	}
	

	private void addAllHelper(int arity, int index, ArrayList<Constant> objects, Set<PseudoTuple> rel) {
		for(PseudoTuple ps : rel) {
//			ps.instantiate(index, c);
		}
	}
	
	/**
	 * Instantiate with all possible tuples of arity arity form objects objects. 
	 */
	public void allInstantiations(PseudoTuple ps, HashSet<Constant> objects) {
		
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
		sb.append("[" + superPredicate.predicateName() + "]{");
		relation.forEach(pt -> pt.collectTuple(sb));
		sb.append("}");
	}
	
	public boolean contains(PseudoTuple t) {
		return relation.contains(t);
	}
}
