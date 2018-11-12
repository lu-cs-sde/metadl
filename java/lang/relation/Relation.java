package lang.relation;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import lang.ast.SuperPredicate;
import lang.ast.Term;

public class Relation {
	private SuperPredicate superPredicate;
	public final PseudoTupleComparator pseudoTupleComparator = new PseudoTupleComparator();
	private Set<PseudoTuple> relation = new TreeSet<PseudoTuple>(pseudoTupleComparator);
	private int arity;
	
	public Relation(SuperPredicate superPredicate, int arity) {
		this.superPredicate = superPredicate;
		this.arity = arity;
	}
	
	public Relation(Relation r) {
		this.arity = r.arity;
		this.superPredicate = r.superPredicate;
		this.relation = new TreeSet<PseudoTuple>(pseudoTupleComparator);
		this.relation.addAll(r.relation);
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
	
	public class PseudoTupleComparator implements Comparator<PseudoTuple> {
		@Override
		public int compare(PseudoTuple tup1, PseudoTuple tup2) {
			for(int i = 0; i != arity; ++i) {
				int res = Term.termComparator.compare(tup1.coord(i), tup2.coord(i));
				if(res != 0) return res;
			}
			return 0;
		}
	}
}
